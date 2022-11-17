use ast::ImportMode;

use crate::{
    compile_stmts,
    error::{CompileError, CompileErrorKind},
    Context,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PathType {
    Local,
    NonLocal,
}

pub fn is_local_path(path: &str) -> bool {
    path.starts_with("./") || path.starts_with("../")
}

pub fn path_type(path: &str) -> PathType {
    match is_local_path(path) {
        true => PathType::Local,
        false => PathType::NonLocal,
    }
}

pub fn handle_import(
    ctx: &mut Context,
    span: types::Span,
    import: ast::Import,
) -> Result<Vec<latex::LatexStatement>, CompileError> {
    let maybe_ast = match path_type(import.path.as_str()) {
        PathType::NonLocal => ctx
            .stdlib
            .load_lib(ctx.loader.clone(), import.path.as_str()),
        PathType::Local => ctx.loader.load(import.path.as_str()),
    };
    let ast = match maybe_ast {
        Some(ast) => ast,
        None => {
            return Err(CompileError {
                kind: CompileErrorKind::ModuleNotFound(import.path),
                span,
            })
        }
    };
    let mut mod_ctx = Context::new_with_loader(ctx.loader.clone());
    let out = compile_stmts(&mut mod_ctx, ast)?;
    Ok(match import.mode {
        ImportMode::Import { name } => {
            ctx.modules.insert(name, mod_ctx);
            out
        }
        ImportMode::Include => out,
    })
}

#[cfg(test)]
mod tests {
    use crate::compiler::tests::*;

    #[test]
    fn stdlib_import() {
        #[derive(Copy, Clone, Debug)]
        struct TestLoader;
        impl crate::Loader for TestLoader {
            fn load(&self, _path: &str) -> Option<ast::LStatements> {
                return None;
            }

            fn parse_source(&self, _source: &str) -> Option<ast::LStatements> {
                return Some(vec![
                    (
                        spn(),
                        ast::Statement::VarDef {
                            name: "test_var".to_string(),
                            val: (spn(), ast::Expression::Num("1".to_string())),
                            inline: false,
                        },
                    ),
                    (
                        spn(),
                        ast::Statement::VarDef {
                            name: "test".to_string(),
                            val: (spn(), ast::Expression::Num("2".to_string())),
                            inline: true,
                        },
                    ),
                ]);
            }
        }

        let i = ast::Import {
            path: "test".to_owned(),
            mode: ast::ImportMode::Include,
        };
        assert_eq!(
            compile_stmt_with_ctx(
                &mut crate::Context::new_with_loader(Box::new(TestLoader)),
                ast::Statement::Import(i.clone())
            ),
            Ok(vec![latex::LatexStatement::Assignment(
                Box::new(latex::Latex::Variable("test_var".to_owned())),
                Box::new(latex::Latex::Num("1".to_owned())),
            )]),
        );
        assert_eq!(
            compile_stmt_with_ctx(
                &mut crate::Context::new_with_loader(Box::new(TestLoader)),
                ast::Statement::Import(ast::Import {
                    mode: ast::ImportMode::Import {
                        name: "test".to_owned()
                    },
                    ..i
                })
            ),
            Ok(vec![latex::LatexStatement::Assignment(
                Box::new(latex::Latex::Variable("test_var".to_owned())),
                Box::new(latex::Latex::Num("1".to_owned())),
            )])
        );
    }
}
