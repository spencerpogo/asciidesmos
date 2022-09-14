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
            vec![]
        }
        ImportMode::Include => out,
    })
}

#[cfg(test)]
mod tests {
    use crate::compiler::tests::*;

    #[test]
    fn stdlib_import() {
        let i = ast::Import {
            path: "test".to_owned(),
            mode: ast::ImportMode::Include,
        };
        check_stmt(
            ast::Statement::Import(i.clone()),
            latex::LatexStatement::Assignment(
                Box::new(latex::Latex::Variable("test_var".to_owned())),
                Box::new(latex::Latex::Num("1".to_owned())),
            ),
        );
        assert_eq!(
            compile_stmt(ast::Statement::Import(ast::Import {
                mode: ast::ImportMode::Import {
                    name: "test".to_owned()
                },
                ..i
            })),
            Ok(vec![])
        );
    }
}
