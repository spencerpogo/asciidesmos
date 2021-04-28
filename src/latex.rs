#[derive(Clone, Debug, PartialEq)]
pub enum Latex {
    Builtin(String),
    Variable(String),
    Num(String),
    Call {
        func: Box<Latex>,
        args: Vec<Latex>,
    },
    BinaryExpression {
        left: Box<Latex>,
        operator: String, // TODO: Replace this with an enum
        right: Box<Latex>,
    },
    UnaryExpression {
        left: Box<Latex>,
        operator: String, // TODO: Replace this with an enum
    },
    List(Vec<Latex>),
    Assignment(Box<Latex>, Box<Latex>),
    FuncDef {
        name: String,
        args: Vec<String>,
        body: Box<Latex>,
    },
}

pub fn format_latex_identifier(v: String) -> String {
    // Don't care about UTF-8 since identifiers are guaranteed to be ASCII
    let mut chars = v.chars();

    match chars.next() {
        Some(c) => {
            let rest: String = chars.collect();
            if rest.is_empty() {
                c.to_string()
            } else {
                format!("{}_{{{}}}", c, rest)
            }
        }
        None => "".to_string(),
    }
}

pub fn multi_latex_to_str(items: Vec<Latex>) -> Vec<String> {
    items.into_iter().map(latex_to_str).collect()
}

pub fn latex_to_str(l: Latex) -> String {
    match l {
        Latex::Builtin(s) => format!("\\{}", s),
        Latex::Variable(s) => format_latex_identifier(s),
        Latex::Num(s) => s.to_string(),
        Latex::Call { func, args } => format!(
            "{}\\left({}\\right)",
            latex_to_str(*func),
            multi_latex_to_str(args).join(",")
        ),
        Latex::BinaryExpression {
            left,
            operator,
            right,
        } => format!(
            "{}{}{}",
            latex_to_str(*left),
            operator,
            latex_to_str(*right)
        ),
        Latex::UnaryExpression { left, operator } => format!("{}{}", latex_to_str(*left), operator),
        Latex::List(items) => multi_latex_to_str(items).join(","),
        Latex::Assignment(left, right) => {
            format!("{}={}", latex_to_str(*left), latex_to_str(*right))
        }
        Latex::FuncDef { name, args, body } => format!(
            "{}\\left({}\\right)={}",
            name,
            args.join(","),
            latex_to_str(*body)
        ),
    }
}
