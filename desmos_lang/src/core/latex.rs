#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Factorial,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Latex {
    Variable(String),
    Num(String),
    Call {
        func: String,
        is_builtin: bool,
        args: Vec<Latex>,
    },
    BinaryExpression {
        left: Box<Latex>,
        operator: BinaryOperator,
        right: Box<Latex>,
    },
    UnaryExpression {
        left: Box<Latex>,
        operator: UnaryOperator,
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

pub fn binaryoperator_to_str(left: Latex, operator: BinaryOperator, right: Latex) -> String {
    let ls = latex_to_str(left.clone());
    let rs = latex_to_str(right.clone());
    match operator {
        BinaryOperator::Add => format!("{}+{}", ls, rs),
        BinaryOperator::Subtract => format!("{}-{}", ls, rs),
        BinaryOperator::Multiply => match (left, right) {
            (Latex::Num(_), Latex::Num(_)) => format!("{}\\cdot {}", ls, rs),
            _ => format!("{}{}", ls, rs),
        },
        BinaryOperator::Divide => format!("\\frac{{{}}}{{{}}}", ls, rs),
    }
}

pub fn latex_to_str(l: Latex) -> String {
    match l {
        Latex::Variable(s) => format_latex_identifier(s),
        Latex::Num(s) => s.to_string(),
        Latex::Call {
            func,
            is_builtin,
            args,
        } => format!(
            "{}{}\\left({}\\right)",
            if is_builtin { "\\" } else { "" },
            func,
            multi_latex_to_str(args).join(",")
        ),
        Latex::BinaryExpression {
            left,
            operator,
            right,
        } => binaryoperator_to_str(*left, operator, *right),
        Latex::UnaryExpression { left, operator } => match operator {
            UnaryOperator::Factorial => format!("{}!", latex_to_str(*left),),
        },

        Latex::List(items) => multi_latex_to_str(items).join(","),
        Latex::Assignment(left, right) => {
            format!("{}={}", latex_to_str(*left), latex_to_str(*right))
        }
        Latex::FuncDef { name, args, body } => format!(
            "{}\\left({}\\right)={}",
            name,
            args.into_iter()
                .map(format_latex_identifier)
                .collect::<Vec<String>>()
                .join(","),
            latex_to_str(*body)
        ),
    }
}
