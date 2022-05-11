#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Factorial,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CompareOperator {
    Equal,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Cond {
    pub left: Latex,
    pub op: CompareOperator,
    pub right: Latex,
    pub result: Latex,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Function {
    Normal { name: String },
    Log { base: String },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Latex {
    Variable(String),
    Num(String),
    Call {
        func: Function,
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
    Piecewise {
        first: Box<Cond>,
        rest: Vec<Cond>,
        default: Box<Latex>,
    },
}

pub fn function_to_str(function: Function) -> String {
    match function {
        Function::Normal { name } => name,
        Function::Log { base } => {
            if base.is_empty() {
                "log".to_string()
            } else {
                format!("log_{{{}}}", base)
            }
        }
    }
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

pub fn compareop_to_str(op: CompareOperator) -> &'static str {
    match op {
        CompareOperator::Equal => "=",
        CompareOperator::GreaterThan => ">", // or \gt
        CompareOperator::LessThan => "<",    // or \lt
        CompareOperator::GreaterThanEqual => "\\le",
        CompareOperator::LessThanEqual => "\\ge",
    }
}

pub fn cond_to_str(cond: Cond) -> String {
    format!(
        "{}{}{}:{}",
        latex_to_str(cond.left),
        compareop_to_str(cond.op),
        latex_to_str(cond.right),
        latex_to_str(cond.result)
    )
}

fn _latex_call_to_str(func: Function, is_builtin: bool, args: Vec<Latex>) -> String {
    format!(
        "{}{}\\left({}\\right)",
        if is_builtin { "\\" } else { "" },
        function_to_str(func),
        multi_latex_to_str(args).join(",")
    )
}

fn latex_call_to_str(func: Function, is_builtin: bool, args: Vec<Latex>) -> String {
    match &func {
        Function::Normal { name } => {
            if is_builtin && (name == "sqrt" || name == "nthroot") {
                // there should only be one arg in case of sqrt, two in case of nthroot
                format!("\\sqrt{{{}}}", multi_latex_to_str(args).join("}{"))
            } else {
                _latex_call_to_str(func, is_builtin, args)
            }
        }
        _ => _latex_call_to_str(func, is_builtin, args),
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
        } => latex_call_to_str(func, is_builtin, args),
        Latex::BinaryExpression {
            left,
            operator,
            right,
        } => binaryoperator_to_str(*left, operator, *right),
        Latex::UnaryExpression { left, operator } => match operator {
            UnaryOperator::Negate => format!("-{}", latex_to_str(*left)),
            UnaryOperator::Factorial => format!("{}!", latex_to_str(*left),),
        },
        Latex::List(items) => format!("\\left[{}\\right]", multi_latex_to_str(items).join(",")),
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
        Latex::Piecewise {
            first,
            rest,
            default,
        } => format!(
            "\\left\\{{{},{}{}\\right\\}}",
            cond_to_str(*first),
            rest.into_iter()
                .map(|cond| cond_to_str(cond) + ",")
                .collect::<String>(),
            latex_to_str(*default)
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: Latex, output: &'static str) {
        assert_eq!(latex_to_str(input), output.to_string());
    }

    #[test]
    fn piecewise_single() {
        check(
            Latex::Piecewise {
                first: Box::new(Cond {
                    left: Latex::Num("1".to_string()),
                    op: CompareOperator::Equal,
                    right: Latex::Num("2".to_string()),
                    result: Latex::Num("3".to_string()),
                }),
                rest: vec![],
                default: Box::new(Latex::Num("4".to_string())),
            },
            "\\left\\{1=2:3,4\\right\\}",
        )
    }

    #[test]
    fn piecewise_multi() {
        check(
            Latex::Piecewise {
                first: Box::new(Cond {
                    left: Latex::Num("1".to_string()),
                    op: CompareOperator::Equal,
                    right: Latex::Num("2".to_string()),
                    result: Latex::Num("3".to_string()),
                }),
                rest: vec![Cond {
                    left: Latex::Num("4".to_string()),
                    op: CompareOperator::LessThan,
                    right: Latex::Num("5".to_string()),
                    result: Latex::Num("6".to_string()),
                }],
                default: Box::new(Latex::Num("7".to_string())),
            },
            "\\left\\{1=2:3,4<5:6,7\\right\\}",
        )
    }

    #[test]
    fn log() {
        check(
            Latex::Call {
                func: Function::Log {
                    base: "".to_string(),
                },
                args: vec![Latex::Num("10".to_string())],
                is_builtin: true,
            },
            "\\log\\left(10\\right)",
        );
    }

    #[test]
    fn log_base() {
        check(
            Latex::Call {
                func: Function::Log {
                    base: "5".to_string(),
                },
                args: vec![Latex::Num("25".to_string())],
                is_builtin: true,
            },
            "\\log_{5}\\left(25\\right)",
        );
    }
}
