pub enum Latex<'a> {
    Builtin(&'a str),
    Variable(&'a str),
    Num(&'a str),
    Call {
        func: Box<Latex<'a>>,
        args: Vec<Latex<'a>>,
    },
    List(Vec<Latex<'a>>),
    Assignment(Box<Latex<'a>>, Box<Latex<'a>>),
}

pub fn format_latex_identifier(v: &str) -> String {
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
        Latex::List(items) => multi_latex_to_str(items).join(","),
        Latex::Assignment(left, right) => {
            format!("{}={}", latex_to_str(*left), latex_to_str(*right))
        }
    }
}
