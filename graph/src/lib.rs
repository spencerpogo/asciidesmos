use serde::{Deserialize, Serialize};

const LATEST_STATE_VERSION: i8 = 9;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CalcState {
    // For our purposes this should always be LATEST_STATE_VERSION
    pub version: i8,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub graph: Option<Graph>,

    // A 32 character hex string
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub random_seed: Option<String>,

    pub expressions: Expressions,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Graph {
    pub viewport: Viewport,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Viewport {
    pub xmin: f64,
    pub xmax: f64,

    pub ymin: f64,
    pub ymax: f64,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Expressions {
    pub list: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Expression {
    // Must be unique as it is used for a react-style key prop. Usually a number.
    //  Should be a valid property name for a javascript object (letters, numbers, and _).
    pub id: String,

    #[serde(flatten)]
    pub value: ExpressionValue,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "camelCase")]
pub enum ExpressionValue {
    Expression {
        // Optional CSS color
        #[serde(skip_serializing_if = "Option::is_none")]
        #[serde(default)]
        color: Option<String>,
        // Optional content
        #[serde(skip_serializing_if = "Option::is_none")]
        #[serde(default)]
        latex: Option<String>,
    },
    Table {
        columns: Vec<Column>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Column {}

impl std::default::Default for CalcState {
    fn default() -> Self {
        Self {
            version: LATEST_STATE_VERSION,
            graph: Some(Graph {
                viewport: Viewport {
                    xmin: -10.0,
                    xmax: 10.0,
                    ymin: -10.0,
                    ymax: 10.0,
                },
            }),
            random_seed: None,
            expressions: Expressions { list: vec![] },
        }
    }
}

impl Expressions {
    pub fn from_latex_strings(latex_strings: Vec<String>) -> Self {
        Self {
            list: latex_strings
                .into_iter()
                .enumerate()
                .map(|(i, l)| Expression {
                    id: i.to_string(),
                    value: ExpressionValue::Expression {
                        color: None,
                        latex: Some(l),
                    },
                })
                .collect(),
        }
    }
}
