use std::collections::HashMap;

use serde::{Deserialize, Serialize};

pub const LATEST_STATE_VERSION: i8 = 9;

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

    #[serde(skip_serializing_if = "Option::is_none")]
    pub ticker: Option<Ticker>,
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
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum FormulaExpressionType {
    XOrY,
    SinglePoint,
    PointList,
    Parametric,
    Polar,
    Implicit,
    Polygon,
    Histogram,
    Dotplot,
    Boxplot,
    Ttest,
    Stats,
    Cube,
    Sphere,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")] // wtf
pub struct Formula {
    expression_type: FormulaExpressionType,
    is_graphable: bool,
    is_inequality: bool,
    action_value: HashMap<String, String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ItemModel {
    id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    folder_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    secret: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    formula: Option<Formula>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum LineStyle {
    Solid,
    Dashed,
    Dotted,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BasicSetExpression {
    id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    latex: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    color: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    line_style: Option<LineStyle>,
    // TODO: This can also be a number
    #[serde(skip_serializing_if = "Option::is_none")]
    line_width: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    line_opacity: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "camelCase")]
pub enum ExpressionValue {
    Expression {
        #[serde(flatten)]
        item_model: ItemModel,

        #[serde(skip_serializing_if = "Option::is_none")]
        fill: Option<bool>,

        #[serde(skip_serializing_if = "Option::is_none")]
        secret: Option<bool>,

        #[serde(skip_serializing_if = "Option::is_none")]
        slider_bounds: Option<SliderBounds>,

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
pub struct SliderBounds {
    min: String,
    max: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    step: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Column {}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Ticker {
    #[serde(skip_serializing_if = "Option::is_none")]
    handler_latex: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    min_step_latex: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    open: Option<bool>,

    #[serde(skip_serializing_if = "Option::is_none")]
    playing: Option<bool>,
}

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
            expressions: Expressions {
                list: vec![],
                ticker: None,
            },
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
            ticker: None,
        }
    }
}
