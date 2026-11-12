//! JSON's oracles: JSONTestSuite's verdicts (at its CON-2 pin) and serde_json
//! as a typed-value comparator.

use super::{Judgement, Oracle, verdict_judgement};
use crate::arm::{Case, Outcome};
use crate::registry::Grammar;

/// `y_` must be accepted, `n_` rejected; `i_` is implementation-defined (the
/// outcome is recorded, never judged).
pub struct JsonTestSuite;

impl Oracle for JsonTestSuite {
    fn name(&self) -> &'static str {
        "jsontestsuite"
    }

    fn applies(&self, g: &Grammar) -> bool {
        g.id == "json"
    }

    fn expected_verdict(&self, _g: &Grammar, _i: usize, case: &Case) -> Option<bool> {
        if !case.id.starts_with("jsontestsuite/") {
            return None;
        }
        match case.file_name().as_bytes().first() {
            Some(b'y') => Some(true),
            Some(b'n') => Some(false),
            _ => None,
        }
    }

    fn judge(&self, _g: &Grammar, _i: usize, case: &Case, out: &Outcome) -> Judgement {
        if !case.id.starts_with("jsontestsuite/") {
            return Judgement::NotApplicable;
        }
        match case.file_name().as_bytes().first() {
            Some(b'y') => verdict_judgement(true, out),
            Some(b'n') => verdict_judgement(false, out),
            _ => Judgement::NotApplicable,
        }
    }
}

/// Where serde_json accepts a file and the arm accepts it with a typed value,
/// the two values are equal in the JSON data model (numbers by value).
pub struct SerdeJson;

pub fn json_equal(a: &serde_json::Value, b: &serde_json::Value) -> bool {
    use serde_json::Value as V;
    match (a, b) {
        (V::Null, V::Null) => true,
        (V::Bool(x), V::Bool(y)) => x == y,
        (V::String(x), V::String(y)) => x == y,
        (V::Number(x), V::Number(y)) => {
            if let (Some(p), Some(q)) = (x.as_i64(), y.as_i64()) {
                return p == q;
            }
            if let (Some(p), Some(q)) = (x.as_u64(), y.as_u64()) {
                return p == q;
            }
            match (x.as_f64(), y.as_f64()) {
                (Some(p), Some(q)) => p == q || (p.is_nan() && q.is_nan()),
                _ => false,
            }
        }
        (V::Array(x), V::Array(y)) => {
            x.len() == y.len() && x.iter().zip(y).all(|(p, q)| json_equal(p, q))
        }
        (V::Object(x), V::Object(y)) => {
            x.len() == y.len()
                && x.iter()
                    .all(|(k, v)| y.get(k).is_some_and(|w| json_equal(v, w)))
        }
        _ => false,
    }
}

/// The first place two JSON values differ, as `path: ours vs theirs`.
pub fn json_diff(a: &serde_json::Value, b: &serde_json::Value, at: &str) -> Option<String> {
    use serde_json::Value as V;
    match (a, b) {
        (V::Array(x), V::Array(y)) if x.len() == y.len() => x
            .iter()
            .zip(y)
            .enumerate()
            .find_map(|(i, (p, q))| json_diff(p, q, &format!("{at}[{i}]"))),
        (V::Object(x), V::Object(y)) if x.len() == y.len() => {
            x.iter().find_map(|(k, v)| match y.get(k) {
                Some(w) => json_diff(v, w, &format!("{at}.{k}")),
                None => Some(format!("{at}.{k}: key absent from serde_json's value")),
            })
        }
        _ if json_equal(a, b) => None,
        _ => {
            let short = |v: &serde_json::Value| {
                let s = v.to_string();
                if s.len() > 60 {
                    format!("{}…", &s[..60])
                } else {
                    s
                }
            };
            Some(format!("{at}: arm {} vs serde_json {}", short(a), short(b)))
        }
    }
}

impl Oracle for SerdeJson {
    fn name(&self) -> &'static str {
        "serde_json"
    }

    fn applies(&self, g: &Grammar) -> bool {
        g.id == "json"
    }

    fn judge(&self, _g: &Grammar, _i: usize, case: &Case, out: &Outcome) -> Judgement {
        let Outcome::Accept { value: Some(v), .. } = out else {
            return Judgement::NotApplicable;
        };
        let Ok(want) = serde_json::from_slice::<serde_json::Value>(&case.bytes()) else {
            return Judgement::NotApplicable;
        };
        if json_equal(v, &want) {
            Judgement::Agree
        } else {
            Judgement::Diverge(format!(
                "value differs from serde_json's at {}",
                json_diff(v, &want, "$").unwrap_or_else(|| "?".into())
            ))
        }
    }
}
