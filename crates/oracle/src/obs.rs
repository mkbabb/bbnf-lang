//! `obs()`: the declared observation function, and the failure contract
//! (sk-v27 LW3.COMP-4a; compiler design C1-8, ruling R-b).
//!
//! Two parses are compared only through `obs(class, tree)`. A class is a
//! property of a rewrite or of an arm pair, never of a grammar: there are
//! exactly the four classes of [`ObsClass`], and each one is a projection
//! (`obs(c, obs(c, t)) == obs(c, t)`). `docs/bbnf/observation.md` holds the
//! sentences that define them; this module is their executable form.
//!
//! A rejected input is compared through [`FailureContract`]: the furthest
//! offset reached and the set of expected labels at that offset, sorted and
//! deduplicated. A contract whose expected set is not strictly increasing
//! cannot be constructed.

use std::fmt;

use serde::{Deserialize, Serialize};

/// A half-open byte range `[start, end)` into the input.
pub type Span = (u32, u32);

/// The tree an arm reports for an accepted input.
///
/// Every arm lowers its own result to this one shape: a `Node` is a
/// grouping (a rule's result, a sequence, a repetition), a `Leaf` is a
/// scalar the input spelled. `label` is the arm's name for the node (a rule
/// name or a typed kind) and is `None` for anonymous groupings; `span` is
/// `None` when the arm does not know where the node came from.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "t", rename_all = "lowercase")]
pub enum Obs {
    Node {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        label: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        span: Option<Span>,
        #[serde(default)]
        children: Vec<Obs>,
    },
    Leaf {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        label: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        span: Option<Span>,
        text: String,
    },
}

impl Obs {
    pub fn leaf(label: Option<&str>, span: Option<Span>, text: impl Into<String>) -> Self {
        Obs::Leaf { label: label.map(str::to_owned), span, text: text.into() }
    }

    pub fn node(label: Option<&str>, span: Option<Span>, children: Vec<Obs>) -> Self {
        Obs::Node { label: label.map(str::to_owned), span, children }
    }

    pub fn label(&self) -> Option<&str> {
        match self {
            Obs::Node { label, .. } | Obs::Leaf { label, .. } => label.as_deref(),
        }
    }

    /// Number of nodes and leaves in the tree.
    pub fn size(&self) -> usize {
        match self {
            Obs::Leaf { .. } => 1,
            Obs::Node { children, .. } => 1 + children.iter().map(Obs::size).sum::<usize>(),
        }
    }
}

/// The four observation classes (compiler design C1-8; LW3-ORDER COMP-4a).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ObsClass {
    /// The lossless Syntax layer: exact equality. Labels, spans, leaf text
    /// and shape all count; nothing is normalised.
    Exact,
    /// The semantic projection after the declared CST normalisation
    /// ([`normalise`]): spans erased, trivia leaves dropped, anonymous
    /// groupings spliced into their parent. Labels and leaf text count.
    Semantic,
    /// Scalar events: the document-order sequence of non-trivia leaf texts.
    /// Labels and shape do not count.
    ScalarEvents,
    /// Left-associative fold: the semantic projection, then every labelled
    /// node whose children form an odd chain `x0 o1 x1 … on xn` (n ≥ 2) is
    /// re-associated as nested left binary nodes `((x0 o1 x1) o2 x2) …` with
    /// the same label. A right-nested chain stays right-nested, so it is
    /// told apart from a left fold.
    LeftFold,
}

impl ObsClass {
    pub const ALL: [ObsClass; 4] =
        [ObsClass::Exact, ObsClass::Semantic, ObsClass::ScalarEvents, ObsClass::LeftFold];

    pub fn name(self) -> &'static str {
        match self {
            ObsClass::Exact => "exact",
            ObsClass::Semantic => "semantic",
            ObsClass::ScalarEvents => "scalar-events",
            ObsClass::LeftFold => "left-fold",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        ObsClass::ALL.into_iter().find(|c| c.name() == s)
    }
}

impl fmt::Display for ObsClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// A trivia leaf: empty text, or text made only of the whitespace the
/// grammar's default `?w` skips (`\s`: space, tab, LF, VT, FF, CR).
pub fn is_trivia(text: &str) -> bool {
    text.bytes().all(|b| matches!(b, b' ' | b'\t' | b'\n' | 0x0b | 0x0c | b'\r'))
}

/// The declared CST normalisation behind [`ObsClass::Semantic`]:
///
/// 1. every span is erased;
/// 2. every trivia leaf ([`is_trivia`]) is dropped;
/// 3. every anonymous node (`label == None`) below the root is replaced by
///    its normalised children, in order;
/// 4. the root keeps its own label (an anonymous root stays a node).
pub fn normalise(t: &Obs) -> Obs {
    match t {
        Obs::Leaf { label, text, .. } => Obs::Leaf { label: label.clone(), span: None, text: text.clone() },
        Obs::Node { label, children, .. } => {
            let mut out = Vec::with_capacity(children.len());
            splice_into(children, &mut out);
            Obs::Node { label: label.clone(), span: None, children: out }
        }
    }
}

fn splice_into(children: &[Obs], out: &mut Vec<Obs>) {
    for c in children {
        match c {
            Obs::Leaf { text, .. } if is_trivia(text) => {}
            Obs::Leaf { label, text, .. } => {
                out.push(Obs::Leaf { label: label.clone(), span: None, text: text.clone() })
            }
            Obs::Node { label: None, children, .. } => splice_into(children, out),
            Obs::Node { label: Some(l), children, .. } => {
                let mut inner = Vec::with_capacity(children.len());
                splice_into(children, &mut inner);
                out.push(Obs::Node { label: Some(l.clone()), span: None, children: inner });
            }
        }
    }
}

fn scalar_events(t: &Obs, out: &mut Vec<Obs>) {
    match t {
        Obs::Leaf { text, .. } if is_trivia(text) => {}
        Obs::Leaf { text, .. } => out.push(Obs::Leaf { label: None, span: None, text: text.clone() }),
        Obs::Node { children, .. } => children.iter().for_each(|c| scalar_events(c, out)),
    }
}

fn left_fold(t: Obs) -> Obs {
    match t {
        leaf @ Obs::Leaf { .. } => leaf,
        Obs::Node { label, span, children } => {
            let children: Vec<Obs> = children.into_iter().map(left_fold).collect();
            if label.is_some() && children.len() >= 5 && children.len() % 2 == 1 {
                let mut it = children.into_iter();
                let mut acc = it.next().expect("odd chain has a head");
                while let (Some(op), Some(rhs)) = (it.next(), it.next()) {
                    acc = Obs::Node { label: label.clone(), span: None, children: vec![acc, op, rhs] };
                }
                acc
            } else {
                Obs::Node { label, span, children }
            }
        }
    }
}

/// `obs(class, tree)`: the observation two parses are compared by.
pub fn obs(class: ObsClass, t: &Obs) -> Obs {
    match class {
        ObsClass::Exact => t.clone(),
        ObsClass::Semantic => normalise(t),
        ObsClass::ScalarEvents => {
            let mut out = Vec::new();
            scalar_events(t, &mut out);
            Obs::Node { label: None, span: None, children: out }
        }
        ObsClass::LeftFold => left_fold(normalise(t)),
    }
}

/// `a ≡_class b`.
pub fn equivalent(class: ObsClass, a: &Obs, b: &Obs) -> bool {
    obs(class, a) == obs(class, b)
}

/// Why a [`FailureContract`] could not be constructed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ContractError {
    /// `expected[at] >= expected[at + 1]`: the set is unsorted or repeats a label.
    NotStrictlySorted { at: usize, left: String, right: String },
}

impl fmt::Display for ContractError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ContractError::NotStrictlySorted { at, left, right } => write!(
                f,
                "expected set is not sorted and deduplicated: [{at}] {left:?} >= [{}] {right:?}",
                at + 1
            ),
        }
    }
}

impl std::error::Error for ContractError {}

/// The failure contract (compiler design C1-8): the furthest offset a parse
/// reached, and the expected-label set at that offset, sorted and
/// deduplicated by construction.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct FailureContract {
    furthest: u32,
    expected: Vec<String>,
}

impl FailureContract {
    /// Refuses an expected set that is not strictly increasing.
    pub fn new(furthest: u32, expected: Vec<String>) -> Result<Self, ContractError> {
        if let Some(at) = expected.windows(2).position(|w| w[0] >= w[1]) {
            return Err(ContractError::NotStrictlySorted {
                at,
                left: expected[at].clone(),
                right: expected[at + 1].clone(),
            });
        }
        Ok(Self { furthest, expected })
    }

    /// The contract with no expected labels (an arm that reports only an offset).
    pub fn at(furthest: u32) -> Self {
        Self { furthest, expected: Vec::new() }
    }

    pub fn furthest(&self) -> u32 {
        self.furthest
    }

    pub fn expected(&self) -> &[String] {
        &self.expected
    }

    /// The contract of two failures of one parse: the further one wins; at an
    /// equal offset the sets are merged (sorted union). A failure at a smaller
    /// offset never replaces one at a larger offset.
    pub fn merge(self, other: Self) -> Self {
        use std::cmp::Ordering::*;
        match self.furthest.cmp(&other.furthest) {
            Greater => self,
            Less => other,
            Equal => {
                let mut out = Vec::with_capacity(self.expected.len() + other.expected.len());
                let (mut a, mut b) = (self.expected.into_iter().peekable(), other.expected.into_iter().peekable());
                loop {
                    match (a.peek(), b.peek()) {
                        (Some(x), Some(y)) => match x.cmp(y) {
                            Less => out.push(a.next().unwrap()),
                            Greater => out.push(b.next().unwrap()),
                            Equal => {
                                out.push(a.next().unwrap());
                                b.next();
                            }
                        },
                        (Some(_), None) => out.push(a.next().unwrap()),
                        (None, Some(_)) => out.push(b.next().unwrap()),
                        (None, None) => break,
                    }
                }
                Self { furthest: self.furthest, expected: out }
            }
        }
    }
}

impl<'de> Deserialize<'de> for FailureContract {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct Raw {
            furthest: u32,
            #[serde(default)]
            expected: Vec<String>,
        }
        let r = Raw::deserialize(d)?;
        FailureContract::new(r.furthest, r.expected).map_err(serde::de::Error::custom)
    }
}
