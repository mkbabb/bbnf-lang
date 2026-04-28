//! Shared boolean DSL fixture for the `ruler_*` integration tests.
//!
//! Defines a tiny propositional language (`True`, `False`, `Not`, `And`)
//! that implements every trait the Ruler substrate consumes:
//! `Alphabet`, `LangNode`, `Language`, and `Interpreter`. Used as the
//! grammar-agnostic happy path for the substrate's contract tests.
//!
//! Lives in `tests/common/` rather than per-file so the three modules
//! (`enumerate`, `oracle`, `residue`) can share a single fixture
//! without duplicating ~80 LOC of boilerplate.

#![allow(dead_code)]

use egraph::Id;
use egraph::Language;
use egraph::ruler::{Alphabet, Interpreter, LangNode, Pattern};

/// Variant tag for the toy boolean DSL.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Tag {
    True,
    False,
    Not,
    And,
}

/// E-node form: tag + child e-class IDs (consumed by the residue
/// filter's internal e-graph). Variant choice (Leaf/Unary/Binary) is
/// pure arity-encoding convenience — `Tag` carries the actual variant.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Bool {
    Leaf(Tag),
    Unary(Tag, [Id; 1]),
    Binary(Tag, [Id; 2]),
}

impl Language for Bool {
    fn children(&self) -> &[Id] {
        match self {
            Bool::Leaf(_) => &[],
            Bool::Unary(_, c) => c,
            Bool::Binary(_, c) => c,
        }
    }
    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Bool::Leaf(_) => &mut [],
            Bool::Unary(_, c) => c,
            Bool::Binary(_, c) => c,
        }
    }
}

pub struct BoolAlphabet;

impl Alphabet for BoolAlphabet {
    type Tag = Tag;
    fn variants(&self) -> &[(Self::Tag, usize)] {
        &[
            (Tag::True, 0),
            (Tag::False, 0),
            (Tag::Not, 1),
            (Tag::And, 2),
        ]
    }
}

impl LangNode for Bool {
    type Alphabet = BoolAlphabet;

    fn build(tag: Tag, children: Vec<Pattern<Bool>>) -> Option<Pattern<Bool>> {
        let arity_ok = match tag {
            Tag::True | Tag::False => children.is_empty(),
            Tag::Not => children.len() == 1,
            Tag::And => children.len() == 2,
        };
        if !arity_ok {
            return None;
        }
        Some(Pattern { tag, children })
    }

    fn build_node(tag: Tag, children: Vec<Id>) -> Option<Bool> {
        match (tag, children.len()) {
            (Tag::True | Tag::False, 0) => Some(Bool::Leaf(tag)),
            (Tag::Not, 1) => Some(Bool::Unary(tag, [children[0]])),
            (Tag::And, 2) => Some(Bool::Binary(tag, [children[0], children[1]])),
            _ => None,
        }
    }
}

/// Reference interpreter for the boolean DSL.
pub struct BoolInterpreter;

impl Interpreter<Bool> for BoolInterpreter {
    type Output = bool;
    type Input = ();

    fn eval(&self, term: &Pattern<Bool>, _input: &Self::Input) -> Self::Output {
        match term.tag {
            Tag::True => true,
            Tag::False => false,
            Tag::Not => !self.eval(&term.children[0], _input),
            Tag::And => {
                self.eval(&term.children[0], _input) && self.eval(&term.children[1], _input)
            }
        }
    }

    fn sample_inputs(&self) -> Vec<Self::Input> {
        vec![()]
    }
}

// ── Pattern constructors ────────────────────────────────────────────

/// Constant `true`.
pub fn t() -> Pattern<Bool> {
    Bool::build(Tag::True, Vec::new()).unwrap()
}

/// Constant `false`.
pub fn f() -> Pattern<Bool> {
    Bool::build(Tag::False, Vec::new()).unwrap()
}

/// Logical negation.
pub fn not(p: Pattern<Bool>) -> Pattern<Bool> {
    Bool::build(Tag::Not, vec![p]).unwrap()
}

/// Logical conjunction.
pub fn and(a: Pattern<Bool>, b: Pattern<Bool>) -> Pattern<Bool> {
    Bool::build(Tag::And, vec![a, b]).unwrap()
}
