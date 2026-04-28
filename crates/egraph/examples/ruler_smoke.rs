//! End-to-end smoke probe for the Ruler substrate.
//!
//! Exercises the three modules in the order a real synthesis driver
//! would: enumerate candidates, residue-filter the universe of pairs,
//! then oracle-check what's left. Output is a `search:N apply:M`
//! counter pair: `search` is the number of pairs the residue filter
//! examined, `apply` is the number of pairs the oracle endorsed as
//! equivalent.
//!
//! Set `bbnf_egraph_report=1` in the environment for a slightly more
//! verbose breakdown.
//!
//! Run via:
//!
//! ```text
//! cargo run -p egraph --example ruler_smoke
//! ```

use egraph::{Id, Language};
use egraph::ruler::{
    Alphabet, EnumerateConfig, EquivalenceResult, Interpreter, LangNode, OracleConfig,
    Pattern, ResidueFilter, check_equivalence, enumerate,
};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
enum Tag {
    True,
    False,
    Not,
    And,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
enum Bool {
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

struct BoolAlphabet;

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

struct BoolInterpreter;

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

fn main() {
    let verbose = std::env::var("bbnf_egraph_report").ok().as_deref() == Some("1");

    // ── Enumerate candidate patterns ────────────────────────────────
    let cfg = EnumerateConfig {
        max_depth: 2,
        max_size: 8,
        seed: 0,
    };
    let alpha = BoolAlphabet;
    let patterns: Vec<Pattern<Bool>> = enumerate::<_, Bool>(&cfg, &alpha).collect();

    if verbose {
        eprintln!("enumerate: {} patterns", patterns.len());
        for p in &patterns {
            eprintln!("  pattern depth={} size={} tag={:?}", p.depth(), p.size(), p.tag);
        }
    }

    // ── Residue filter + oracle pipeline ────────────────────────────
    let mut filter: ResidueFilter<Bool> = ResidueFilter::new();
    let oracle_cfg = OracleConfig::default();
    let interp = BoolInterpreter;

    // `search` counts pairs examined (every (lhs, rhs) cross product
    // entry the residue filter asks about). `apply` counts pairs the
    // oracle endorses as equivalent.
    let mut search: u64 = 0;
    let mut apply: u64 = 0;

    for (i, lhs) in patterns.iter().enumerate() {
        for rhs in patterns.iter().skip(i + 1) {
            search += 1;
            if !filter.is_residual(lhs, rhs) {
                // Residue filter discharged the pair — no oracle needed.
                continue;
            }
            match check_equivalence(&oracle_cfg, &interp, lhs, rhs) {
                EquivalenceResult::Equivalent => {
                    apply += 1;
                }
                _ => {}
            }
        }
    }

    println!("search:{} apply:{}", search, apply);

    if verbose {
        eprintln!(
            "residue-filter e-graph state: classes={} nodes={}",
            filter.egraph().total_classes(),
            filter.egraph().total_nodes()
        );
    }

    // Sanity: with depth-2 patterns the boolean DSL has known
    // equivalences (e.g., not(false) ≡ true, and(t,t) ≡ true), so
    // `apply` should always be > 0. Likewise `search` > 0 unless the
    // enumerator emitted nothing.
    assert!(search > 0, "expected non-empty search space");
    assert!(apply > 0, "expected at least one accepted equivalence");
}
