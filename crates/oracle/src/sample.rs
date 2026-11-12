//! Deterministic generators for the oracle's own laws (SplitMix64; no
//! external property-testing dependency, so a case is reproduced by its seed).

use crate::obs::{Obs, Span};

/// SplitMix64.
#[derive(Clone, Debug)]
pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Self {
        Rng(seed)
    }

    pub fn next_u64(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }

    /// Uniform in `0..n` (`n > 0`).
    pub fn below(&mut self, n: u64) -> u64 {
        self.next_u64() % n
    }

    pub fn chance(&mut self, num: u64, den: u64) -> bool {
        self.below(den) < num
    }

    pub fn pick<'a, T>(&mut self, xs: &'a [T]) -> &'a T {
        &xs[self.below(xs.len() as u64) as usize]
    }
}

const LABELS: [&str; 6] = ["expr", "term", "pair", "list", "atom", "op"];
const TEXTS: [&str; 10] = ["a", "b", "1", "+", "-", "*", "(", ")", "\"s\"", "true"];
const TRIVIA: [&str; 5] = ["", " ", "\n", "\t ", "\r\n"];

fn span(rng: &mut Rng) -> Option<Span> {
    if rng.chance(1, 4) {
        None
    } else {
        let s = rng.below(64) as u32;
        Some((s, s + rng.below(16) as u32))
    }
}

/// A random observation tree of depth at most `depth`.
pub fn tree(rng: &mut Rng, depth: u32) -> Obs {
    if depth == 0 || rng.chance(1, 3) {
        let text = if rng.chance(1, 6) { *rng.pick(&TRIVIA) } else { *rng.pick(&TEXTS) };
        let label = if rng.chance(1, 2) { Some(*rng.pick(&LABELS)) } else { None };
        return Obs::leaf(label, span(rng), text);
    }
    let n = rng.below(6) as usize;
    let children = (0..n).map(|_| tree(rng, depth - 1)).collect();
    let label = if rng.chance(2, 3) { Some(*rng.pick(&LABELS)) } else { None };
    Obs::node(label, span(rng), children)
}

/// A perturbation the semantic normalisation must not see: spans moved,
/// trivia leaves inserted, and labelled-free groupings wrapped around runs of
/// children. The result is semantically equal to `t` and, in general, not
/// exactly equal.
pub fn semantic_noise(rng: &mut Rng, t: &Obs) -> Obs {
    match t {
        Obs::Leaf { label, text, .. } => Obs::Leaf { label: label.clone(), span: span(rng), text: text.clone() },
        Obs::Node { label, children, .. } => {
            let mut kids: Vec<Obs> = children.iter().map(|c| semantic_noise(rng, c)).collect();
            if rng.chance(1, 2) {
                let at = rng.below(kids.len() as u64 + 1) as usize;
                kids.insert(at, Obs::leaf(None, span(rng), *rng.pick(&TRIVIA)));
            }
            if kids.len() >= 2 && rng.chance(1, 2) {
                let a = rng.below(kids.len() as u64) as usize;
                let b = a + 1 + rng.below((kids.len() - a) as u64) as usize;
                let run: Vec<Obs> = kids.drain(a..b).collect();
                kids.insert(a, Obs::node(None, span(rng), run));
            }
            Obs::Node { label: label.clone(), span: span(rng), children: kids }
        }
    }
}

/// A perturbation every class except `ScalarEvents` must see: one label
/// renamed or one non-trivia leaf's text changed. Returns `None` when `t`
/// has nothing to change.
pub fn semantic_change(rng: &mut Rng, t: &Obs) -> Option<Obs> {
    let mut sites = Vec::new();
    collect_sites(t, &mut Vec::new(), &mut sites);
    if sites.is_empty() {
        return None;
    }
    let path = sites[rng.below(sites.len() as u64) as usize].clone();
    let mut out = t.clone();
    let mut cur = &mut out;
    for i in path {
        cur = match cur {
            Obs::Node { children, .. } => &mut children[i],
            Obs::Leaf { .. } => unreachable!(),
        };
    }
    match cur {
        Obs::Leaf { text, .. } => text.push('#'),
        Obs::Node { label, .. } => *label = Some(format!("{}#", label.as_deref().unwrap_or(""))),
    }
    Some(out)
}

/// Paths to the leaves with non-trivia text and to the labelled nodes, i.e.
/// the places the semantic projection keeps.
fn collect_sites(t: &Obs, path: &mut Vec<usize>, out: &mut Vec<Vec<usize>>) {
    match t {
        Obs::Leaf { text, .. } => {
            if !crate::obs::is_trivia(text) {
                out.push(path.clone())
            }
        }
        Obs::Node { label, children, .. } => {
            if label.is_some() && !path.is_empty() {
                out.push(path.clone());
            }
            for (i, c) in children.iter().enumerate() {
                path.push(i);
                collect_sites(c, path, out);
                path.pop();
            }
        }
    }
}

/// A random expected-label list (possibly unsorted, possibly repeating).
pub fn labels(rng: &mut Rng) -> Vec<String> {
    let n = rng.below(5) as usize;
    (0..n).map(|_| (*rng.pick(&LABELS)).to_string()).collect()
}
