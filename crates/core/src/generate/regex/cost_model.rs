//! Centralized cost model for regex emission strategy selection.
//!
//! All heuristic thresholds live here, not scattered across match arms.
//! Thread `&CostModel` through `emit_regex()` and the pass pipeline.

/// Cost model controlling emission strategy selection thresholds.
///
/// Three presets: `DEFAULT` (balanced), `AGGRESSIVE` (prefer SIMD),
/// `CONSERVATIVE` (prefer scalar simplicity).
#[derive(Debug, Clone, Copy)]
pub struct CostModel {
    /// Maximum distinct needle bytes for memchr-based scanning (1–3).
    pub memchr_max_needles: usize,
    /// Maximum excluded bytes for nibble-LUT SIMD scanning (4–8).
    pub nibble_lut_max_excluded: usize,
    /// Maximum DFA states for decision-tree codegen.
    pub decision_tree_max_states: usize,
    /// Maximum DFA states for static transition table codegen.
    pub table_lookup_max_states: usize,
    /// Minimum literal alternation branches for PHF emission.
    pub phf_min_literals: usize,
    /// Minimum group size for factoring shared prefixes.
    pub factor_min_group_size: usize,
    /// Minimum occurrences before hoisting a regex to a shared function.
    pub hoist_min_occurrences: usize,
}

impl CostModel {
    /// Balanced defaults — good for most grammars.
    pub const DEFAULT: Self = Self {
        memchr_max_needles: 3,
        nibble_lut_max_excluded: 8,
        decision_tree_max_states: 12,
        table_lookup_max_states: 64,
        phf_min_literals: 16,
        factor_min_group_size: 2,
        hoist_min_occurrences: 2,
    };

    /// Aggressive SIMD — lower thresholds, prefer vectorized paths.
    pub const AGGRESSIVE: Self = Self {
        memchr_max_needles: 3,
        nibble_lut_max_excluded: 12,
        decision_tree_max_states: 16,
        table_lookup_max_states: 128,
        phf_min_literals: 8,
        factor_min_group_size: 2,
        hoist_min_occurrences: 2,
    };

    /// Conservative — prefer scalar simplicity, minimize code size.
    pub const CONSERVATIVE: Self = Self {
        memchr_max_needles: 2,
        nibble_lut_max_excluded: 4,
        decision_tree_max_states: 6,
        table_lookup_max_states: 32,
        phf_min_literals: 32,
        factor_min_group_size: 3,
        hoist_min_occurrences: 3,
    };
}

impl Default for CostModel {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// Hint about expected match length, derived from grammar context.
///
/// Used by SIMD strategy selection — memchr's ~15-cycle setup cost
/// makes scalar loops faster for short inputs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LengthHint {
    /// `?` or `{1,4}` — typically 1–4 bytes. Prefer scalar.
    Short,
    /// Moderate-length matches. Default strategy selection.
    Medium,
    /// Top-level `+`/`*` in token scanner. Prefer SIMD.
    Long,
    /// Unknown context. Use default strategy.
    #[default]
    Unknown,
}

/// Options for regex emission, threaded through `emit_regex()`.
#[derive(Debug, Clone)]
pub struct EmitOpts<'a> {
    /// Whether to fuse JSON number regex with f64 conversion.
    pub fuse_numbers: bool,
    /// Expected match length hint for SIMD strategy selection.
    pub length_hint: LengthHint,
    /// Cost model for threshold-based decisions.
    pub cost: &'a CostModel,
    /// Grammar IR — when set, regex classification is resolved via the
    /// `ir.regex_info` cache (populated by `compute_regex_info` pass)
    /// instead of re-parsing the HIR. Callers without an IR (e.g.
    /// standalone fast-path tests) leave this `None` and pay the parse.
    pub ir: Option<&'a bbnf_ir::GrammarIR>,
}

impl<'a> EmitOpts<'a> {
    pub fn new(cost: &'a CostModel) -> Self {
        Self {
            fuse_numbers: false,
            length_hint: LengthHint::Unknown,
            cost,
            ir: None,
        }
    }

    pub fn with_fuse(mut self, fuse: bool) -> Self {
        self.fuse_numbers = fuse;
        self
    }

    pub fn with_length_hint(mut self, hint: LengthHint) -> Self {
        self.length_hint = hint;
        self
    }

    pub fn with_ir(mut self, ir: &'a bbnf_ir::GrammarIR) -> Self {
        self.ir = Some(ir);
        self
    }

    /// Classify a regex pattern via the `ir.regex_info` cache when
    /// available, else fall back to a fresh HIR parse.
    pub fn classify_regex(
        &self,
        pattern: &str,
    ) -> ::parse_that::regex::classify::RegexClass {
        if let Some(ir) = self.ir {
            if let Some(info) = find_regex_info(ir, pattern) {
                return info.classification.clone();
            }
        }
        ::parse_that::regex::classify::classify_regex(pattern)
    }
}

/// Resolve `pattern` → `&RegexInfo` via reverse-lookup through the
/// string pool. Interning is O(n) over `ir.strings`, but that's still
/// a handful of byte comparisons vs. a full HIR parse on cache miss.
fn find_regex_info<'a>(
    ir: &'a bbnf_ir::GrammarIR,
    pattern: &str,
) -> Option<&'a ::parse_that::regex::RegexInfo> {
    let sid = ir.strings.iter().position(|s| s == pattern)? as u32;
    ir.regex_info.get(&sid)
}
