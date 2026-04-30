//! Grammar-activated structural-scan policy facts.
//!
//! The emitter derives per-rule structural-scan admission from
//! CSP-inferred FIRST-set facts intersected with the grammar's mined
//! `structural_alphabet` + `structural_digraph_mask`. The facts are
//! consumed during codegen by structural-scan-admitting shapes
//! (`object_key_seek` inlining in `__path_walk`, bounded lookahead in
//! regex-scan adapters). No policy table is emitted into generated
//! runtime modules.
//!
//! Sample entries (shape per grammar):
//!
//! - JSON `object`: Dense admission (FIRST intersect alphabet =
//!   `{`, `:`, `,`, `}`), flags=OBJECT_KEY_SEEK |
//!   BOUNDED_LOOKAHEAD | SCAN_STRUCTURAL_BOUNDED.
//! - CSS L4 `declaration`: Dense admission (`:`, `;`, `/`), flags=
//!   BOUNDED_LOOKAHEAD | SCAN_STRUCTURAL_BOUNDED | DIGRAPH_ADMIT
//!   for comment digraph `/*`.
//! - Sheets `cell_ref`: Sparse admission (`:` for range), flags=
//!   BOUNDED_LOOKAHEAD.
//! - BBNF `rule`: Digraph admission (`->`, `(*`, `*)`), flags=
//!   BOUNDED_LOOKAHEAD | DIGRAPH_ADMIT.

use bbnf_ir::GrammarIR;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScanAlphabetClass {
    Empty,
    Sparse,
    Dense,
    Digraph,
}

/// Emitter-private activation flags for structural-scan admission.
/// These never cross into generated runtime code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ScanActivationFlags(u8);

impl ScanActivationFlags {
    /// Inlines object-key seek for value-position lookups.
    pub(crate) const OBJECT_KEY_SEEK: u8 = 0b0000_0001;
    /// Inlines bounded lookahead for child/key scans.
    pub(crate) const BOUNDED_LOOKAHEAD: u8 = 0b0000_0010;
    /// Inlines bounded structural scanning for positional access.
    pub(crate) const SCAN_STRUCTURAL_BOUNDED: u8 = 0b0000_0100;
    /// Emits a digraph-aware structural probe.
    pub(crate) const DIGRAPH_ADMIT: u8 = 0b0000_1000;

    #[inline]
    const fn from_bits(bits: u8) -> Self {
        Self(bits)
    }

    /// Probe a single flag.
    #[inline]
    pub(crate) const fn contains(self, flag: u8) -> bool {
        (self.0 & flag) == flag
    }
}

/// Look up structural-scan activation for a rule. This is an
/// emission-time helper: generated code receives only the inlined
/// primitive selected from the returned flags, never a policy table.
pub(crate) fn lookup_scan_policy<'ir>(
    ir: &'ir GrammarIR,
    rule_id: bbnf_ir::RuleId,
) -> Option<ScanActivationFlags> {
    use crate::generate::regex::byte_class::classify_rule_alphabet;
    use bbnf_ir::IrNode;

    let rule = ir.rules.iter().find(|r| r.id == rule_id)?;
    if rule.meta.is_transparent {
        return None;
    }

    let profile = ir.profile();
    let structural_alphabet = profile.structural_alphabet.to_vec();
    let structural_digraph_mask = profile.structural_digraph_mask;

    let first_bytes: Vec<u8> = rule.meta.first_set.iter().collect();
    let is_compound = matches!(
        &rule.body,
        IrNode::Seq(_) | IrNode::Alt(_, _) | IrNode::Repeat { .. } | IrNode::TokenDispatch { .. }
    );

    let facts = classify_rule_alphabet(
        &first_bytes,
        &structural_alphabet,
        &structural_digraph_mask,
        is_compound,
    );

    let class = if facts.admits_digraph && !structural_alphabet.is_empty() {
        ScanAlphabetClass::Digraph
    } else if facts.alphabet_intersection_count >= 4 {
        ScanAlphabetClass::Dense
    } else if facts.alphabet_intersection_count >= 1 {
        ScanAlphabetClass::Sparse
    } else {
        ScanAlphabetClass::Empty
    };

    let mut flags: u8 = 0;
    if facts.is_compound {
        match class {
            ScanAlphabetClass::Dense => {
                flags |= ScanActivationFlags::OBJECT_KEY_SEEK;
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
            }
            ScanAlphabetClass::Sparse => {
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
            }
            ScanAlphabetClass::Digraph => {
                flags |= ScanActivationFlags::BOUNDED_LOOKAHEAD;
                flags |= ScanActivationFlags::SCAN_STRUCTURAL_BOUNDED;
                flags |= ScanActivationFlags::DIGRAPH_ADMIT;
            }
            ScanAlphabetClass::Empty => {}
        }
    }

    Some(ScanActivationFlags::from_bits(flags))
}
