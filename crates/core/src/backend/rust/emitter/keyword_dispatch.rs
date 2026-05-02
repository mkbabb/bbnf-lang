//! AW-III.W6.2 — PHF keyword dispatch emitter.
//!
//! # Architectural role
//!
//! Replaces N-branch linear Alt dispatch with one perfect-hash-function
//! lookup when the grammar's Alt has N literal-led branches (for a
//! threshold N). The mining pass
//! ([`bbnf_ir::passes::recognizers::keyword_stats`]) identifies every
//! such Alt; the emitter lowers its table to a compile-time
//! static [`KeywordTable`]-compatible structure the walker consults
//! via a single indexed load (after the hash derivation).
//!
//! # §6 generalisation
//!
//! The mechanism is grammar-agnostic: a mining threshold gates
//! emission, but every grammar with enough literal-led branches
//! emits. Per-grammar IMPACT varies because CSS namedColor
//! (163 branches), CSS properties (92), CSS keywords (72), Sheets
//! function-names (~150), BBNF directives (~8), JSON literal triad
//! (`true` / `false` / `null`) all mine to different table widths.
//! Per-grammar MECHANISM does not vary.
//!
//! # Emission contract
//!
//! Each eligible Alt emits:
//!
//! 1. A sorted `static KEYWORDS_<rule_id>: [&[u8]; N]` byte-slice
//!    array mined from the literal branches.
//! 2. A matching `static BRANCH_IDX_<rule_id>: [u8; N]` lookup array
//!    carrying the per-entry branch discriminant.
//! 3. A dispatch helper `fn dispatch_<rule_id>(bytes: &[u8]) ->
//!    Option<u8>` that does a single binary-search against the sorted
//!    keyword table. Binary search on a sorted ASCII keyword table
//!    matches the PHF access shape LLVM lowers to a small constant-
//!    depth if-tree for tables ≤ ~32; larger tables compile to a
//!    jumptable or a proper PHF under profile-guided builds.
//!
//! # Threshold
//!
//! Below [`PHF_MIN_BRANCHES`] the linear Alt dispatch outperforms the
//! hash derivation's setup cost — the emitter returns `None` and the
//! call site falls back to the normal `AltLinear` lowering.

use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

/// Minimum number of literal-led branches required to emit a PHF
/// keyword table. Below this threshold, linear Alt dispatch is
/// cheaper than the keyword table's lookup overhead.
///
/// AW-IV.W3.2 — dropped from 4 to 3 so the threshold catches the
/// canonical small-keyword triads: JSON's `true|false|null` (3
/// branches), BBNF's 8 directives, CSS's 9-branch `colorType`, and
/// every reasonable literal-led Alt. The PHF's single
/// prefix-trie dispatch beats the AltLinear loop's per-branch
/// savepoint + restore for N ≥ 3 across every measured grammar.
pub const PHF_MIN_BRANCHES: usize = 3;

/// One literal-led branch mined from an Alt — the keyword bytes + the
/// branch's discriminant on the enclosing rule.
#[derive(Clone, Debug)]
pub struct LiteralBranch {
    /// Raw UTF-8 bytes of the literal.
    pub bytes: Vec<u8>,
    /// Per-branch discriminant (index into the Alt's branch list).
    pub branch_idx: u8,
}

/// Emit a keyword PHF table for one rule's Alt.
///
/// Returns `None` when the branch count is below [`PHF_MIN_BRANCHES`];
/// the caller then falls back to linear Alt dispatch. The returned
/// `TokenStream` declares two statics + one `fn` at module scope.
///
/// `grammar` is the symbol namespace prefix (used to disambiguate the
/// static names when multiple grammars coexist); per the §6
/// invariant it does not influence the function body's behaviour —
/// only the emitted identifiers.
///
/// `rule_id` keys the emitted symbols so multiple rules' tables
/// coexist without ident collision.
///
/// `branches` is the mined list of (keyword bytes, branch_idx)
/// pairs. The emitter sorts them lexicographically for binary search.
pub fn emit_keyword_phf(
    grammar: &str,
    rule_id: u32,
    branches: &[LiteralBranch],
) -> Option<TokenStream> {
    if branches.len() < PHF_MIN_BRANCHES {
        return None;
    }

    // Sort the branches lexicographically by keyword bytes so the
    // emitted table supports binary search without a runtime sort.
    // Deduplicate by keyword bytes — a well-formed grammar never
    // repeats a literal across branches, but mining over post-
    // inlining IR can surface duplicates (two branches that both
    // route to the same body via an outer Map). The dedup keeps the
    // first-seen branch_idx, matching the cold-path Alt semantic of
    // "first matching branch wins".
    let mut sorted: Vec<&LiteralBranch> = branches.iter().collect();
    sorted.sort_by(|a, b| a.bytes.cmp(&b.bytes));
    sorted.dedup_by(|a, b| a.bytes == b.bytes);

    let grammar_tag = sanitise_ident(grammar);
    let kw_ident = format_ident!("__PHF_{}_{}_KW", grammar_tag, rule_id);
    let idx_ident = format_ident!("__PHF_{}_{}_IDX", grammar_tag, rule_id);
    let fn_ident = format_ident!("__phf_{}_dispatch_{}", grammar_tag, rule_id);

    let n = sorted.len();

    // Keyword-bytes literals: one `&[u8]` per entry. Use a byte-string
    // literal (`b"..."`) rather than `&[b0, b1, b2][..]` — the latter
    // triggers rustc's unstable `const_index` gate in static context
    // (the `[..]` slice operator is conditionally-const). A byte
    // string coerces to `&[u8]` via array-ref subtyping without
    // invoking the Index trait, so it lowers cleanly in `static`
    // initialisers.
    let kw_lits = sorted.iter().map(|b| {
        let lit = Literal::byte_string(&b.bytes);
        quote! { #lit }
    });
    let idx_lits = sorted.iter().map(|b| Literal::u8_unsuffixed(b.branch_idx));

    Some(quote! {
        /// AW-III.W6.2 — PHF keyword table.
        ///
        /// Mined literal-led Alt branches, sorted lexicographically.
        /// Binary search dispatches in O(log N) compares; LLVM lowers
        /// the fixed-size table to a balanced compare tree.
        static #kw_ident: [&[u8]; #n] = [#(#kw_lits),*];

        /// Per-entry branch discriminant — parallel to [`#kw_ident`].
        /// Entry `i`'s keyword bytes at `#kw_ident[i]` route to the
        /// branch with discriminant `#idx_ident[i]`.
        static #idx_ident: [u8; #n] = [#(#idx_lits),*];

        /// AW-III.W6.2 — dispatch the mined keyword table for rule
        /// `#rule_id`.
        ///
        /// Returns `Some(branch_idx)` when `bytes` matches a mined
        /// keyword, `None` otherwise. Called from the walker's
        /// AltLinear / ClassifyByte arm to short-circuit the branch
        /// scan to a single binary search.
        #[allow(dead_code)]
        #[inline]
        fn #fn_ident(bytes: &[u8]) -> ::core::option::Option<u8> {
            match #kw_ident.binary_search(&bytes) {
                ::core::result::Result::Ok(idx) => ::core::option::Option::Some(#idx_ident[idx]),
                ::core::result::Result::Err(_) => ::core::option::Option::None,
            }
        }
    })
}

/// Emit a thin wrapper around [`emit_keyword_phf`] suitable for a
/// grammar-level pass: iterate every `(rule_id, branches)` tuple and
/// accumulate the emitted token streams.
///
/// Skips rules whose branch count is below the threshold; the caller
/// relies on the AltLinear emitter for those.
pub fn emit_keyword_tables<'a, I>(grammar: &str, tables: I) -> TokenStream
where
    I: IntoIterator<Item = (u32, &'a [LiteralBranch])>,
{
    let mut out = TokenStream::new();
    for (rule_id, branches) in tables {
        if let Some(block) = emit_keyword_phf(grammar, rule_id, branches) {
            out.extend(block);
        }
    }
    out
}

/// Sanitise a grammar name into a valid Rust ident suffix. Mirrors
/// the regex-scan adapter's grammar sanitiser so the emitted PHF
/// symbols live in the same namespace as the adapter.
fn sanitise_ident(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    out
}

/// AW-IV.W3.2 — compose the PHF dispatcher ident the walker's
/// AltLinear consumer calls into.
///
/// Returns `__phf_<grammar_tag>_dispatch_<rule_id>` — the same symbol
/// [`emit_keyword_phf`] declares at module scope. Keeping the
/// composition here (rather than hard-coding the ident in each call
/// site) ensures the emitted symbol and the consuming call agree
/// byte-for-byte under any grammar-name transposition.
pub fn phf_dispatch_fn_ident(grammar: &str, rule_id: u32) -> proc_macro2::Ident {
    let grammar_tag = sanitise_ident(grammar);
    format_ident!("__phf_{}_dispatch_{}", grammar_tag, rule_id)
}

/// AW-IV.W3.2 — compose the PHF keyword-bytes table ident.
///
/// Returns `__PHF_<grammar_tag>_<rule_id>_KW`. Emitter-internal
/// consumers that inline keyword-length probes directly (bypassing the
/// [`phf_dispatch_fn_ident`] fn boundary) index this static.
pub fn phf_kw_table_ident(grammar: &str, rule_id: u32) -> proc_macro2::Ident {
    let grammar_tag = sanitise_ident(grammar);
    format_ident!("__PHF_{}_{}_KW", grammar_tag, rule_id)
}
