# LOCK-6 · Family 6A — MIRROR-TREE · pass-3 research

Route: hand-maintained mirrored module trees + an isomorphism LINTER that verifies (a)
tree correspondence and (b) a name map; code idiomatic per language. This report makes the
linter concrete against the *one live cross-language pair* in the greenfield tree —
`crates/bbnf-path` (Rust `path!` proc-macro) mirrored by `crates/bbnf-path-ts` (a
`serde-wasm-bindgen` cdylib + hand TS at `ts/path.ts`), both validating the *same* path
literals against the *same* per-grammar `StructRegistry`.

---

## 0. The live pair, as substrate (measured ground, not a hypothetical)

Three limbs carry the same taxonomy across the boundary:

- **Canonical spine** — `crates/core/src/path/ir.rs` (`OwnedPathSegment`:162, `TypedPath`:146,
  `PathSegment`:42) and `crates/core/src/path/error.rs` (`PathError`:90 with fields
  `segment_index`/`segment_str`/`struct_name`/`alternatives`/`reason` at :92-107,
  `PathErrorReason`:44 with 8 variants). Consumed by both frontends via
  `check_path_against_registry` (`type_check.rs:59`).
- **Rust serde bridge** — `crates/bbnf-path-ts/src/schema.rs`: `OwnedSegmentPayload`:26,
  `TypedPathPayload`:45, `PathErrorReasonPayload`:59, `PathErrorPayload`:81. Hand-authored
  owned-serde twins (borrowed `&'static str` alternatives → `Vec<String>`, `serde(tag="kind")`).
- **TS interface** — `crates/bbnf-path-ts/ts/path.ts`: `OwnedPathSegment`:21, `TypedPath`:31,
  `PathErrorReason`:37, `PathErrorPayload`:52, `PathError extends Error`:62. A third
  hand-authored mirror with `format`/`reasonText` presentation (:81-101).

The lexer is the counter-case: **one** grammar-derived source, `bbnf_regex::lex_path`, is
called by *both* frontends — `bbnf-path/src/path_macro.rs:270` and
`bbnf-path-ts/src/compile.rs:125`. It cannot drift because there is no second copy; the TS
side runs the compiled Rust behind wasm. This bifurcation — *shared-source* names vs
*hand-mirrored* names — is the axis the whole family turns on.

---

## Q1 — the exemption budget, grep-countable and not fit to the total

**The failure mode this must avoid.** If the exemption cap is set equal to the observed
exemption count, the linter is tautological: it can never RED a *new* illegitimate exemption,
and it "passes" precisely because it was fit to the data. The cap must instead be *derived*
from structural inputs independent of the exemption total.

**Inputs, defined by grep, not by counting exemptions.**

- **T = spine cardinality** — the canonical authored taxonomy names on the cross-language
  spine. Grep rule: `rg -c 'pub (struct|enum) ' crates/core/src/path/{ir,error}.rs` counting
  the types that appear in *both* an owned/serde and a TS form. For the live pair
  T = { `OwnedPathSegment`, `TypedPath`, `PathError`, `PathErrorReason` } ⇒ **T = 4** (the
  borrowed forms `PathSegment`/`Path`, `ir.rs:42/74`, are not on the spine — they map to the
  owned form across the boundary and are themselves an idiom, below).

- **I = idiom classes** — a *closed, enumerated* registry of the reasons a name may lawfully
  differ across the boundary. Each class is a **relational rewrite rule** r: T → names, not a
  bare suffix. For the live pair, exactly **five** classes exist, each grep-detectable:

  | # | idiom class | rewrite rule r(t) | live members (grep) | count |
  |---|---|---|---|---|
  | I1 | serde transport mirror | `t` → owned serde twin (`*Payload`) | `schema.rs` `*Payload` | 4 |
  | I2 | native-target shim | `f` → `f_native` (wasm export panics off-wasm) | `lib.rs:69,77` | 2 |
  | I3 | language-native error | `PathError` → JS `Error` subclass (`format`/`reasonText`) | `path.ts:62` | 1 |
  | I4 | surface-syntax constructor | `path!` macro ↔ `makePathTag`/`TEMPLATE_TAG_JS` tag | `path.ts:114`, `template_tag.rs:20` | 2 |
  | I5 | marker representation | Rust marker ZST ↔ TS string | `markers.rs:15/20/25` (`Json`/`CssL4`/`Sheets`) ↔ `"Json"`… | 4 |

**The cap, derived.** A non-identity name on side B is a *legitimate exemption* iff it is the
image of a spine name under some idiom rule:

```
legit(B) = { b ∈ names(B) : b = t   (identity mirror)     for some t ∈ T }
         ∪ { b ∈ names(B) : b = r(t) for some r ∈ I, t ∈ T }
cap = |legit(B)|              (never a tuned scalar)
RED  ⇔  ∃ b ∈ names(B) \ legit(B)
```

- **Reproduces the live count without fitting it.** Every current exemption is `r(t)` for a
  real spine `t`, so the derived `legit(B)` equals the observed set *by construction of the
  rules*, not by a matched integer. The number `4+2+1+2+4` is an output, never an input.
- **RED on a synthetic extra exemption.** Inject `export interface FooView {…}` into
  `ts/path.ts` (or `pub struct BogusPayload` into `schema.rs`): `FooView` is neither an
  identity mirror nor `r(t)` for any spine `t` (no canonical `Foo`), so it is unbudgeted →
  RED. Because rules are *relational* (`r(t)` requires the pre-image `t ∈ T`), the classic
  suffix-gaming hole — `BogusPayload` with no canonical `Bogus` — is also RED: `Bogus ∉ T`.
- **Forbids growth at a defended count.** A new name can only be added as (i) an identity
  mirror — then it is not an exemption; (ii) `r(t)` for an *existing* spine `t` — but that
  image already exists (a second payload for the same `t` REDs as a duplicate); or (iii)
  under a *new* idiom rule, which requires editing the closed `I` registry — a reviewed diff,
  gated at the class level, never the name level. Growth of `I` is the only lawful growth and
  it is human-reviewed; growth of exemptions at fixed `I` is impossible.

**Answer to "how many idiom classes":** **five** on the live pair (I1-I5), and the family's
claim is that five is *closed* for a transport-mirror boundary — three data-shape idioms
(I1 serde-owned, I3 native-error, I5 marker-rep), two surface idioms (I2 target-shim, I4
constructor-syntax). A sixth would have to name a genuinely new axis of lawful divergence and
enter the registry by review.

**The concrete gap this exposes.** The linter is only as good as its reach. Today the
`schema.rs` (Rust twin) is behaviorally tied to canonical by the exhaustive match
`rust_reason_to_payload` (`tests/isomorphic_path_error.rs:89`), which *compile-fails* on
Rust-side drift. **Nothing parses `ts/path.ts`.** The third limb — the TS union
`PathErrorReason` (:37) and its `reasonText` switch (:90) — has no automated gate; a variant
added to canonical + payload but omitted from the TS union links to nothing. So the pass-3
name-map linter must parse `ts/path.ts` declarations (a `tsc` type-extraction or a
`ts-morph`/AST grep) and run `legit(B)` over the TS surface, not only the Rust twin.

---

## Q2 — provenance traces for two spine names

**Name A — `PathErrorReason` (the 8-variant failure taxonomy).** *Authored independently on
every side.* Origins: canonical `crates/core/src/path/error.rs:44` (hand-authored — a
resolver classification, **not** grammar-derived); Rust twin `schema.rs:59`
`PathErrorReasonPayload` (a *second*, independent enum); TS union `ts/path.ts:37` (a *third*).
The only automated tie is the exhaustive Rust match `rust_reason_to_payload`
(`isomorphic_path_error.rs:89-100`) — total over the canonical arm, so it compile-fails if
canonical grows and the twin does not. The TS union is tied by **nothing** compiled. This is
the archetypal *hand-mirror* spine name: N copies, partial gating, TS limb exposed.

**Name B — `lex_path` / the path-segment lexer.** *Single grammar-derived source.* There is
one definition, `bbnf_regex::lex_path`, consumed unchanged by the Rust proc-macro
(`path_macro.rs:270`) and by the TS-bridge cdylib (`compile.rs:125`). Because the TS frontend
executes this compiled Rust behind wasm, the lexer *cannot* diverge — isomorphism by shared
source, the strongest form. Its segment output flows into `bbnf_ir::PathSegment` /
`StructRegistry` (`isomorphic_path_error.rs:19-21`), also single-source.

**The lesson.** The clean spine partitions by provenance: (a) **single-source** names
(`lex_path`, `PathSegment`, the `StructRegistry` field/rule names projected from `TypeDesc`)
that are isomorphic *by construction* and need **no** mirror check; (b) **hand-mirror** names
(`PathErrorReason`, `OwnedPathSegment` variants, `PathError` fields) replicated 2-3× that need
the exemption budget + a name-map gate. The linter must know which class each name is in — and
that knowledge is the grammar/IR's, not LOCK-6's (Q3).

---

## Q3 — ownership of the provenance / name-map linter (LOCK-3 vs LOCK-6)

Two distinct gates hide under "the linter":

1. **The provenance manifest** — *is this name single-source (grammar-derived) or authored?*
   This is a fact only the IR tower knows: `StructRegistry` (`bbnf_ir::registry`) emits the
   canonical `rule_name`/field `name`/`FieldSource` from the grammar via `TypeDesc`
   (`isomorphic_path_error.rs:36-80`); only that layer can label a name grammar-derived
   (single-source, exempt from mirror-check) vs authored. **Owner: LOCK-3.** Per the XL-B
   tower frame (LOCK-3 owns the middle-IR tower + decision-driver seat), provenance *is* the
   tower's contract. LOCK-3 emits, as a `cargo xtask regen` byproduct, a checked-in
   `provenance.manifest` (name → {single-source | authored}) governed by the clean-regen
   discipline (never hand-patched).

2. **The name-map / exemption linter** — *does each authored spine name have an identity
   mirror or a legitimate `r(t)` exemption on the other side, within budget?* This gate reads
   TS/serde surface idioms (I1-I5) — knowledge that must **not** live in the backend-agnostic
   IR tower (that would violate the backend-agnostic-types law; `TypeDesc::Named` etc. know
   nothing of `*Payload` or JS `Error`). **Owner: LOCK-6.** It *consumes* LOCK-3's manifest:
   single-source names are skipped (can't drift); only `authored` names enter `legit(B)`.

**Rationale against the tower frame.** Homing provenance in LOCK-6 would force LOCK-6 to
re-derive what the grammar already produced (duplicating the tower); homing the mirror gate
in LOCK-3 would inject TS-idiom awareness into a backend-agnostic middle (a layering
violation). The clean seam is the **manifest file**: LOCK-3 is the source of provenance
truth (it regenerates it), LOCK-6 is the isomorphism gate that reads it. This is the same
producer/consumer shape the tower uses everywhere and keeps each lock's concern intact.

---

## Evidence — behavioral drive of the TS surface vs canonical Rust (the honest state)

The one running oracle today is `tests/isomorphic_path_error.rs`: it drives the TS-bridge
compile path against canonical `check_path_against_registry` over a shared fixture registry,
comparing `(segment_index, segment_str, struct_name, alternatives, reason)` field-for-field
(`:148-165`), plus exhaustive reason-variant isomorphism (`:228-267`) and an unknown-marker →
`UnregisteredRule` case (`:210-225`).

**But this is exactly the intra-Rust comparison the directive warns against.** The entry it
drives, `compile_path_native` (`lib.rs:69`), is a *Rust* function — the cdylib's native shim
that exists *because* the `#[wasm_bindgen]` exports panic off-wasm (`lib.rs:60-68`). The test
never executes `ts/path.ts`, never crosses `serde-wasm-bindgen`, never runs Node. The
third limb — the hand TS interfaces and `PathError.format`/`reasonText` — is behaviorally
**un-driven**. The claimed "W5.2 TS Node Execution Proof" (`lib.rs:117`) is undelivered:
`rg 'spawn|Command::new."node"' crates/bbnf-path-ts` returns nothing.

**A live counterexample the pair already documents.** On `$.statuses[0].nope` the two
surfaces *disagree*: the canonical offline checker leaves `TypeDesc::Named(_)` intact and
surfaces `UnregisteredRule`, while the cdylib resolves `Named(_)` eagerly through
`layout(_)` and surfaces `UnknownField` (`isomorphic_path_error.rs:129-138`). This is a
*behavioral* divergence — **not** a name/layout idiom — so the exemption budget (Q1) would
never catch it; only a behavioral tier would. It marks the honest boundary of MIRROR-TREE:
6A gates trees and names; behavior needs a companion differential oracle (the 6D null
hypothesis), and the two are complementary, not substitutes.

---

## What a prototype must test to move 6A

1. **Reach the TS limb.** Build `bbnf-path-ts` to `wasm32-unknown-unknown`, load in Node,
   run `makePathTag(compile_path,"Json")` over `path\`$.statuses[0].text\`` and the four
   failure paths, and compare — through the *actual* `ts/path.ts` + `serde-wasm-bindgen`
   transport — against canonical `bbnf::path` over the shared fixture. This is the real
   behavioral drive; today's native-shim test is a stand-in.
2. **Run `legit(B)` over `ts/path.ts`.** Implement the name-map linter as a `ts-morph`/AST
   pass that extracts TS type/interface/class names, joins them against T (from the Rust
   spine) and the closed `I` registry, and REDs any unbudgeted name. Prove: it reproduces
   the live count, REDs an injected `FooView`, REDs `BogusPayload`.
3. **Wire the LOCK-3 manifest seam.** Have `cargo xtask regen` emit `provenance.manifest`;
   have the 6A linter skip single-source names (`lex_path`, `PathSegment`) and gate only
   `authored` ones. Prove the linter goes GREEN on `lex_path` drift-immunity and RED on a
   dropped `PathErrorReason` variant in *any* of the three limbs.
4. **Confirm class closure.** Attempt to file a sixth idiom class; show it either collapses
   into I1-I5 or names a genuinely new divergence axis requiring registry review — evidence
   that `I = 5` is defended, not merely observed.
