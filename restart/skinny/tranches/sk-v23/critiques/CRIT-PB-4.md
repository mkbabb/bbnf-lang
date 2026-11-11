# CRIT-PB-4 — Critique of the type-plane greenfield skeleton (PROTO-PB-4)

**Tranche:** SK-V23 / TB4.W1. **Date:** 2026-07-10. **Disposition:** adversarial
review of PROTO-PB-4 and the codex section it tests (SPEC-DRAFT §1 type-plane row,
§ TB4.W1). **Convergence returned: 70%.** No cargo run; verification by grep/read
over the live trees plus the cited sk-v21 research/28 §6.

## What the prototype got right (independently verified)

- **Arrow counts are exact.** `grep -o '\->'` = **5** on `grammar/json/json.bbnf`,
  **13** on `grammar/css/l4/easing.bbnf`. Matches the instrument.
- **The frozen split model is real.** `skinny/crates/grammar/src/lib.rs:1754-1755`
  does assert `Projection=1, TypedProjection=1` — but over the inline `css-lite.bbnf`
  fixture `tag = "from" -> 0u8 | "paint" -> crate::paint(input): u32 ;` (lib.rs:1731),
  NOT json. `:2044-2060` does pin the `parse_hex_color(input) : u32`
  `RuntimeTypedProjection`. Both citations accurate.
- **There is no type system to carry.** grep for `TypeDesc`/`join_types`/`infer` across
  `skinny/crates` hits only `grammar/src/lib.rs` (the `type_text: String` byte-slice at
  `:200`). Greenfield sizing is honest.
- **The two named surfaces ARE §6's two open-ended surfaces** (research/28:72-82:
  `->`/`@host` value exprs; grammar-closure generics). The CSP→worklist demotion
  aligns with §6:402 and the user's changed-flag precept. Fair.
- **This is a compile+count instrument, not a perf probe.** It says so; no PMU/loadavg
  claim is made or needed. No false cold/warm framing. Correct method labeling.

So the mechanical spine is sound. The failure is the **leap from "scalar arrows type
cleanly" to "the greenfield type plane is well-founded / the HM core reduces to two
dormant seams."** That leap does not follow, for four reasons.

## Finding 1 (BLOCKING) — the instrument is arrow-scoped and never types the
structural/join core, which the SAME corpus exercises on day one

The instrument folds only over `->` targets. But the actual type system on the SAME
`json` grammar computes, per the frozen generated registry
`crates/core/src/grammar/generated/json.rs`:

- `object` → `Vec(...)` (`:341`), `array` → `Vec(...)` (`:356`), `pair` → `Tuple(...)`
  (`:371`),
- `value` → **`HeterogeneousAltJoin([BoxedEnum, Span, Bool, F64])`** (`:392`).

None of these carry a `->`. None appear in the instrument's histogram
(`U8×1 Bool×2 F64×1 Named×1`). The prototype's histogram is the **arrow-target**
picture, not the **type** picture of json. The hardest type json produces — the
`value` heterogeneous alternation join — is invisible to the falsifier.

`easing.bbnf` is worse: its own header TODO (`grammar/css/l4/easing.bbnf:6-9`)
documents the exact obligations skipped — *"cubic-bezier carries (f64×4); steps carries
(i64, u8 step_position); linearEasing is a variable-length list of (f64, f64?)"* — plus
`easingFunction = cubicBezier | steps | linearEasing | easingKeyword` (`:29`), a
four-way heterogeneous alt. The instrument reports `U8×13` — it typed only the 13
keyword→u8 arrows and skipped every aggregate the grammar itself flags as *not done*.
Both test grammars were (whether by chance or selection) the two that let an arrow-only
scan report PASS while the aggregate core sits untouched.

**Consequence:** the invariant `arrow-count == projection-count` (5==5, 13==13) is TRUE
but weak. It certifies that annotated scalar arrows reach a `TypeDesc`; it says nothing
about the un-annotated structural composition where §6 spends its entire length.

## Finding 2 (BLOCKING) — the prototype's "land 1/3/5, gate 2/4/6" INVERTS §6's own
load-bearing / frontier boundary, then claims fidelity

The bottom line asserts the disposition is *"exactly as sk-v21 §6 dispositions"* and
maps steps 1/3/5 = implemented-structural, 2/4 = open surfaces (gated), 6 = deferred.
§6's actual build order (research/28:400-405) is:

> (1) `MapExpr::synth`; (2) `HostSigTable` + first-order unifier; (3)
> `TypeDesc::Record`/`Sum` during projection, **retiring `HeterogeneousAltJoin`-as-type**;
> (4) rank-1 schemes; (5) re-home the fold onto a worklist; (6) Salsa.
> **Steps 1–3 are the load-bearing honesty-and-capability fixes; 4–6 are the
> power-and-speed frontier.**

§6 mandates **1-2-3** (including host-sig unify AND Record/Sum) and defers **4-5-6**.
The prototype regroups to implement 1/3/5 and gate 2/4/6 — **demoting §6's load-bearing
step 2 (host-sig unify) into the deferred bucket, and stubbing §6's load-bearing step 3
(Record/Sum) to "identity on corpus."** That is not §6's disposition; it inverts it.
The claim `step3_resolve_rows — identity on corpus` and the caveat *"§6 step 3 has no
corpus witness"* are both false: json `value` (`json.rs:392`) and easing `easingFunction`
are the witnesses, and §6:343,400 names step 3's job as resolving exactly them.

If TB4.W1 inherits the prototype's grouping, it defers two of §6's three load-bearing
fixes and mislabels the third as done.

## Finding 3 (BLOCKING) — corpus mislocation: the falsifier is vacuous on the
graduated tree

`skinny/grammars/` — the graduated corpus — contains ONE grammar, `json.bbnf`, with
**zero** `->` (verified: `grep -o` = 0; the graduated json is a plain recognizer). Every
arrow-bearing grammar the prototype tested lives in the legacy `grammar/` tree, which
§2.1 slates for `git rm` at TB1. So the codex close-gate *"every `->` in every corpus
grammar reaches the tape emitter"* is **vacuously true on today's graduated corpus** and
was validated only against archival grammars. TB4.W1 has an unsequenced prerequisite:
the arrow-bearing corpus must be migrated into `skinny/grammars/` (re-acquiring the `->`
projections graduation stripped) before the falsifier has anything to falsify. Neither
TB4.W0 nor §1 sequences this. The prototype does not disclose that its two grammars are
archival, not graduated.

## Finding 4 (refinement) — the "never-lose-a-type" oracle is the weak one; a complete
oracle exists unused

The subset claim diffs against the frozen `RuntimeTypedProjection` set, which threads a
type **only** where a `: Type` ascription is present (skinny grammar `lib.rs:192-201`).
That oracle already drops every structural type (Vec/Tuple/Sum). "Frozen ⊆ derived,
never a loss" is therefore satisfiable while silently losing all of json's aggregate
types — the opposite of the invariant's intent. The **complete** oracle is in the same
tree: `crates/core/src/grammar/generated/*.registry.json` carries the full per-rule
`rule_type` `TypeDesc` (json's `value`/`object`/`array`/`pair` included). TB4.W1's
"never lose a type" gate should be per-rule registry equivalence, not arrow-only
projection subset.

## Secondary imprecisions

- **TypeDesc "mirrors `crates/ir/src/types/type_desc.rs` (same scalar set)"** — true for
  scalars only. The legacy enum carries `BoxedEnum`, `Enum`, and
  `HeterogeneousAltJoin(Vec<TypeDesc>)` (`type_desc.rs:44-46,63`); the prototype omits all
  three and substitutes `Record`/`Sum` (§6's target). Defensible as a forward port, but
  "mirror" overstates it, and the reconciliation (legacy join obligation → typed Sum) is
  unmentioned work.
- **`runtime/bbnf/view.rs:141`** cited as the "width-destruction root-cause" is a bare
  `_ => None` fallthrough inside a min/max range helper — an imprecise anchor.
- **"cold run"** label on a compile+count instrument (line 27) is meaningless (a count
  has no thermal state); harmless but sloppy.

## Does the verdict follow?

Partially. Criteria 1 (compiles) and the mechanical half of 2 (5==5, 13==13) hold.
Criterion 3 ("zero frozen projections lost") holds only against the arrow-only oracle and
is misleading against the complete registry. Criterion 4 ("two surfaces … HM core
quarantined") does **not** follow: step-3 structural/join resolution is a live,
non-identity obligation on both test grammars, so the corpus exercises a third inference
locus the instrument declared dormant. The headline *"the greenfield type plane is
well-founded"* overreaches — the instrument validated the easy scalar-arrow fraction and
mislabeled §6's load-bearing structural core as witness-free.

## Convergence: 70%

The ARCHITECTURE (greenfield, §6 order, `TypeDesc` with Record/Sum) is directionally
sound and the greenfield sizing is honest. The EVIDENCE is thin and its cited
disposition inverts §6. To reach executable-tomorrow, TB4.W1 must (a) broaden the
falsifier to per-rule TypeDesc/registry equivalence, not arrow-only reach; (b) keep §6
steps 1-2-3 (host-sig unify + Record/Sum) load-bearing, not deferred; (c) sequence the
arrow-bearing corpus migration into skinny before the falsifier can bite. All three are
tranche-addressable; none is fatal — hence 70, not lower.
