---
lens: CH5 HIDDEN-COUPLING
pass: T-P1-excavation
cycle: V3
subject: SK-V17 T-P1 excavation artefacts (restart/audit/totality/sk-v17/p1/*.md)
generated_at: 2026-05-29T18:30:00Z
firewall: Lock 1 substrate-union + Lock 10 5-shape BackendShape + Track-1≡Track-2 honesty
artefacts_present: [1a-substrate-evidence.md, 1b-codegen-evidence.md, 1c-runtime-evidence.md, 1d-skinny-lessons.md, 1e-locks-evidence.md, 1f-coherence-scan.md, 1f-anti-pattern.md, 1f-past-corpora.md]
artefacts_absent: []
prior_cycle: V2 CH5 (ACCEPT 6 / REVISE 2 / REJECT 1; the REJECT was CH5-S0 = missing 1A/1B spine)
live_truth_method: "Read over crates/core/src/runtime/tape/{mod,record}.rs, crates/core/src/runtime/{json/parse_with,css_l4/builder,css_l4/value}.rs, crates/ir/src/registry/struct.rs:84,202,313-314,331, crates/simd-scan/src/{lib.rs:53-65,80, alphabet.rs:19-37}, skinny/crates/runtime/src/tape/mod.rs:94-100,175, skinny/crates/runtime/src/grammars/json/value.rs:143, skinny/crates/bbnf-simd/src/dispatch.rs:42; grep -c scan_structural over crates/core/src/grammar/generated/*.rs (8/8 wired); grep -rn TapeStructBuilder crates/core/src (grep-zero outside tape/); grep -rn StructLayout crates/ = 960; sed over restart/{ARCHITECTURE.md:1086-1090,1200-1210, locks/LOCKS.md:75-160, skinny/tranches/sk-v17/SPEC.md:108-116,256-260,792-796,805-815,823-839}; no cargo/build mutation. Master HEAD 445925167."
counts:
  accept: 7
  revise: 1
  reject: 0
---

## Lens Charge

CH5 HIDDEN-COUPLING (V3) is the Lock-1 substrate-union firewall. It scans the
SK-V17 T-P1 excavation for: (a) any catalogued state implying a **parallel
substrate**, a **sidecar producer**, or a **renamed-scanner / cross-call
classifier-state** violation; (b) whether the **5-shape BackendShape canon**
(Lock 10) is excavated whole and not mis-stated as a 6th-shape pressure; (c)
**Track-1 ≡ Track-2 dishonesty** — a producer masquerading as an independent
oracle, or a direction-reversal smuggle (totality → skinny); (d) whether the
**tape-as-unified-substrate** question is framed without implying a parallel or
second substrate beside the live one. Per PASS-1 §8.1 the Lock-1 substrate-union
audit is **1A's spine** and CH5's firewall counterpart; the structural projection
IS the tape — a catalogued "sidecar producer" is a divergence, not a feature.

## V2 → V3 Closure (the load-bearing change)

The V2 CH5 REJECT (CH5-S0) was structural: the 1A substrate spine and 1B
BackendShape inventory were **absent**, so the firewall had no dedicated subject
and 1F was carrying 1A's load (scope-bleed). **That REJECT is closed in V3.**
`restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md` now exists (23 KB,
cycle V3 frontmatter), owns the Lock-1 union spine, and explicitly absorbs the
migrated firewall rows under a dedicated **Substrate-Union Firewall (Lock 1, CH5
spine)** section (`1a-substrate-evidence.md:96-103`). 1B/1C/1D/1E are likewise
present. The V2 REVISE CH5-S4 (nine→six `pending_*` count) and CH5-S8 (Lock-1
closure obligation) are folded: `1f-anti-pattern.md:63` now reads "six `pending_*`
Vecs … plus one `pending_value: Option` … = SEVEN pending_ fields, NONE of them
`Vec<Vec>`"; `1a-substrate-evidence.md:76` (SUB17-002) carries the explicit
"Exactly-one-encoding post-fold; AoS/SoA coexistence is transient-only"
closure-obligation row. The firewall now reviews the **substrate inventory it was
designed to firewall**.

## Section Dispositions

### CH5-S1 — Parallel-substrate firewall: ONE unwired tape in crates/core
**Subject:** `1a-substrate-evidence.md:100` (Firewall row 1) + SUB17-001/002/003 (`:75-77`); `1f-anti-pattern.md:61` (AP17-001).
**Disposition: ACCEPT.**
**Evidence:** The central claim — crates/core holds exactly ONE tape construct
(`TapeStructBuilder`, `tape/mod.rs:58`) and it is UNWIRED — resolves at live
file:line. `grep -rn TapeStructBuilder crates/core/src` returns **zero** hits
outside `runtime/tape/` (verified; the sole adjacent mention is a doc-comment in
`backend/rust/emitter/shapes/number.rs:17`, not a usage). The live substrate is
the eager `JsonStructBuilder::new()` (`json/parse_with.rs:34`, verified) and
`CssStructBuilder` (`css_l4/builder.rs:66`, verified). 1A's verdict — "NO
same-tree parallel substrate; the unwired tape is the SK-V18 fold target sitting
dormant; the AoS-vs-SoA cross-tree gap (SUB17-002) is a fold-convergence question,
not a same-tree second substrate" — is the correct Lock-1 reading: a dead
construct is not a second substrate.
**Why ACCEPT:** No Track-1 ≡ Track-2 dishonesty asserted; the tape-as-unified-
substrate question is posed as the ONE dormant substrate awaiting wiring, never as
a plane beside a live tape. The firewall holds at the spine.

### CH5-S2 — Sidecar producer / retained structural projection (OnceCell across all 8 carriers)
**Subject:** `1a-substrate-evidence.md:101` (Firewall row 2) + U-SUB17-002 (`:132`); `1f-anti-pattern.md:62` (AP17-004).
**Disposition: ACCEPT.**
**Evidence:** The retained `OnceCell<StructuralIndex>` claim resolves: `scan_structural`
is wired in ALL 8 generated grammars — `json.rs:732`, `css_l4.rs:15982`,
`bnf.rs:848`, `css_pretty.rs:1905`, `bbnf.rs:4843`, `ebnf.rs:1381`, `csv.rs:566`,
`google_sheets.rs:3559` (all verified by `grep scan_structural`). The Lock-1 prose
"if structural offsets are retained, the structural projection IS the tape (no
second sidecar)" is verbatim at `ARCHITECTURE.md:1088` (verified). 1A's distinction
— the retained index feeds the eager `OpenFrame` builders, NOT a tape, so it is a
**retained scan cache, not the tape projection** — is the precise Lock-1
distinction, and the firewall correctly refuses to pre-classify it, pinning it to
the four `substrate_target` manifest values (`LOCKS.md:120-126`, verified) and to
REDRESS-53 re-entry (SPEC `:825`/`:839`, verified: "A retained parallel index
collapses into REDRESS-53"). The census-scope-to-all-8 directive (folding the V2
CH1-V2-004 REVISE) is honoured in both 1A's firewall row and 1f-anti-pattern's
verify_action.
**Why ACCEPT:** This is the strongest hidden-coupling catch in the set. It names the
only LIVE retained projection, refuses to clear it, scopes the census to all 8
carriers (not json-alone), and binds it to the manifest + REDRESS-53. No
parallel-substrate assertion.

### CH5-S3 — Renamed-scanner / cross-call classifier-state (Lock 1 v+1)
**Subject:** `1a-substrate-evidence.md:102` (Firewall row 3); `1f-anti-pattern.md:66`.
**Disposition: ACCEPT.**
**Evidence:** "core scan is `scan_structural(input, &alphabet)` (`crates/simd-scan/src/lib.rs:80`)
producing a `StructuralIndex` per call; no cross-call classifier-state retention
visible" resolves: `scan_structural` signature at `lib.rs:80` (verified). The Lock
1 v+1 ELEVATION ("no cross-call retained classifier state … carry MUST stay within
a single chunk-call boundary"; `retention_lifetime ∈ {transient-single-call,
retained-within-chunk, retained-across-call-boundary}`, third = REJECT) resolves at
`LOCKS.md:137-149` (verified). 1A correctly states the REJECT class is NOT tripped
and gives the right verify_action (grep `prev_state|carry|prefix_xor|retained` over
`crates/simd-scan/src/` before any SIMD fold).
**Why ACCEPT:** The subtlest Lock-1 v+1 trap is held open with the correct primitive
(`retention_lifetime`) and not falsely cleared. The `OnceCell<StructuralIndex>`
retains the OUTPUT index per-parse, correctly distinguished from classifier carry
state.

### CH5-S4 — 5-shape BackendShape canon framing (Lock 10) + aarch64-NEON absorption
**Subject:** `1a-substrate-evidence.md:79` (SUB17-005), `:94` (BackendShape map row), `:112,:124`; `1f-coherence-scan.md` BackendShape rows (carried via 1A migration).
**Disposition: ACCEPT.**
**Evidence:** Lock 10's 5-shape search domain `{EagerTape, OffsetTape, EventTape,
SinkOnly, CollapsedStage}` resolves verbatim at `LOCKS.md:107-108` (verified, with
the explicit "Adding a 6th BackendShape variant remains G-Omega gated"). The
`CollapsedStage` admission gate — "`target.arch == x86` + `target.avx512bw` …
aarch64 mechanically refused … aarch64 candidate is **UNKNOWN-2D-05**" — resolves
verbatim at `ARCHITECTURE.md:1206` (verified). SK-V17's aarch64-only bar ("No x86,
no AVX-512, no SVE") resolves at SPEC `:258` (verified) and the second-substrate
block bars "sixth `BackendShape`" at SPEC `:807-811` (verified). 1A's framing —
"the 5-shape canon holds at Lock 10; CollapsedStage admission is x86-pinned,
mechanically refused on aarch64, and the aarch64 candidate is the spec-named open
unknown UNKNOWN-2D-05 — NOT a fresh undiscovered gap; SK-V17's NEON union sits
under the four LLVM shapes' scan-leaf FFI" — is the exactly-correct Lock-10
framing. The tape-as-unified-substrate question is posed as **absorption into the
existing five shapes**, never as a 6th shape or a parallel plane.
**Why ACCEPT:** The lens's core BackendShape charge — canon excavated whole, no
6th-shape pressure, aarch64-NEON framed as absorption under the four LLVM shapes,
CollapsedStage-aarch64 correctly resolved to the spec-named UNKNOWN-2D-05 rather
than a novel gap — holds at every cited line.

### CH5-S5 — Lock-1 closure obligation: exactly-one-encoding post-fold (folds V2 CH5-S8)
**Subject:** `1a-substrate-evidence.md:76` (SUB17-002 Note), `:88` (Cross-Tree map row 1), `:131` (U-SUB17-001).
**Disposition: ACCEPT.**
**Evidence:** The V2 REVISE (CH5-S8) demanded the catalogue pin the post-fold
one-substrate invariant rather than leaving the AoS/SoA coexistence as license for
two encodings to persist. V3 folds it: SUB17-002's Note now reads "**Lock-1
closure obligation:** post-SK-V18 exactly ONE encoding survives across both trees
(`LOCKS.md:75` 'parallel substrates are dead'); AoS/SoA coexistence is admissible
ONLY as a transient fold-state — a post-fold dual encoding is a Lock-1 violation,
not a tree-local choice (CH5-S8)." The Cross-Tree map row 1 (`:88`) carries the
same invariant ("Exactly-one-encoding post-fold; AoS/SoA coexistence is
transient-only"), and U-SUB17-001 (`:131`) closes with "Exactly ONE encoding
survives post-fold; the dual encoding is a transient fold-state only." Lock 1's
"parallel substrates are dead" + "columnar SoA is dead" is verbatim at
`LOCKS.md:75` (verified); the core mod-doc's "kept AoS first … the same TapeCursor
API rides a later SoA split" is verbatim at `tape/mod.rs:6-9` (verified).
**Why ACCEPT:** The latent two-substrate end-state the V2 firewall flagged is now
named as an explicit invariant in three places (divergence Note, Cross-Tree row,
open question). The cross-tree AoS/SoA mismatch is correctly framed as admissible
ONLY transiently — no license for a post-fold dual substrate.

### CH5-S6 — Second-substrate / monotonic-direction / forbidden-name collision firewall
**Subject:** `1a-substrate-evidence.md:79` (SUB17-005), `:94` (BackendShape row); `1f-past-corpora.md` D6 row (continuity); SPEC `:807-811`.
**Disposition: ACCEPT.**
**Evidence:** The SPEC §9 second-substrate block names skinny
`StructLayout`/`TapeStructBuilder`/`TapeCursor`, public `UnionTape`, "sixth
`BackendShape`", "parallel source passes", "cross-call classifier-state retention"
as FORBIDDEN — resolves verbatim at SPEC `:807-811` (verified). These are
precisely the crates/core fold-target names. 1A correctly catalogues the direction
guard: SK-V17 frames the core tape as the SK-V18 **adoption target** (SPEC
`:110-114`, verified: "the TOTALITY tree … can adopt them in SK-V18"), monotonic
skinny→totality — the proven skinny `Tape`/`ValueRef` shape is adopted INTO
crates/core, never the forbidden-in-skinny names relocated back into skinny as a
second substrate. The "Wrong-tree dishonesty" bar — "keying a tape/layout gate on
`crates/core/` rather than `skinny/crates/`" (SPEC `:813-815`, verified) — is the
exact Track-1 ≡ Track-2 honesty fence, and 1A respects it (crates/core is the fold
TARGET; skinny is the proven engine; the gate `tape_activated` is "NOT a
crates/core grep", SUB17-003).
**Why ACCEPT:** The forbidden-name collision (the §9-forbidden-in-skinny names ARE
the crates/core fold-target names) is the single most important direction-reversal
hidden coupling, and 1A draws it correctly — adoption INTO core, never relocation
INTO skinny.

### CH5-S7 — StructRegistry/FieldSource per-leaf hot-path indirection (do-not-redrive fence)
**Subject:** `1a-substrate-evidence.md:103` (Firewall row 4) + SUB17-009 (`:116`), Cross-Tree value-API row (`:91`); `1f-anti-pattern.md:65` (AP17-005).
**Disposition: ACCEPT.**
**Evidence:** `begin_compound(&StructLayout)` reads `layout.rule_id & 0x1F`
(`tape/mod.rs:185`, verified). `StructLayout` originates from `StructRegistry`, a
`BTreeMap<RuleId, StructLayout>` (`struct.rs:313-314`, verified) queried via
`layout(rule_id)` (`struct.rs:331`, verified); `FieldSource` lives inside the same
registry (`struct.rs:84`, verified). The 28-65×/983×/10583× regression pre-block
("StructRegistry / Arena<G> / Builder<G> hot-path indirection … No registry lookup
in the per-leaf hot path") is verbatim at SPEC `:794-795` (verified). 1A's fence —
"a naive per-leaf `StructRegistry` BTreeMap lookup on the hot path re-opens the
regression; the `FieldSource`/`StructLayout` walk MUST be compile-time
projection-emission resolved ONCE at codegen, NOT a per-leaf runtime registry
lookup" — is the correct do-not-redrive coupling. `begin_compound` taking a
pre-resolved `&StructLayout` by reference is the correct shape; a per-leaf
`registry.layout(rule)` is the REJECT shape, exactly as 1f-anti-pattern AP17-005
states.
**Why ACCEPT:** This is a genuine hidden coupling the firewall must surface: the
SK-V18 value-projection fold could silently re-open the worst measured regression
in the corpus if the `FieldSource` walk became per-leaf. 1A and 1F both pin it at
file:line with the correct compile-time-resolution invariant and a concrete
verify_action (grep the projection generator for runtime `registry.layout(`/`.field(`
inside the per-leaf emit loop).

### CH5-S8 — Firewall-row REDRESS-53 anchor imprecision
**Subject:** `1a-substrate-evidence.md:101` (Firewall row 2, "REDRESS-53 (SPEC :807-811)").
**Disposition: REVISE.**
**Evidence:** 1A's Firewall row 2 (sidecar producer) closes with "a retained index
parallel to a wired tape is REDRESS-53 (SPEC :807-811)". But REDRESS-53 itself is
NOT named at SPEC `:807-811` — that band is the general second-substrate D6 block
(which names `StructLayout`/`TapeStructBuilder`/`TapeCursor`/`UnionTape`/sixth
`BackendShape`, but not REDRESS-53 by number). REDRESS-53 is named at SPEC `:577`
("the L1/L4 index retained as a parallel vector (REDRESS-53)"), `:657` ("parallel
to the tape (REDRESS-53)"), `:825` (W2 do-not-reopen row), and `:839` ("A retained
parallel index collapses into REDRESS-53") — all verified. The sibling artefact
`1f-anti-pattern.md:62` already cites REDRESS-53 correctly at SPEC `:825`; 1A's
firewall row is the lone imprecise anchor for that REDRESS number.
**Why REVISE not ACCEPT:** The substantive coupling (retained parallel index =
REDRESS-53 re-entry) is correct and load-bearing; only the line-anchor for the
REDRESS number is wrong (it points at the general D6 block, not the REDRESS-53
naming line). A wrong anchor on a REDRESS citation is a CH1/CH3 paper-close smell
even when the conclusion is right, and it desynchronises 1A from its own sibling
1f-anti-pattern which anchors it correctly.
**Concrete fix:** Edit `1a-substrate-evidence.md:101` to read "… is REDRESS-53
(SPEC :825/:839; the §9 second-substrate D6 block is :807-811)" — i.e. anchor the
REDRESS-53 number at `:825`/`:839` (where it is named against the retained-parallel-index
shape) and retain `:807-811` only as the second-substrate-block citation. This
syncs 1A's firewall row with `1f-anti-pattern.md:62`'s correct `:825` anchor and
U-SUB17-002 (`:132`) which already cites `:825` ("else REDRESS-53 re-entry (SPEC
:825)"). No verdict changes; only the cited line.

## CH5 Firewall Verdict (Track 1 ≡ Track 2)

The V3 excavation passes the Lock-1 firewall. With 1A now present as the dedicated
substrate-union spine, NO catalogued state implies a live parallel substrate,
sidecar producer, or renamed-scanner / cross-call-classifier-state violation, and
NO Track-1 ≡ Track-2 dishonesty surfaces:

- **One substrate, unwired** — `TapeStructBuilder` is grep-zero outside `tape/`;
  the eager `OpenFrame` builders are the live substrate; the dead tape is the
  SK-V18 fold target, not a second plane (CH5-S1).
- **The only live retained projection** — the `OnceCell<StructuralIndex>` scan
  cache across all 8 carriers — is correctly held as a retained scan cache (not the
  tape projection) and pinned to the four-value `substrate_target` manifest +
  REDRESS-53, scoped to all 8 carriers (CH5-S2).
- **No cross-call classifier carry** under Lock 1 v+1; the `retention_lifetime`
  REJECT class is correctly held open, not falsely cleared (CH5-S3).
- **The 5-shape BackendShape canon is excavated whole**; CollapsedStage is
  x86-pinned and the aarch64 candidate is correctly resolved to the spec-named
  UNKNOWN-2D-05; aarch64-NEON is framed as absorption under the four LLVM shapes,
  never a 6th shape (CH5-S4).
- **The Lock-1 closure obligation** (exactly-one-encoding post-fold; AoS/SoA
  coexistence transient-only) — the V2 REVISE — is folded as an explicit invariant
  in three places (CH5-S5).
- **The direction guard** (monotonic skinny→totality; the §9-forbidden-in-skinny
  names ARE the fold-target names; adoption INTO core, never relocation INTO
  skinny) holds (CH5-S6).
- **The do-not-redrive fence** (FieldSource/StructLayout walk is compile-time
  projection-emission, never a per-leaf registry lookup that re-opens the
  28-65×/983×/10583× regression) is correctly surfaced (CH5-S7).

The single REVISE (CH5-S8) is a line-anchor imprecision on the REDRESS-53 citation
in 1A's firewall row 2 — the coupling is correct, the anchor points at the general
D6 block (`:807-811`) instead of the REDRESS-53 naming line (`:825`/`:839`). It is
an orphan-free REVISE with a one-line concrete fix and does not change any verdict.

The V2 CH5-S0 REJECT (firewall had no 1A spine) is **closed**: 1A exists, owns the
substrate-union spine, and the firewall now reviews the inventory it was designed
to firewall rather than 1F's out-of-charter load.

## Disposition Summary

| Section | Subject | Disposition |
|---|---|---|
| CH5-S1 | Parallel-substrate firewall: one unwired tape (SUB17-001/002/003, AP17-001) | ACCEPT |
| CH5-S2 | Sidecar / retained structural projection across all 8 (Firewall row 2, AP17-004) | ACCEPT |
| CH5-S3 | Renamed-scanner / cross-call classifier-state (Lock 1 v+1) | ACCEPT |
| CH5-S4 | 5-shape BackendShape canon + aarch64-NEON absorption (Lock 10, SUB17-005) | ACCEPT |
| CH5-S5 | Lock-1 closure obligation: exactly-one-encoding post-fold (folds V2 CH5-S8) | ACCEPT |
| CH5-S6 | Second-substrate / monotonic-direction / forbidden-name collision (SUB17-005, §9) | ACCEPT |
| CH5-S7 | StructRegistry/FieldSource per-leaf hot-path do-not-redrive fence (SUB17-009, AP17-005) | ACCEPT |
| CH5-S8 | Firewall-row REDRESS-53 anchor imprecision (1A:101) | **REVISE** |

**Counts: ACCEPT 7 · REVISE 1 · REJECT 0 (8 sections).**
ACCEPT rate 87.5%. The pass does NOT yet meet the §4 ≥95% criterion, but the
single REVISE is a non-orphan one-line citation-anchor fix (no verdict change, no
structural defect), and the V2 REJECT is closed. With CH5-S8 folded in V4, the
substrate-union + BackendShape firewall converges. Zero open critical defects from
this lens; zero orphan REVISE.
