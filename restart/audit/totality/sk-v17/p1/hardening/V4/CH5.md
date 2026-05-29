---
lens: CH5 HIDDEN-COUPLING
pass: T-P1-excavation
cycle: V4
subject: SK-V17 T-P1 excavation artefacts (restart/audit/totality/sk-v17/p1/*.md)
generated_at: 2026-05-29T00:00:00Z
firewall: Lock 1 substrate-union + Lock 10 5-shape BackendShape + Track-1≡Track-2 honesty
artefacts_present: [1a-substrate-evidence.md, 1b-codegen-evidence.md, 1c-runtime-evidence.md, 1d-skinny-lessons.md, 1e-locks-evidence.md, 1f-coherence-scan.md, 1f-anti-pattern.md, 1f-past-corpora.md]
artefacts_absent: []
prior_cycle: V3 CH5 (ACCEPT 7 / REVISE 1 / REJECT 0; the REVISE was CH5-S8 = 1a:101 REDRESS-53 anchor at the D6 block :807-811 instead of the naming line :825/:839)
live_truth_method: "Read over crates/core/src/runtime/tape/{mod,record}.rs (mod.rs:6-9/55-58/185, record.rs:103/120/121), crates/ir/src/registry/struct.rs:84/202/313-314/331, crates/simd-scan/src/lib.rs:80; grep -c scan_structural over crates/core/src/grammar/generated/*.rs (8/8 wired; math.rs=0, mod.rs=0); grep -rn TapeStructBuilder crates/core/src | grep -v runtime/tape/ (grep-zero); grep -rn StructLayout crates/ = 960; nl -ba over restart/skinny/tranches/sk-v17/SPEC.md:806/807-811/823-841 + grep REDRESS-53 (named :577/:657/:825/:839), restart/locks/LOCKS.md:75/107-108/119-126/137-149/160, restart/ARCHITECTURE.md:1088/1206; cross-read 1a:101-106/132-135, 1e:101/109/111/119/122/179, 1f-anti-pattern.md:61-64/85; no cargo/build mutation. Master HEAD 445925167154de73540e3ea3283d0170371de790 (re-confirmed via git rev-parse)."
counts:
  accept: 7
  revise: 1
  reject: 0
---

## Lens Charge

CH5 HIDDEN-COUPLING (V4) is the Lock-1 substrate-union firewall over the SK-V17
T-P1 excavation. It scans for: (a) catalogued state implying a **parallel
substrate**, a **sidecar producer**, or a **renamed-scanner / cross-call
classifier-state** violation; (b) whether the **5-shape BackendShape canon** (Lock
10) is excavated whole and not mis-stated as 6th-shape pressure; (c) **Track-1 ≡
Track-2 dishonesty** — a producer masquerading as an independent oracle, or a
direction-reversal smuggle (totality → skinny); (d) whether the
**tape-as-unified-substrate** question is framed without implying a parallel or
second substrate beside the live one. Per PASS-1 §8.1 the Lock-1 union audit is
1A's spine and CH5's firewall counterpart; the structural projection IS the tape —
a catalogued "sidecar producer" is a divergence, not a feature.

## V3 → V4 Closure (the load-bearing change)

The sole V3 REVISE was **CH5-S8**: 1A's Sidecar-producer firewall row anchored the
REDRESS-53 *number* at the §9 second-substrate D6 block (`:807-811`) rather than at
the line where REDRESS-53 is named against the retained-parallel-index shape
(`:825`/`:839`), desynchronising 1A from its sibling `1f-anti-pattern.md:62` which
already anchored `:825`. **That REVISE is closed in V4 and live-verified.**
`1a-substrate-evidence.md:104` now reads "… a retained index parallel to a wired
tape is REDRESS-53, named at SPEC `:825` (W2 row …) + `:839` (… A retained parallel
index collapses into REDRESS-53); the general second-substrate block is the
separate `:807-811`." The V4 frontmatter records the fold
(`1a-substrate-evidence.md:19` `CH5-S8-V4-REDRESS-53-anchor-at-825-839`) and
`:66-72` carries the closure prose. I independently re-verified the SPEC anchors:
REDRESS-53 is named at `:577`, `:657`, `:825`, `:839` (`grep REDRESS-53`); `:806`
is the x86/AVX-512/SVE bar; `:807-811` is the general second-substrate block. 1A's
new anchor is correct on every line. The desync with `1f-anti-pattern.md:62/85`
(both `:825`) is resolved.

A second secondary anchor (CH3-R4) folded the same cycle: SUB17-005 (`:82`) now
cites the x86 bar at `:806` (verified) and the second-substrate block at the
separate `:807-811` (verified) — distinct, both correct.

## Section Dispositions

### CH5-S1 — Parallel-substrate firewall: ONE unwired tape in crates/core
**Subject:** `1a-substrate-evidence.md:103` (Firewall row 1) + SUB17-001/002/003 (`:78-80`); `1f-anti-pattern.md:61` (AP17-001).
**Disposition: ACCEPT.**
**Evidence:** The central claim — crates/core holds exactly ONE tape construct
(`TapeStructBuilder`, `tape/mod.rs:58`, verified) and it is UNWIRED — resolves at
live file:line. `grep -rn TapeStructBuilder crates/core/src | grep -v runtime/tape/`
returns **zero** hits (verified this cycle). The live substrate is the eager
`JsonStructBuilder` (`json/builder.rs:9`) and `CssStructBuilder` (`css_l4/builder.rs:16`).
The 16-byte AoS `TapeRec` is const-asserted (`record.rs:103,120,121` —
`size_of==16`, `align_of==4`, both verified), and the AoS-first provisionality is
documented verbatim at `tape/mod.rs:6-9` ("kept AoS first … the same TapeCursor API
rides a later SoA split", verified). 1A's verdict — "NO same-tree parallel
substrate; the unwired tape is the SK-V18 fold target sitting dormant; the
AoS-vs-SoA cross-tree gap (SUB17-002) is a fold-convergence question, not a
same-tree second substrate" — is the correct Lock-1 reading: a dead construct is
not a second substrate.
**Why ACCEPT:** No Track-1 ≡ Track-2 dishonesty asserted; the
tape-as-unified-substrate question is posed as the ONE dormant substrate awaiting
wiring, never as a plane beside a live tape. The firewall holds at the spine.

### CH5-S2 — Sidecar producer / retained structural projection (OnceCell across all 8 carriers) + V3 REVISE closure
**Subject:** `1a-substrate-evidence.md:104` (Firewall row 2) + U-SUB17-002 (`:135`); `1f-anti-pattern.md:62` (AP17-004).
**Disposition: ACCEPT.**
**Evidence:** The retained `OnceCell<StructuralIndex>` claim resolves:
`scan_structural` is wired in ALL 8 generated grammars (`grep -c scan_structural`
returns 1 for each of `bbnf.rs`, `bnf.rs`, `css_l4.rs`, `css_pretty.rs`, `csv.rs`,
`ebnf.rs`, `google_sheets.rs`, `json.rs`; and 0 for `math.rs`/`mod.rs` — the
non-carriers, correctly noted, verified this cycle). The Lock-1 prose "if structural
offsets are retained, the structural projection IS the tape (no second sidecar)" is
verbatim at `ARCHITECTURE.md:1088` (verified). 1A's distinction — the retained index
feeds the eager `OpenFrame` builders, NOT a tape, so it is a **retained scan cache,
not the tape projection** — is the precise Lock-1 distinction, and the firewall
correctly refuses to pre-classify it, pinning it to the four `substrate_target`
manifest values (`LOCKS.md:119-126`, verified) and to REDRESS-53 re-entry. **The V3
CH5-S8 REVISE is folded here:** the firewall row now anchors REDRESS-53 at `:825`
(W2 row) + `:839` (shortlist condition 1) — both verified to name REDRESS-53
against the retained-parallel-index shape — and retains `:807-811` only as the
general second-substrate-block cite. This syncs 1A with `1f-anti-pattern.md:62`
(`:825`) and U-SUB17-002 (`:135`, `:825`).
**Why ACCEPT:** This is the strongest hidden-coupling catch in the set, and the V3
anchor desync is now resolved at the correct naming line. It names the only LIVE
retained projection, refuses to clear it, scopes the census to all 8 carriers, and
binds it to the manifest + REDRESS-53 at the right anchors. No parallel-substrate
assertion.

### CH5-S3 — Renamed-scanner / cross-call classifier-state (Lock 1 v+1)
**Subject:** `1a-substrate-evidence.md:105` (Firewall row 3); `1e-locks-evidence.md:138`.
**Disposition: ACCEPT.**
**Evidence:** "core scan is `scan_structural(input, &alphabet)` producing a
`StructuralIndex` per call; no cross-call classifier-state retention visible"
resolves: the signature `pub fn scan_structural(input: &[u8], alphabet:
&StructuralAlphabet) -> StructuralIndex` is at `crates/simd-scan/src/lib.rs:80`
(verified this cycle — the function takes input + alphabet by reference and returns
a fresh index, no `&mut self` carry). The Lock 1 v+1 ELEVATION ("no cross-call
retained classifier state … Carry MUST stay within a single chunk-call boundary";
`retention_lifetime ∈ {transient-single-call, retained-within-chunk,
retained-across-call-boundary}`, third = REJECT) resolves verbatim at
`LOCKS.md:137-149` (verified). 1A correctly states the REJECT class is NOT tripped
and gives the right verify_action (grep `prev_state|carry|prefix_xor|retained` over
`crates/simd-scan/src/` before any SIMD fold). 1e:138 carries the identical reading
("the OUTPUT index per-parse, NOT classifier carry state").
**Why ACCEPT:** The subtlest Lock-1 v+1 trap is held open with the correct primitive
(`retention_lifetime`) and not falsely cleared. The `OnceCell<StructuralIndex>`
retains the OUTPUT index per-parse, correctly distinguished from classifier carry
state. 1A's frontmatter records this V3-ACCEPT row was re-verified live this cycle
(`:15`).

### CH5-S4 — 5-shape BackendShape canon framing (Lock 10) + aarch64-NEON absorption
**Subject:** `1a-substrate-evidence.md:82` (SUB17-005), `:97` (BackendShape map row), `:127`; `1e-locks-evidence.md:101,111,122,171`.
**Disposition: ACCEPT.**
**Evidence:** Lock 10's 5-shape search domain `{EagerTape, OffsetTape, EventTape,
SinkOnly, CollapsedStage}` resolves verbatim at `LOCKS.md:107-108` (verified, with
the explicit "Adding a 6th `BackendShape` variant remains G-Omega gated"). The
`CollapsedStage` admission gate — "`target.arch == x86` + `target.avx512bw` …
aarch64 mechanically refused … aarch64 candidate is UNKNOWN-2D-05" — resolves
verbatim at `ARCHITECTURE.md:1206` (verified, with "NOT-ADMITTED: x86-only").
ARCH:1088 also carries the canonical 5-shape description (CollapsedStage "fuses
mask-state and emission for AVX-512-class hardware", verified). SK-V17's
aarch64-only bar resolves at SPEC `:806` ("x86 / AVX-512 / SVE: aarch64 only",
verified) and `:258`; the second-substrate block bars "sixth `BackendShape`" at SPEC
`:808` (verified). 1A's framing — "the 5-shape canon holds at Lock 10; CollapsedStage
admission is x86-pinned, mechanically refused on aarch64, and the aarch64 candidate
is the spec-named open unknown UNKNOWN-2D-05 — NOT a fresh undiscovered gap; SK-V17's
NEON union sits under the four LLVM shapes' scan-leaf FFI" — is the exactly-correct
Lock-10 framing, and is mirrored consistently in 1e:101/111/122 (verified). The
tape-as-unified-substrate question is posed as **absorption into the existing five
shapes**, never as a 6th shape or a parallel plane.
**Why ACCEPT:** The lens's core BackendShape charge — canon excavated whole, no
6th-shape pressure, aarch64-NEON framed as absorption under the four LLVM shapes,
CollapsedStage-aarch64 correctly resolved to the spec-named UNKNOWN-2D-05 rather
than a novel gap — holds at every cited line, and 1A↔1E agree.

### CH5-S5 — Lock-1 closure obligation: exactly-one-encoding post-fold (continuity of V2 CH5-S8)
**Subject:** `1a-substrate-evidence.md:79` (SUB17-002 Note), `:91` (Cross-Tree map row 1), `:134` (U-SUB17-001); `1e-locks-evidence.md:109,167`.
**Disposition: ACCEPT.**
**Evidence:** The exactly-one-encoding-post-fold invariant — folded in V3 from the
V2 CH5-S8 REVISE — is intact in V4: SUB17-002's Note (`:79`) reads "**Lock-1
closure obligation:** post-SK-V18 exactly ONE encoding survives across both trees
(`LOCKS.md:75` 'parallel substrates are dead'); AoS/SoA coexistence is admissible
ONLY as a transient fold-state — a post-fold dual encoding is a Lock-1 violation, not
a tree-local choice (CH5-S8)." The Cross-Tree map row 1 (`:91`) carries the same
invariant; U-SUB17-001 (`:134`) closes with "Exactly ONE encoding survives
post-fold; the dual encoding is a transient fold-state only." Lock 1's "columnar SoA
is dead … parallel substrates are dead" is verbatim at `LOCKS.md:75` (verified); the
core mod-doc's "kept AoS first … the same TapeCursor API rides a later SoA split" is
verbatim at `tape/mod.rs:6-9` (verified). 1e:109 ("the post-fold closure obligation
is exactly-one-encoding, not a dual end-state") and 1e:167 (LAC-1E-SKV17-01) carry
the same invariant.
**Why ACCEPT:** The latent two-substrate end-state is named as an explicit invariant
in three places in 1A and re-stated in 1E; the cross-tree AoS/SoA mismatch is
correctly framed as admissible ONLY transiently — no license for a post-fold dual
substrate, no new substrate proposed.

### CH5-S6 — Second-substrate / monotonic-direction / forbidden-name collision firewall
**Subject:** `1a-substrate-evidence.md:82` (SUB17-005), `:97` (BackendShape row); SPEC `:807-811`, `:814-815`, `:110-114`.
**Disposition: ACCEPT.**
**Evidence:** The SPEC §9 second-substrate block names skinny
`StructLayout`/`TapeStructBuilder`/`TapeCursor`, public `UnionTape`, "sixth
`BackendShape`", "parallel source passes", "cross-call classifier-state retention"
as FORBIDDEN — resolves verbatim at SPEC `:807-811` (verified). These are precisely
the crates/core fold-target names. 1A correctly catalogues the direction guard:
SK-V17 frames the core tape as the SK-V18 **adoption target** (SPEC `:110-114`,
verified: "the TOTALITY tree (`crates/core/src/runtime/tape/`) can adopt them in
SK-V18"), monotonic skinny→totality — the proven skinny `Tape`/`ValueRef` shape is
adopted INTO crates/core, never the forbidden-in-skinny names relocated back into
skinny as a second substrate. The "Wrong-tree dishonesty" bar — "keying a
tape/layout gate on `crates/core/` rather than `skinny/crates/`" — resolves verbatim
at SPEC `:814-815` (verified), and 1A respects it (crates/core is the fold TARGET;
skinny is the proven engine; the gate `tape_activated` is "NOT a crates/core grep",
SUB17-003 `:80`).
**Why ACCEPT:** The forbidden-name collision (the §9-forbidden-in-skinny names ARE
the crates/core fold-target names) is the single most important direction-reversal
hidden coupling, and 1A draws it correctly — adoption INTO core, never relocation
INTO skinny. No Track-1 ≡ Track-2 dishonesty.

### CH5-S7 — StructRegistry/FieldSource per-leaf hot-path indirection (do-not-redrive fence)
**Subject:** `1a-substrate-evidence.md:106` (Firewall row 4) + SUB17-009 (`:119`), Cross-Tree value-API row (`:94`); `1f-anti-pattern.md:64` (grammar-name leak / data-binding).
**Disposition: ACCEPT.**
**Evidence:** `begin_compound(&StructLayout)` is at `tape/mod.rs:185` (verified —
`fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle`, taking a
pre-resolved `&StructLayout` by reference, the correct shape). `StructLayout`
originates from `StructRegistry`, a `BTreeMap<RuleId, StructLayout>`
(`struct.rs:313-314`, verified) queried via `layout(rule_id) -> Option<&StructLayout>`
(`struct.rs:331`, verified); `FieldSource` is the same registry's enum
(`struct.rs:84`, verified `pub enum FieldSource`). 1A's fence — "a naive per-leaf
`StructRegistry` BTreeMap lookup on the hot path re-opens the 28-65×/983×/10583×
regression; the `FieldSource`/`StructLayout` walk MUST be compile-time
projection-emission … NOT a per-leaf runtime registry lookup" — is the correct
do-not-redrive coupling. `begin_compound` taking a pre-resolved `&StructLayout` by
reference is the correct shape; a per-leaf `registry.layout(rule)` is the REJECT
shape.
**Why ACCEPT:** A genuine hidden coupling the firewall must surface: the SK-V18
value-projection fold could silently re-open the worst measured regression in the
corpus if the `FieldSource` walk became per-leaf. 1A pins it at file:line with the
correct compile-time-resolution invariant and a concrete verify_action. The tape
"dispatches on the StructLayout … never on per-grammar route strings"
(`tape/mod.rs:54-56`, verified) — Lock-14-clean, no grammar-name branch.

### CH5-S8 — 1E REDRESS-53 re-entry anchor off-by-one (SPEC :578 → :577)
**Subject:** `1e-locks-evidence.md:179` (Open Question 1E-SKV17-U2, "else REDRESS-53 re-entry (`restart/skinny/tranches/sk-v17/SPEC.md:578`)").
**Disposition: REVISE.**
**Evidence:** This is the V3 CH5-S8 desync recurring in a DIFFERENT artefact. The V3
REVISE fixed 1A's anchor (`:101`→`:104`, now `:825`/`:839`), but 1E's parallel
firewall surface carries its own REDRESS-53 re-entry anchor at SPEC `:578` — and
`:578` does NOT name REDRESS-53. `grep REDRESS-53 SPEC.md` returns exactly `:577`,
`:657`, `:825`, `:839` (verified, and identical to the four lines V3 CH5.md:207 lists
as the canonical naming sites). SPEC `:577` is the line that names REDRESS-53 against
the parallel-vector shape ("the L1/L4 index retained as a parallel vector
(REDRESS-53)"); `:578` is the adjacent "retained cursor / aux density / sidecar event
vector" continuation — a real but DIFFERENT pre-blocked-route line that does not
carry the REDRESS-53 token. 1E's other REDRESS-53-adjacent surfaces are correct (the
W2 row reference and the substrate-target rows all resolve), and the substantive
coupling (retained parallel index = REDRESS-53 re-entry) is right; only this single
re-entry anchor points one line low. Sibling 1A (`:104`/`:135`) and
`1f-anti-pattern.md:62/85` anchor the same re-entry at `:825`; 1E is the lone
artefact off by one here.
**Why REVISE not ACCEPT:** The coupling is correct and load-bearing; only the
line-anchor for the REDRESS-53 number is wrong (it points at the adjacent
sidecar-vector clause, not the REDRESS-53 naming clause). A wrong anchor on a REDRESS
citation is a CH1/CH3 paper-close smell even when the conclusion is right, and it
desynchronises 1E from its sibling 1A (which V3 explicitly re-anchored to the naming
line) and from `1f-anti-pattern.md`. This is the same failure class V3 CH5-S8 closed
in 1A, surfacing now in 1E — closing it here completes the cross-artefact
consistency.
**Why REVISE not REJECT:** Single-token line offset on a re-entry citation whose
conclusion and shortlist binding are otherwise correct; no verdict, no substrate
claim, no firewall reading changes. Orphan-free, one-line concrete fix.
**Concrete fix:** Edit `1e-locks-evidence.md:179` to read "… else REDRESS-53
re-entry (`restart/skinny/tranches/sk-v17/SPEC.md:577` [parallel-vector naming];
W2 row `:825`; shortlist condition 1 `:839`)." — i.e. anchor the REDRESS-53 number
at `:577`/`:825`/`:839` (the three naming sites) rather than `:578` (the adjacent
sidecar-vector clause). This syncs 1E with 1A's V4 firewall row and
`1f-anti-pattern.md:62/85`. No verdict changes; only the cited line.

## CH5 Firewall Verdict (Track 1 ≡ Track 2)

The V4 excavation passes the Lock-1 firewall. With 1A holding the dedicated
substrate-union spine and the V3 CH5-S8 REVISE folded + live-re-verified, NO
catalogued state implies a live parallel substrate, sidecar producer, or
renamed-scanner / cross-call-classifier-state violation, and NO Track-1 ≡ Track-2
dishonesty surfaces:

- **One substrate, unwired** — `TapeStructBuilder` is grep-zero outside `tape/`
  (re-verified); the eager `OpenFrame` builders are the live substrate; the dead
  tape is the SK-V18 fold target, not a second plane (CH5-S1).
- **The only live retained projection** — the `OnceCell<StructuralIndex>` scan
  cache across all 8 carriers — is correctly held as a retained scan cache (not the
  tape projection), pinned to the four-value `substrate_target` manifest + REDRESS-53
  **at the corrected `:825`/`:839` naming anchors** (the V3 REVISE, now closed), scoped
  to all 8 carriers (CH5-S2).
- **No cross-call classifier carry** under Lock 1 v+1; the `retention_lifetime`
  REJECT class is correctly held open, not falsely cleared (CH5-S3).
- **The 5-shape BackendShape canon is excavated whole**; CollapsedStage is
  x86-pinned and the aarch64 candidate is correctly resolved to the spec-named
  UNKNOWN-2D-05; aarch64-NEON is framed as absorption under the four LLVM shapes,
  never a 6th shape; 1A↔1E agree (CH5-S4).
- **The Lock-1 closure obligation** (exactly-one-encoding post-fold; AoS/SoA
  coexistence transient-only) is intact as an explicit invariant in three places in
  1A and re-stated in 1E (CH5-S5).
- **The direction guard** (monotonic skinny→totality; the §9-forbidden-in-skinny
  names ARE the fold-target names; adoption INTO core, never relocation INTO skinny;
  the wrong-tree-dishonesty bar at SPEC `:814-815`) holds (CH5-S6).
- **The do-not-redrive fence** (FieldSource/StructLayout walk is compile-time
  projection-emission, never a per-leaf registry lookup that re-opens the
  28-65×/983×/10583× regression) is correctly surfaced; `begin_compound(&StructLayout)`
  takes a pre-resolved reference (CH5-S7).

The single REVISE (CH5-S8) is a line-anchor off-by-one on the REDRESS-53 re-entry
citation in `1e-locks-evidence.md:179` — the coupling is correct, the anchor points
at the adjacent sidecar-vector clause (`:578`) instead of the REDRESS-53 naming line
(`:577`/`:825`/`:839`). It is the same failure class V3 CH5-S8 closed in 1A, now
surfacing in 1E; it is an orphan-free REVISE with a one-line concrete fix and does
not change any verdict.

The V3 CH5-S8 REVISE (1A:101 anchor at the D6 block) is **closed**: 1A:104 now
anchors REDRESS-53 at its naming lines `:825`/`:839`, re-verified live, and synced
with both sibling artefacts.

## Disposition Summary

| Section | Subject | Disposition |
|---|---|---|
| CH5-S1 | Parallel-substrate firewall: one unwired tape (SUB17-001/002/003, AP17-001) | ACCEPT |
| CH5-S2 | Sidecar / retained projection across all 8 + V3 CH5-S8 anchor closure (Firewall row 2, AP17-004) | ACCEPT |
| CH5-S3 | Renamed-scanner / cross-call classifier-state (Lock 1 v+1) | ACCEPT |
| CH5-S4 | 5-shape BackendShape canon + aarch64-NEON absorption (Lock 10, SUB17-005, 1E parity) | ACCEPT |
| CH5-S5 | Lock-1 closure obligation: exactly-one-encoding post-fold | ACCEPT |
| CH5-S6 | Second-substrate / monotonic-direction / forbidden-name collision (SUB17-005, §9) | ACCEPT |
| CH5-S7 | StructRegistry/FieldSource per-leaf hot-path do-not-redrive fence (SUB17-009) | ACCEPT |
| CH5-S8 | 1E REDRESS-53 re-entry anchor off-by-one (SPEC :578 → :577; 1e:179) | **REVISE** |

**Counts: ACCEPT 7 · REVISE 1 · REJECT 0 (8 sections).**
ACCEPT rate 87.5%. The pass does NOT yet meet the §4 ≥95% criterion from this lens.
The single REVISE is a non-orphan one-line citation-anchor fix (no verdict change, no
structural defect) — the SAME REDRESS-53-anchor failure class V3 closed in 1A,
recurring in the sibling 1E. With CH5-S8 folded in V5 (anchor 1e:179 → `:577`), the
substrate-union + BackendShape firewall converges. Zero open critical defects from
this lens; zero orphan REVISE. The V3 REVISE (CH5-S8 in 1A) is closed.
