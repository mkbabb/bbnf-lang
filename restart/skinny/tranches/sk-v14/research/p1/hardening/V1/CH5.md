# SK-V14 S-P1 V1 CHALLENGE — Lens CH5 — HIDDEN COUPLING

Author: CH5 lens agent, S-P1 CHALLENGE V1.
Date: 2026-05-23.
Scope: lens CH5 — HIDDEN COUPLING — across the six committed P1 artefacts
(`p1a-samply-mode-1.md` … `p1f-results-delta.md`).
Binding: `PASS-1-PROFILE.md §3 CH5` (lines 148-153) + `ORCHESTRATOR.md §3W` CH5
("No parallel substrate, sidecar producer, renamed-scanner Lock 1 violation, or
Track 1 ≡ Track 2 dishonesty; substrate union holds.") + `SYNTHESIS.md §0` P-7
(Track 1 ≡ Track 2 dishonesty; lines 145-148) + `SYNTHESIS.md §2`
`track2_entry_point` column (line 240).
Discipline: write-only, `path:line` per claim, executable-verification mandate.

## §1 — CH5 disposition summary

CH5 asks whether any hot-leaf attribution implies a **parallel substrate**, a
**sidecar event vector**, a **second source scan**, or a **retained cursor**
that conflates Track 1 (generated runtime) with Track 2 (structurally
independent oracle). The CHALLENGE-CONTEXT V1 focus narrows this to two
load-bearing claims that V1 must verify:

- P1-B `DirectParser::skip_value` (typed plane, 72.5%–76.1% dominance on
  Twitter / citm_catalog / etc.) — flagged as "substrate-walk-with-shape-
  validation". CH5 must verify the framing is "hybrid substrate-union
  primitive" (Lock 1 sustained), not "parallel substrate" (Lock 1
  violation).
- P1-C ANOM-2 (`host_call_eager_decode` is view-walk + UTF-8 decode, not
  parse) — flagged as the view-boundary materialization cost of the Lock 1
  substrate union. CH5 must verify this is named as a substrate
  observation, not as a parallel sidecar.

### Headline verdict

**ACCEPT (5 of 6 axes).** The P1 wave keeps Lock 1 intact at the symbol-path
level: every reported hot-leaf attribution resolves to either the singular
Track 1 generated runtime substrate (`runtime::generated_json::*` with the
shared `&mut usize` cursor + offset-tape positions) **or** the singular
Track 2 oracle substrate (`bbnf_bench::direct_struct::hand::HandParser` and
`bbnf_bench::generated_real_typed::DirectParser`, each with its own
`cursor: usize` field), with **no** crosswalks between the two cursor
state machines. The substrate-union framing is correctly named (P1-A §4
"Lock-1 same-substrate union signal"; P1-E §4.4 "substrate-union (Lock 1)
substrate-vs-producer mixing"; P1-C ANOM-2 "Lock 1's view-boundary
materialization").

**REVISE (1 of 6 axes).** The mandated `track2_entry_point` schema column
(SYNTHESIS.md §2:240) — the CH5 hidden-coupling guard — is **absent from
every row** in `RESULTS.md` and absent from every per-row table in
P1-A / P1-B / P1-C / P1-D. P1-F §4.1 names the gap correctly
(p1f-results-delta.md:175 "track2_entry_point | CH5 hidden-coupling guard
| No — zero matches | 76 rows × column gap") but the P1 axis tables
themselves do not populate the column even informally. CH5 cannot
**enforce** the no-cross-ancestor guard until the column lands; the V1
disposition is "column-gap acknowledged + carries forward to C-2 wave
(R2 + schema rewrite per SYNTHESIS §2)".

ACCEPT-rate per axis (6 axes): 5/6 = **83 %** ACCEPT, 1/6 = **17 %** REVISE
(no REJECT). The REVISE is a downstream-wave column gap, not a P1 symbol-
attribution error; CH5 cannot fault P1 for failing to populate a column the
bench harness does not emit.

## §2 — Per-artefact CH5 disposition table

The CH5 audit per artefact: does any hot leaf imply (a) parallel substrate
walking the same bytes via a second classifier, (b) a sidecar event vector
the parser writes outside the offset-tape, (c) a retained cursor whose
lifetime spans parse iterations, (d) a second source scan over the input,
or (e) a Track 1 ≡ Track 2 symbol-path collapse?

| Artefact | (a) parallel substrate? | (b) sidecar event vector? | (c) retained cursor? | (d) second source scan? | (e) Track 1 ≡ Track 2 collapse? | CH5 verdict |
|---|---|---|---|---|---|---|
| `p1a-samply-mode-1.md` (340 lines) | No — every hot leaf resolves to inlined frames inside `runtime::generated_json::generated::dispatch_value` against the single `&mut usize` cursor + shared `Vec<u32>` positions tape (`generated.rs:466-484`; `scan.rs:131-156`). | No — `copy_nonoverlapping` 9.5-11.4% on marine_ik/canada explicitly flagged as **tape-commit pressure** (p1a-samply-mode-1.md:318-320 "the Lock-1 same-substrate union signal P1-E must explicitly attribute as substrate"), i.e. the offset-tape **is** the substrate, not a sidecar to it. | No — every cursor in the symbol path is the function-argument `*cursor: usize` per `parse_object_value_at_direct` signature (`generated.rs:469`), bounded by the per-parse call frame; no inter-parse state. | No — `parse_only` walks the input once; the inlined `match_string_at_quote_trusted_utf8` SIMD path is part of the same scan, not a second source pass. | No — Track 2 not in scope of P1-A (parse_only Track 1 only); no collapse risk. | **ACCEPT** |
| `p1b-samply-mode-2.md` (320 lines) | **Conditional ACCEPT** — `DirectParser::skip_value` (typed plane) at `generated_real_typed.rs:2949` (verified at HEAD; SK-V13 P1-E cited `:1739` but the function is at `:2949` in the SK-V14 baseline source — source-line drift documented in §3 anomaly A) walks `self.bytes` (a borrowed `&'i [u8]`) directly via its own `cursor: usize` field (`generated_real_typed.rs:2745`). This is Track 2's substrate, **structurally distinct** from Track 1's `parse_object_value_at_direct` which threads `cursor: &mut usize` from the public entry. Two cursors, two substrates, no crosswalk — Lock 1 holds. P1-B §4 anomaly 4 (lines 272-274) correctly classifies `skip_value` as "substrate-walk-with-shape-validation primitive" within the typed product plane; the P1-E §4.4 reading (p1e-hot-leaf-attribution.md:229-234 "substrate-union observation … neither a pure substrate primitive nor a pure producer primitive, but a hybrid that walks the substrate while validating type-shape") is the correct CH5 framing. | No — `DirectParser` writes nothing to a sidecar; it returns a typed struct via `Result<(), DirectBuildError<'i>>` only. The typed-product output is the function-return value, not a sidecar event vector. | **Yes-but-bounded.** `DirectParser::cursor` is retained for the duration of one `track1_typed` call (one whole-input parse); not retained across iterations. Per `profile_direct.rs` driver, `DirectParser::new` is called fresh on every iteration (`black_box(input)` per iter; `bbnf_bench::direct_struct::track1_digest` — p1b-samply-mode-2.md:15 "There is no warm-cache amortisation — the parser receives the source slice cold on every iteration"). The "retained" cursor is per-parse-call, not per-bench-loop; this is the standard hand-coded parser pattern, not a Lock 1 violation. | No — the typed product plane reads `self.bytes` once via `cursor` advances; even `skip_value`'s recursion (skip_object → skip_value → skip_string_raw at `generated_real_typed.rs:2966-3018`) is a single linear walk through the source. Per P1-B §4 anomaly 4 (line 274): "S-P2's primitive design must include a skip_value that walks the offset tape without touching the source byte slice — the current implementation calls back into the source-byte scanner." This is the **observation** that the current `skip_value` re-scans source bytes instead of consuming an offset tape; CH5 reads this correctly as the Lock 1 substrate-union observation (the typed plane currently re-walks source because Track 2 has no offset tape — by design, Track 2 is the **structurally independent oracle**). The substrate-union question for S-P2 is whether Track 1's product surface should consume an offset tape; that is a Track 1 design question, not a Track 1 ≡ Track 2 collapse. | No — `bb::grt::DirectParser` (Track 2 typed) and `runtime::generated_json::generated::parse_object_value_at_direct::<JsonDigestSink>` (Track 1 direct) share **zero symbol-path ancestors** beyond the workspace root. The two product-plane parsers have entirely independent module hierarchies and entirely independent cursor state machines; the substrate-union framing at the conceptual level does **not** project to symbol-path collapse at the implementation level. | **ACCEPT** |
| `p1c-samply-mode-3.md` (607 lines) | **Conditional ACCEPT** — ANOM-2 (`host_call_eager_decode`, lines 434-451) names the view-walk + UTF-8 decode dominance correctly as **Lock 1's view-boundary materialization**: "The host_call probe measures the cost of Lock 1's view-boundary materialization, not parse. … the substrate union forces a second pass to lift offset-tape positions back into decoded string slices." This is a substrate observation (the offset-tape view boundary), not a parallel sidecar. The "second pass" is over the view-tree iterators (`JsonObjectPairs::next`, `JsonArrayValues::next` at `view.rs:268, 310`) consuming the offset-tape, **not** a second walk over the source bytes — verified per the §2.3 primitive table (p1c-samply-mode-3.md:311-326): every "second-pass" symbol lives in `runtime/src/grammars/json/view.rs`, consuming the same `Tape` substrate that Track 1's parse populated. No parallel substrate; this is the substrate's intended view boundary. | No — the view tree is itself a read-only projection of the offset-tape, not a written sidecar. The `eager_decode_strings::walk` (line 319) writes into the probe's scratch buffer for measurement, not into a parser-owned sidecar. | No — view iterators are per-parse, per-tree-walk; no retained cursor across iterations. | The framing question — "is the view tree a second source scan?" — is correctly disposed: per ANOM-2 (line 446-451) the view walk **does** consume source bytes (`from_utf8 10.16%`, `unescape_string 2.83%`, `string_body_range 15.68%` for slice-into-source resolution), but this is **boundary materialization on demand from the offset-tape**, not a second classifier walk. The structurals were classified once during parse; the view materialization is a per-leaf lookup keyed on the tape, not a re-classification. CH5 ACCEPT contingent on this reading staying explicit in S-P2; the V1 fold recommendation (§4 below) flags the need to name the view-tree consumer's source-touch budget separately in any future schema row. | No collapse — the ANOM-1 misnamed-probe finding (line 414: "`alternate_scalar_plan` … the body implements the serde_json comparator. … Zero samples in any `runtime::generated_json::*` symbol") **proves** non-collapse: the comparator probe and the Track 1 path share zero hot-leaf symbols. The probe-misnaming is a separate CH2 / CH6 finding; from CH5's lens, the zero overlap is exactly what Lock 1 demands. ANOM-4 (line 470-484) — `parse-attribution` off folds `parse_object/array/string/number/literal/pair/key_colon` into the single `dispatch_value` symbol — is a CH6 paper-close risk (envelope folding), but **not** a CH5 collapse: every folded inlined symbol still resolves into the Track 1 module path. | **ACCEPT** |
| `p1d-pmu-cycles.md` (648 lines) | No — the 231-row PMU table cites only symbol-blind cycle / instruction counters from `proc_pid_rusage(RUSAGE_INFO_V5)` per process; the per-row c/B is the load-bearing comparator and does not imply any substrate hypothesis. Probe-level analysis (§4 anomaly 3, line 531-538: `host_call_eager_decode` "the offset-tape itself is cheap, but eager string materialisation amplifies on unicode") and (§4 anomaly 7, line 565-567 "the SIMD ratio is a substrate truth, not a prompt for parallel-substrate redress") explicitly forecloses parallel-substrate readings. | No — the cycles counter is per-process aggregate; no sidecar implication. | No — same. | No — same. | No — Track 1 / Track 2 / sonic / serde rows are independent table rows with independent counters; the §2.3 "Track 2 typed > Track 1 typed on apache_builds and random" finding (line 540-550) is per-row independent measurement, not collapse. | **ACCEPT** |
| `p1e-hot-leaf-attribution.md` (306 lines) | No — §4.4 (line 229-234) is the canonical CH5 dispositional paragraph: "`DirectParser::skip_value` (5 of 7 typed rows) is a substrate-union observation: it is neither a pure substrate primitive (the offset tape) nor a pure producer primitive (typed-value construction), but a hybrid that walks the substrate while validating type-shape. … Under that binding, `skip_value` is `substrate` + `dispatch` in equal parts. S-P2 must not split it into two separate primitives — it is a single substrate-union primitive". This is the correct V1-focus framing per CHALLENGE-CONTEXT §2: hybrid not parallel. | No — the `tape` primitive class (line 78) names "offset-tape emit primitive (substrate-union Lock 1) tape positions Vec push inside scan_tail + parse_*_direct lanes" — i.e. the tape IS the substrate, not a sidecar. The Vec<u32> positions are co-substrate with the byte slice, not a parallel event log. | No — per-parse only. | No — single classifier pass; the typed plane re-walks within the substrate union framing per §4.4. | No — §4.4 explicitly **forbids** splitting `skip_value` into separate "substrate" and "producer" primitives (which would imply Track 1 ≡ Track 2 collapse via shared primitive name). The Lock 14 disallow on JSON-specific naming (line 234 "the answer is yes structurally, but Lock 14 disallows JSON-specific naming") protects against the generalization-induced collapse. | **ACCEPT** |
| `p1f-results-delta.md` (260 lines) | No — schema audit; no hot-leaf substrate implication in this artefact. | No — same. | No — same. | No — same. | **The CH5 column gap is the single REVISE in this CHALLENGE.** Per §4.1 (p1f-results-delta.md:175): `track2_entry_point` is **NOT present in RESULTS.md** — zero matches across 186 lines. SYNTHESIS.md §2 line 240 makes the column mandatory: "symbol path of the Track 2 oracle entry point; `xtask gate-json` rejects any row where the Track 1 and Track 2 entry-point symbol paths share a common ancestor in `runtime::tape::` beyond the public `Tape` / `OffsetFlags` types." Without this column populated, CH5 cannot **mechanically enforce** the no-cross-ancestor guard per row. The narrative across P1-A/B/C/D/E (per the per-artefact rows above) names the entry points correctly, but the schema enforcement is deferred to the C-2 wave (R2 + schema rewrite). | **REVISE** (column gap; not a P1 attribution error; carry forward to C-2) |

Per-axis ACCEPT-rate: 5/6 ACCEPT, 1/6 REVISE → **83 % ACCEPT** on the CH5 lens.

## §3 — Critical findings

### Finding CH5-A — source-line drift on `DirectParser::skip_value`

P1-E (p1e-hot-leaf-attribution.md:140-141, 144) cites `DirectParser::skip_value`
at `bbnf-bench/src/generated_real_typed.rs:1739` (carry-through from SK-V13
P1-B V1). The same function in the SK-V14 HEAD source is at
`generated_real_typed.rs:2949` (verified by `grep -n "fn skip_value"`; file
has grown to 3056 lines). The CH5 substantive verdict is unchanged — the
symbol still walks `self.bytes` via its own `cursor: usize` (line 2745) —
but the file:line citation in P1-E is stale.

P1-B (p1b-samply-mode-2.md:89) sidesteps the line-citation issue with the
softer formulation "`skinny/crates/bbnf-bench/src/generated_real_typed.rs`
(`DirectParser` impl)". That is non-falsifying but loses the
file:line discipline the CHALLENGE-CONTEXT §3 mandates ("Cite `path:line`
on every claim").

V2 fold action: P1-E line 140-141, 144 update to cite `:2949` (skip_value)
+ `:2966` (skip_object) + `:2987` (skip_array); P1-B §2.1 line 89 add
explicit line numbers. Severity: LOW (claim correctness unchanged; cite
hygiene only).

### Finding CH5-B — `track2_entry_point` column gap (the CH5 hidden-coupling guard)

The mandated CH5 schema enforcement column is absent from every per-row
table in the P1 wave. P1-F correctly names the gap (line 175) and assigns
the population to C-2 (R2 wave). The narrative coverage across P1-A/B/C/E
is sufficient for V1 CHALLENGE ACCEPT — Track 1 entry points (`parse_only`,
`parse_object_value_at_direct::<JsonDigestSink>`) and Track 2 entry points
(`hand::HandParser::value`, `DirectParser::skip_value`) are
**distinguishable by inspection** at the symbol-path level and share no
`runtime::tape::*` ancestor beyond the public types. The mechanical gate
remains a C-2 deliverable.

V2 fold action: when P1 re-runs against the post-R2 bench harness, the
`track2_entry_point` column lands populated; the V2 CHALLENGE CH5 then
escalates from "narrative ACCEPT, column REVISE" to "schema-enforced
ACCEPT". Severity: MED (column populates after C-2; P1 cannot deliver it
unilaterally).

### Finding CH5-C — view-tree consumer source-touch budget unbounded in schema

P1-C ANOM-2 names the view-walk source-touch cost (line 437-441:
"`at_cursor` 23.28%, `string_body_range` 15.68%, `from_utf8` 10.16%, pair/
value iterators 9.26%, `unescape_string` 2.83%, `as_str` 2.56%,
`validate_block_scalar` 1.32%") but the SK-V14 schema has no column that
bounds the view-tree's source re-touch budget. The substrate-union framing
sustains Lock 1 only if the view tree's source-touch is **measured and
bounded** relative to the parse-time source-touch; otherwise an unbounded
view-side scan could re-classify the source bytes in a parallel substrate
walk, which **would** be a CH5 violation.

V2 fold action: S-P2 primitive design must specify whether `string_body_range`
+ `from_utf8` in the view-walk reads new source bytes or only resolves
offsets already classified during parse; if the latter, name the
substrate-union accounting that proves no second classification. The
"`view_source_touch_ratio`" or equivalent telemetry field is the natural
home. Severity: MED (S-P2 design question; CH5 does not falsify P1's
disposition, but flags the structural question P1-C ANOM-2 implicitly
raises).

### Finding CH5-D — `parse-attribution=off` folds 7 Track 1 leaves into `dispatch_value` (CH6 risk; CH5 reads as non-collapse)

P1-C ANOM-4 (line 470-484) + P1-E §4 (line 219) both flag that
`#[cfg_attr(not(feature = "parse-attribution"), inline(always))]` folds
`parse_object / parse_array / parse_string / parse_number / parse_literal /
parse_pair / parse_key_colon` into the single `dispatch_value` symbol at
`generated.rs:45`. From CH5's lens this is **not** a collapse — every
folded symbol resolves into `runtime::generated_json::generated::*`, all
under Track 1's substrate. The CH6 paper-close risk (the envelope obscures
the inner primitive) is real and is logged on the CH6 lens; the CH5 lens
notes only that the folding **does not** create symbol-path overlap with
Track 2 (which lives under `bbnf_bench::*`).

V2 fold action: a `parse-attribution=on` profile pass (one full
P1-A/B/C run with `--features parse-attribution`) cracks the envelope for
CH6; CH5's verdict carries over unchanged because the inner primitives
remain under the same Track 1 module path. Severity: LOW (CH6 risk, not
CH5).

## §4 — V2 fold recommendations

The V1 → V2 fold for CH5 requires four narrowly-scoped actions:

1. **CH5-A cite hygiene.** Update P1-E source-line citations to the
   SK-V14 HEAD line numbers (`skip_value :2949`, `skip_object :2966`,
   `skip_array :2987`). P1-B §2.1 line 89 add line numbers analogous to
   the other table rows. (LOW; cite-only; no claim revision.)

2. **CH5-C view-tree source-touch budget.** S-P2 primitive design pass
   must answer the substrate-union question the P1-C ANOM-2
   view-materialization framing raises: does the view-tree walk re-read
   source bytes that the parse-time scanner already classified, or does
   it only resolve offsets emitted by the scanner into byte ranges? The
   answer determines whether the view tree is a substrate consumer
   (Lock 1 holds) or a parallel substrate (Lock 1 fails). Recommend
   landing a `view_source_touch_ratio` telemetry field in the C-2 schema
   so the question is mechanically gateable per row. (MED; design
   question, schema deliverable.)

3. **CH5-B `track2_entry_point` column population.** When C-2 lands
   the R2 schema rewrite + the per-row `track2_entry_point` column,
   the V_C-2_+1 CHALLENGE CH5 then enforces the no-cross-ancestor
   guard per row mechanically. Recommend P1-F V2 also call out which
   17 rows × 3 planes = 51 JSON cells + 24 CSS feature rows need the
   column populated. (MED; downstream C-2 deliverable; P1 cannot
   deliver unilaterally.)

4. **Cross-V1-lens consistency.** CH5's "ACCEPT contingent on view-walk
   substrate framing" reading (per CH5-C) should propagate to the V1
   CHALLENGE consolidation: any V2 fold that lands a "view-walk
   primitive" in the S-P2 primitive set must explicitly cite either
   substrate-union accounting (Lock 1 sustained) or a redress route to
   the parallel-substrate alternative (Lock 1 violation, REJECT). The
   consolidation should add this as a forward-CH5 guard the V2 wave
   carries. (LOW; cross-lens housekeeping.)

## §5 — Sources cited (executable-verification)

Verified per CHALLENGE-CONTEXT §3 "Executable-verification mandate":

- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` — read end-to-end (54 lines).
- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (340 lines) — §2 hot-leaf tables + §4 anomalies read.
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (320 lines) — §2.1 file:line table + §3 typed-plane delta + §4 anomaly 4 (`DirectParser::skip_value`) read end-to-end.
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (607 lines) — §2.3 primitive substrate table + §4 ANOM-1/2/3/4 read end-to-end.
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (648 lines) — §2.3 + §4 anomalies 3, 6, 7 + §3 delta-table read.
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (306 lines) — §1.3 primitive classes, §3 typed-plane table, **§4.4 substrate-union paragraph** (load-bearing for CH5).
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` (260 lines) — §4.1 schema-extension column table (`track2_entry_point` row).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` §0 P-7 (lines 145-148) + §2 telemetry binding (line 240 `track2_entry_point`).
- `restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH5 (lines 148-153).
- `restart/prompts/ORCHESTRATOR.md` §3W CH5 (line 87) + §3W invariants (lines 202-203).

Source-code verification (CH5-A, CH5-B framing):

- `skinny/crates/runtime/src/grammars/json/generated.rs:466-484` — Track 1 `parse_object_value_at_direct` signature uses `cursor: &mut usize` threaded through call frame (substrate cursor public).
- `skinny/crates/runtime/src/grammars/json/scan.rs:131-156` — `scan_tail_byte` writes `positions: &mut Vec<u32>` (the offset-tape; the substrate-union companion to the byte slice).
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2742-2756` — `struct DirectParser { input, bytes, cursor: usize }` — Track 2 typed substrate, structurally independent of Track 1's positions tape.
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2949-3003` — `DirectParser::skip_value` / `skip_object` / `skip_array` — walks `self.bytes` via `self.cursor` only; no Track 1 substrate touch.
- File line count: `wc -l generated_real_typed.rs` = 3056; SK-V13 P1-E `:1739` citation is stale vs SK-V14 HEAD (current line :2949). CH5-A cite hygiene finding.
- `grep -c 'track2_entry_point' skinny/RESULTS.md` = 0 (column absent; CH5-B gap confirmed by p1f-results-delta.md:175).
