# SK-V16 P3-A: Candidate Shortlist

Pass: S-P3 Synthesis-Plan. Cycle: V16.
Date: 2026-05-28.
Scope: distil the locked S-P2 pool into <=8 SK-V16 interventions.
Output: this file.
Pass Alpha goalset: preserve JSON 51/51, rebuild CSS L4 through grammar-derived typed equality and cssparser SOTA, retire or intrinsically block dirty generated state, advance Pattern H to generator-owned collapse, preserve Decision/BackendShape/FNV guards, and scope native SIMD only profile-first on aarch64.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## Section 1 - Synthesis

S-P3 cannot treat the whole SK-V16 goalset as an optimization shortlist. The
required CSS, dirty-generated, Pattern H, Lock 14, Decision, and FNV work is
Alpha/S-P0 prune and gate work. The S-P2 candidate pool contributes only the
primitive or evidence classes that survive V2 hardening: byte-set/class-table
scans, local string-special masks, escape/hex/digit atoms under strict
scalar/checkasm gates, sealed-tape scalar view operations, and materialization
ratio reporting as evidence.

Rejected or quarantined S-P2 routes stay out of the shortlist: x86 scope,
PMULL/CSSC promotion from ISA or checkasm alone, retained sidecars, retained
cursor/list, parser-owned structural streams, second tapes, public substrate
APIs, harness checksum/FNV, CSS legacy fact streams, old JSON Unicode,
tiny-string, string64, and numeric shortcut framings.

## Section 2 - Deliverable

| # | Candidate intervention | Owner path family | Scalar reference | Checkasm/parity | Same-wave consumer | Falsifiability gate |
|---:|---|---|---|---|---|---|
| 1 | SK-V16 report consumers and telemetry gate | `skinny/xtask/src`, `skinny/crates/bbnf-bench/src/{report.rs,gate.rs}`, `skinny/RESULTS.md` | Existing schema-v3 and SK-V15 report validators | Report negative fixtures; no SIMD | `cargo xtask gate-json --check-results` plus SK-V16 side-report flags | W0 rejects missing CSS typed, dirty-generated, Pattern H, and native SIMD report fields; JSON 51 maintain; CSS remains 0/24 admitted |
| 2 | Dirty generated retirement or intrinsic block | CSS L4 generated runtime files, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, codegen checks | Current broad regen/check commands | Byte-equivalent regen/check proof | Dirty-generated report gate | Exact dirty manifest and broad-command result per file; no dirty file can serve as close proof |
| 3 | Grammar-derived CSS provider and typed API | `grammar/css/l4`, CSS generator/provider/runtime, CSS bench/report | cssparser same-workload typed summary is the semantic oracle | Typed equality, not checkasm | CSS typed report and CSS API tests | 24 CSS rows require grammar-derived provider, typed summaries, pass/error counts, and no `CSS_GENERATED_RS`/fact-stream proof |
| 4 | CSS same-workload SOTA retime | CSS bench/report/results/redress | cssparser same typed workload | Typed equality must already pass | `--skv16-css-typed-report` speed fields | Each admitted CSS row must satisfy `track1_typed_mbps >= cssparser_typed_mbps + 1.000` on Apple M5 Max/aarch64 |
| 5 | Pattern H generator-owned collapse | `crates/core/src/runtime`, generator/runtime templates, roundtrip gate | Existing 67-file runtime census | Byte-equivalent generator roundtrip | `--skv16-pattern-h-roundtrip-report` | File count remains 67; generated ownership is roundtrip proof, not header text |
| 6 | Grammar-neutral byte/class/string primitive consumer | `skinny/crates/bbnf-simd`, `skinny/crates/parse-that-regex`, generated consumers | Existing scalar byte/class/string/escape helpers or new scalar oracle | `BBNF_SIMD_STRICT=1 cargo xtask primitive-checkasm` for native paths | One selected JSON or CSS/Sheets/BBNF generated hot-path caller in same wave | Target rows from P3-C meet floors; full JSON 51 maintain; no x86, PMULL/CSSC-only, FNV, or sidecar proof |
| 7 | Sealed tape/view scalar consumer | `skinny/crates/runtime/src/tape`, generated views, materialization report | Current sealed tape/view traversal | Golden view/tape parity; no checkasm unless native mask source is introduced | Generated view/tape accessor or tape construction path | Tape/view parity passes; no retained cursor/list/aux table/second tape; ratio report is evidence only |
| 8 | Decision/BackendShape preservation hardening | `skinny/crates/passes`, codegen lowerers, decision/CSP fixtures | Existing W7-W9 proof surfaces | Generated fixture parity and anti-scaffold tests | Decision/lowerer report gate | E-graph/CSP and all-five lowerer proofs remain load-bearing; no sixth `BackendShape` or EventTape sidecar |

## Section 3 - Falsifiability Binding

All candidate gates are measured from the bench or from a gate-consumed report.
Behavior candidates bind to the P3-C JSON maintain formulas and CSS typed
SOTA formula. Gate-only candidates close only on negative fixtures and
no-behavior proof; they cannot admit a row by relabeling.

Native SIMD is conditional. A selected native primitive must name the S-P1 hot
leaf, scalar oracle, strict checkasm command, same-wave consumer, cold row
measurement, and no-x86 proof before redress.

Planning row bindings:

| Candidate | Named rows | Concrete threshold |
|---|---|---|
| 1 report consumers | all 51 JSON rows, all 24 CSS rows | JSON Track 1 `max(0.98 * open, sonic + 1.000)`, Track 2 `0.98 * open`; CSS remains 0/24 admitted |
| 2 dirty generated | all 51 JSON rows; 24 CSS rows if diagnostic rerendered | JSON formulas; CSS diagnostic Track 1, if rerendered before typed rebuild, stays >=2272.660 Mbps and non-admission |
| 3 CSS provider/equality | all 24 CSS rows | equality first: `css_typed_summary_equal=true`, equal pass/error counts, grammar-derived provider; no speed admit |
| 4 CSS SOTA | all 24 CSS rows | `track1_typed_mbps >= cssparser_typed_mbps + 1.000` per row |
| 5 Pattern H | 67 runtime files plus JSON guard rows | count exactly 67; JSON formulas; no performance row moves from header-only proof |
| 6 byte/class/string/escape/digit primitive | `twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, `y_string_unicode`, `canada`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic` parse rows; selected direct/typed guards | P3-C floors, for example `twitter/parse_only >= 8182.304`, `canada/parse_only >= 16375.703`, `numbers/parse_only >= 14182.862`, `unicode_escapes/parse_only >= 7739.500`, plus full JSON formulas |
| 7 sealed tape/view scalar | JSON view/eager-decode rows selected by the wave plus JSON guard rows | view/tape parity and JSON formulas; any behavior claim must also beat its W0 row floor, otherwise evidence-only |
| 8 Decision/materialization evidence | Decision/lowerer fixtures, materialization rows, JSON guard rows | e-graph/CSP/all-five lowerer reports consumed; materialization ratio is evidence-only and JSON formulas hold |

## Section 4 - Pre-Blocked Routes

Every candidate blocks REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89,
96-98, 183/184/209-213, 215, 242-247, and FNV production migration under
their old framing. A fresh route needs fresh P1 evidence, scalar/oracle proof,
same-wave consumer, row movement, and REDRESS non-reopen proof.

## Section 5 - Sources

- `restart/skinny/tranches/sk-v16/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v16/HANDOFF.md`
- `restart/skinny/tranches/sk-v16/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
- `restart/skinny/tranches/sk-v16/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v16/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v16/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
