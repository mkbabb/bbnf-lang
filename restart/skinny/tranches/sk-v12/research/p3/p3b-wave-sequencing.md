# SK-V12 P3-B: Wave Sequencing

Pass: S-P3 Synthesis-Plan. Cycle: PIN-V1.
Date: 2026-05-20.
Scope: topologically sequence the pin-aware SK-V12 S-P2 survivor pool into a W0..Wn wave manifest.
Output: this file.
Pass Alpha goalset: ADMIT requires a generated CSS L4 Track 1 row with strict equality and throughput greater than `lightningcss_mbps + 1` on the same corpus/output plane/host, with JSON guards held or measured-demoted, Lock 14 clean, Lock 16 clean for SIMD, and the carried aarch64 orphan set at zero. FIXPOINT requires a measured CSS L4 redress attempt, a new measured union-substrate implementation attempt, a new measured ASM-gen implementation attempt, and zero production aarch64 orphans.
Candidate pool: research/p2/ post-CHALLENGE survivors under `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.

## §1 - Synthesis

The user pin replaces the stale V5 sequence. CSS L4 is the first and
authoritative grammar target; Sheets and BBNF-self are fallback-only after a
measured CSS L4 redress attempt. The close floor is not a generated-baseline
lift: admission requires generated CSS L4 Track 1 throughput strictly greater
than `lightningcss_mbps + 1` on the same corpus, output plane, host, and strict
equality semantics.

The sequence therefore starts with legality and measurement infrastructure,
not with an optimization primitive. The value/API audit names seven Lock-14
leaks in the current JSON template surface: structural alphabet, value
dispatch, string escape/quote policy, number policy, quoted-key/object-pair
policy, `OffsetFlags` meaning, and `JsonSink` methods. `GrammarConfig` or an
equivalent generated metadata surface must land before CSS L4 emission is
legal. W1a is that legality wave.

W1b creates the first CSS L4 row. It must stand up generated Track 1, a
canonical CSS fact stream, an independent oracle or Track 2, and a same-plane
lightningcss comparator. W1b can close the admission target only if the measured
row already clears `lightningcss_mbps + 1`, but W1b is still a valid baseline
wave when it records strict equality, provenance, and the exact measured gap.

SIMD and ASM work cannot precede the known correctness blocker. The
`escape_mask_64` NEON falsifier (`0xCAFEF00DBAADF00D`) is a hard gate before
any new SIMD admission. W2 resolves and verifies that correctness issue. It is
not a throughput admission wave and must not be counted as an ASM-gen attempt
unless it wires a new optimized consumer, which this sequence does not assume.

The union category is unblocked by the user pin, but P2-D contributes no
shortlist-ready tape primitive before a CSS baseline exists. W3 is therefore
conditional on W1b's generated CSS baseline and fresh CSS hot-leaf evidence. It
is the only legal union slot in this sequence: a same-tape CSS fact/fact-kind
attempt, with REDRESS 96/97/98 cited as historical implementations and a
material differential named. It may be skipped only if ADMIT is already
reachable without a union attempt; it is mandatory for FIXPOINT.

The ASM-gen category is also unblocked at category level, and D5 requires zero
aarch64 production orphans at close. W4 is the ARMv9.2/Lock-16 consumer and
orphan-disposition wave. It selects at most one row-moving ASM/SIMD consumer
from the S-P2 survivors (`a64_tbl_tbx_byte_class_mask64`,
`a64_udot_digit_run_span`, `a64_wide_string_special_scan64`,
`a64_hex_quartet_decode_x4`, or `a64_ascii_set_run_skip`), or a materially
different narrow PMULL/CSSC support consumer if W1b/W3 evidence names it. It
must also dispose the carried orphan set:
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`byte_context`, and `cache_hints`, by same-wave consumption, removal, or
inventory demotion with evidence. W4 is mandatory before close because D5 is a
close condition; it is the ASM-gen attempt required for FIXPOINT when ADMIT is
uncloseable.

W5 closes. It admits only under the CSS L4 > lightningcss bar. Otherwise it may
record FIXPOINT only if W1b produced a measured CSS attempt, W3 produced a new
measured union attempt, W4 produced a new measured ASM-gen attempt, the orphan
set is zero, and JSON guards are held or honestly demoted. If neither ADMIT nor
FIXPOINT is satisfied, W5 synthesizes the routed remainder for SK-V13 rather
than paper-closing.

## §2 - Deliverable

### Wave Manifest

| Wave | Title | Entry gate | Owner-path family | Dispatch status | Hard cap | LOC / risk | Same-wave consumer requirement | Dependency |
|---|---|---|---|---|---:|---:|---|---|
| W0 | Pin-aware SK-V12 telemetry/gate-lock revalidation | S-P3 converged; W0 commit `f788eb97` reachable; `skinny/RESULTS.md`, gate/report paths, and JSON guard floors readable | `skinny/RESULTS.md`; `skinny/crates/bbnf-bench/src/report.rs`; `skinny/crates/bbnf-bench/src/bin/gate.rs`; W0 research/redress docs | First, unconditional revalidation; redo only on measured drift | research 20 / plan 15 / redress 30 min | <=120 docs/gate LOC / low | Gate/report consumer must consume every required field; no producer-only telemetry | none |
| W1a | `GrammarConfig` legality and Lock-14 non-JSON emission gate | W0 PASS; user pin active; seven Lock-14 leaks enumerated; JSON guard command available | `skinny/crates/runtime/src/tape/grammar_config.rs` or equivalent; `skinny/crates/runtime/src/tape/*`; `skinny/crates/codegen/src/*`; generated template providers; Lock-14 gate/report paths | Dispatchable after W0; CHALLENGE mandatory | research 20 / plan 15 / redress 30 min | <=360 hand LOC / high | Generated metadata must be consumed by JSON guard regeneration and CSS emission preflight in the same wave; no generic grammar-name branches | W0 |
| W1b | CSS L4 generated baseline + lightningcss comparator | W1a PASS; CSS L4 corpus, output plane, canonical fact stream, independent oracle/Track 2, lightningcss adapter, equality command, benchmark command, and rollback slice named | `grammar/css/l4/*`; `skinny/crates/codegen/src/*`; `skinny/crates/runtime/src/grammars/css_l4/*` generated outputs; `skinny/crates/bbnf-bench/src/*`; comparator/oracle fixture paths; `skinny/RESULTS.md` or gate-consumed companion report | Dispatchable after W1a; CSS first, no Sheets/BBNF fallback inside this wave | research 20 / plan 15 / redress 30 min | <=620 hand LOC plus generated LOC budget / very high | Generated Track 1, independent oracle/Track 2, lightningcss comparator, strict equality, and report/gate consumption must land together | W1a |
| W2 | `escape_mask_64` correctness unblock | W1b has a CSS measured row or measured CSS baseline failure; falsifier reproduction command named; no new SIMD admission has landed after the pin | `skinny/crates/bbnf-simd/src/*`; `skinny/crates/bbnf-simd/tests/checkasm_*`; `CHECKASM-REPORT.md`; corpus parity/gate paths | Mandatory before W4 or any SIMD-backed W3 plan; CHALLENGE mandatory because correctness-gate source may touch SIMD | research 20 / plan 15 / redress 30 min | <=180 hand LOC / high-correctness | Strict checkasm, xorshift falsifier reproduction, fixed carry-handoff proof, and JSON corpus parity are the consumer; no throughput row credit by itself | W1b |
| W3 | CSS-local same-tape union/fact attempt | W1b produced CSS Track 1 + comparator evidence; fresh CSS profile/hot-leaf or W1b gap analysis names a retained/fact-stream consumer; micro-proof shows a same-host gain; REDRESS 96/97/98 material differential accepted by CHALLENGE | Existing `Tape`/`TapeBuilder`/fact stream/view paths; CSS generated runtime; CSS equality/visitor/comparator; bench/gate/report paths | Conditional for ADMIT; mandatory for FIXPOINT if W1b/W4 cannot admit | research 20 / plan 15 / redress 30 min | <=420 hand LOC / very high | CSS fact stream/equality or retained CSS visitor must consume the same-tape fact in the same commit; no side vector, no `UnionTape`, no public substrate API | W1b (and W2 if SIMD support is used) |
| W4 | ARMv9.2 ASM-gen consumer + orphan disposition | W2 PASS; W1b CSS row exists; selected S-P2 ASM/SIMD candidate has scalar reference, strict checkasm plan, isolated micro-proof, same-wave CSS or JSON-guard consumer, and REDRESS 88/89/90 differential where adjacent | `skinny/crates/bbnf-simd/src/aarch64/*`; scalar refs/tests; `skinny/crates/parse-that-regex/src/*`; generated CSS runtime; bench/gate/report; orphan inventory docs | Mandatory before close; CHALLENGE mandatory | research 20 / plan 15 / redress 30 min | <=430 hand LOC / very high | One named generated consumer must call the primitive in the same commit, and all five carried orphans must be consumed, removed, or inventory-demoted with evidence | W1b + W2; W3 if the ASM consumer depends on union facts |
| W5 | Close, Alpha feedback, and campaign disposition | W0, W1a, W1b, W2, W3, and W4 admitted, rejected, or routed with measurement; guard rows refreshed or demoted; orphan set disposition known | `restart/skinny/tranches/sk-v12/{SYNTHESIS,SPEC,HANDOFF,DISPATCH-PROMPT}.md`; `skinny/RESULTS.md`; `skinny/REDRESS.md`; close/campaign docs | Final; synthesizes SK-V13 if neither ADMIT nor FIXPOINT holds | research 20 / plan 15 / redress 30 min | <=160 docs/gate LOC / medium | Close gate must consume CSS Track 1/lightningcss numbers, oracle equality, Lock 14/16, union disposition, ASM-gen disposition, orphan state, and JSON guard state | W0, W1a, W1b, W2, W3, W4 |

### Topological Order

`W0 -> W1a -> W1b -> W2 -> W4 -> W5` is the minimum admission path when W1b or
W4 achieves CSS L4 > lightningcss and W3 is not needed. `W3` inserts after W1b
and before W5 when the CSS row misses and a same-tape fact/union attempt is
needed for ADMIT or for FIXPOINT:

`W0 -> W1a -> W1b -> W2 -> W3 -> W4 -> W5`.

W2 precedes W4 because the pin forbids new SIMD admission until
`escape_mask_64` is verified and resolved. W3 precedes W4 only when the chosen
ASM consumer depends on W3's same-tape facts; otherwise W3 and W4 are
topologically independent after W1b/W2, but the orchestrator should run them
serially to avoid shared generated-runtime, bench, and gate-file races. The
manifest is seven wave identifiers at maximum, below the skinny <=12 ceiling.

### Fallback Order

Sheets and BBNF-self are not W1/W2 alternatives in the opening plan. They may
enter only after W2 records a measured CSS L4 redress attempt as BLOCKED or
REJECTED. If that happens, S-P3/SPEC must add an explicit fallback wave in the
next folded cycle or next tranche; P3-B does not hide a Sheets/BBNF fallback
inside CSS redress.

### Candidate-to-Wave Binding

| Candidate family | First eligible wave | Sequencing reason |
|---|---|---|
| `GrammarConfig` / generated metadata surface | W1a | CSS emission is illegal until generic JSON policy moves into generated metadata/templates. |
| CSS L4 generated runtime, canonical fact stream, oracle, lightningcss comparator | W1b | The pin's ADMIT bar cannot be evaluated until the CSS row exists and gate/report consumes the comparator. |
| `ESCAPE_MASK_64_FIX_GATE` | W2 | Correctness prerequisite before any new SIMD-backed admission; not a row mover. |
| Same-tape CSS fact/kind/retained-view union | W3 | P2-D has no pre-baseline selectable union; the material differential exists only after W1b creates CSS fact-stream/equality consumers. |
| Byte-set/classifier/layout/FIRST dispatch (`C1`, `C6`, `pt_byte_set_run_skip`, generated FIRST/follow) | W4 unless used in scalar W1b | Needs generated CSS byte sets and same-wave caller; cannot ship as an orphan classifier. |
| Bounded string span, string-special scan, escaped segments, hex quartet x4 (`C4`, `C5`) | W4 | Needs W2 correctness for SIMD/string-region routes and CSS string/identifier/URL consumers. |
| Digit-run/UDOT numeric span (`C3`) | W4 | Needs CSS number/dimension/percentage consumer and full digit-run checkasm beyond the smoke proof. |
| PMULL prefix-XOR or CSSC CTZ narrow support (`C7`, `C8` support-only) | W4 only after CHALLENGE | Category-unblocked, but REDRESS 88/89 default-body routes remain historical rejects; only a narrow named consumer is eligible. |
| Output-plane digest/fact reporting | W1b/W5 as oracle/report support only | Parser-candidate-ineligible by itself; cannot close without generated parser/equality/comparator evidence. |
| Tape capacity, sparse flag lookup, retained cursor skip diagnostics | Not in PIN-V1 manifest unless W2 profile makes them hot | P2-D marks them diagnostic-only today. |

## §3 - Falsifiability Binding

W0 revalidates the opening JSON and telemetry surface. It must keep the current
family counts coherent: JSON `parse_only` remains diagnostic; JSON
`direct_to_struct` is guard/routed ledger; JSON `real_typed_struct` is a guard
surface; CSS L4 has no admitted row before W2.

W1 passes only if:

- `GrammarConfig` or equivalent generated metadata removes the seven named
  Lock-14 leaks from generic code paths;
- generic-crate scan finds no grammar-name branch, public per-grammar generic
  API, or hand-written CSS/Sheets/BBNF runtime module outside generated output;
- JSON direct and typed guard rows hold or measured demotion is recorded;
- W2 can legally emit CSS L4 without JSON-only provider gates.

W1b passes its baseline gate only if the CSS L4 row records:

- generated Track 1 source path and Mbps;
- independent Track 2/oracle path and Mbps;
- lightningcss comparator command/artifact and `lightningcss_mbps`;
- strict equality on the canonical CSS fact stream;
- same corpus, output plane, host, build flags, feature mask, run id, sample
  count, generated LOC/module byte size, profile artifact, and gate result.

W1b records ADMIT eligibility only when:

`generated_css_l4_track1_mbps > lightningcss_mbps + 1`.

Equality at `lightningcss_mbps + 1` is a miss. A lower but strictly equal row is
a measured CSS attempt and a baseline for W3/W4, not an ADMIT close.

W2 passes only if strict SIMD/checkasm parity reproduces and resolves the
`escape_mask_64` falsifier, including carry-in/out coverage and corpus parity.
No new SIMD row may dispatch while W2 is open.

W3 passes or rejects on measurement. Its gate must name:

- the REDRESS 96/97/98 material differential;
- the exact same-tape fact/kind/storage shape;
- the CSS fact-stream or retained-view consumer that reads the fact in the same
  commit;
- strict CSS equality and final CSS Track 1/lightningcss comparison;
- JSON guard state.

Any retained structural-position vector, parser-owned cursor list, whitespace
bitmap, aux projection column, decoded-byte sidecar, public substrate API, or
`UnionTape` falsifies W4 regardless of throughput.

W4 passes or rejects on measurement. Its gate must name:

- the selected ARMv9.2 primitive or support body;
- scalar reference and strict checkasm/parity;
- isolated same-host micro-proof;
- same-wave CSS L4 or JSON-guard hot-leaf consumer;
- REDRESS 88/89/90 material differential where adjacent;
- final CSS Track 1/lightningcss comparison;
- the disposition of all five aarch64 orphans.

An orphan production primitive remaining after W4 falsifies close even if the
CSS row is faster than lightningcss.

W5 admits only if the W1b/W3/W4 final CSS row clears
`generated_css_l4_track1_mbps > lightningcss_mbps + 1`, strict equality and
oracle independence hold, Lock 14 and Lock 16 hold, zero-orphan disposition
holds, and JSON guards hold or have measured REDRESS demotions.

W5 records FIXPOINT only if ADMIT is measured-uncloseable and all of these are
present in the closing tranche: measured CSS L4 attempt, measured new union
attempt (W3), measured new ASM-gen attempt (W4), zero orphans, and REDRESS
evidence for every miss.

Seed JSON direct guard floors carried from `SYNTHESIS.md`:

| Row | Track 1 maintain | Track 2 maintain |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Seed JSON typed guard floors:

| Row | Track 1 maintain | Track 2/oracle maintain |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

## §4 - Pre-Blocked Routes

Global pre-blocks that remain closed:

- `parse_only` SOTA admission.
- Sheets or BBNF-self before a measured CSS L4 redress attempt.
- CSS admission against `ceil(baseline_mbps * 1.01)` or any floor other than
  `lightningcss_mbps + 1`.
- Generic-crate JSON policy, grammar-name branches, hand-only non-JSON
  witnesses, stale report lanes, or producer-only telemetry as
  grammar-generalization proof.
- New BBNF directive, BIR variant, BackendShape variant, public substrate API,
  parser-owned sidecar, second retained substrate, or x86 implementation work.
- SIMD admission before the `escape_mask_64` correctness blocker is verified
  and resolved.
- Orphan production aarch64 primitives at close.

Category-level unblocks carried from the user pin:

- Union/substrate routes adjacent to REDRESS 96/97/98 are eligible only as new
  material-differential attempts with CHALLENGE, same-tape ownership,
  scalar/parity/equality proof, and same-wave consumer.
- ASM-gen routes adjacent to REDRESS 88/89/90 are eligible only as new
  material-differential attempts with scalar reference, checkasm/parity,
  micro-proof, feature-gated fallback, and same-wave consumer.

Per-wave pre-block map:

| Wave | Pre-blocked routes |
|---|---|
| W0 | No row movement by telemetry accounting alone; no W0-clamped JSON direct admission; no producer-only fields. |
| W1 | No CSS emission while JSON policy remains in generic templates; no grammar-name branch or hand-written CSS runtime as a shortcut; no JSON guard demotion without measurement. |
| W1b | No Sheets/BBNF fallback; no lightningcss-free comparator; no CSS baseline without independent oracle/Track 2; no generated-size overflow without tracing O(N) growth source. |
| W2 | No throughput credit; no SIMD admission on partial falsifier coverage; no waiver of strict checkasm/corpus parity. |
| W3 | No retained side vector, class column, cursor list, whitespace bitmap, aux table, decoded sidecar, `UnionTape`, public substrate API, or unlabeled replay of REDRESS 96/97/98. |
| W4 | No orphan kernel; no default PMULL prefix-XOR body as row movement; no global CSSC CTZ/bulk replacement; no UDOT/hex/string proof-only reuse; no cache hint as parser primitive. |
| W5 | No close on future-phase promises; no ADMIT without `> lightningcss_mbps + 1`; no FIXPOINT without measured CSS + union + ASM-gen attempts and zero orphans. |

## §5 - Sources

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A7-sheets-execution-scout.md`
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
