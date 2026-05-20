# SK-V12 Pass Alpha - Alpha-D Validated / Invalidated Ledger

Pass: Pass Alpha re-bracket.
Agent: alpha-D.
Date: 2026-05-20.
Scope: SK-V11 -> SK-V12 validated / invalidated / demoted / still-open ledger
under `USER-PIN-W1-CSS-L4-SOTA.md`.
Output: this file only.

## Contract Boundary

This ledger supersedes the pre-pin Alpha-D lane wherever the 2026-05-20 user
pin amends SK-V12. The original Pass Alpha SK-V11 -> SK-V12 packet correctly
seeded SK-V12 from SK-V11 REDRESS 120, but its W1 target ordering is obsolete:
CSS L4 is now authoritative, Sheets and BBNF-self are post-CSS-redress
fallbacks only, and the close bar is `lightningcss_mbps + 1` on the same
corpus, same output plane, strict equality, and same-host independent oracle.

The measured SK-V11 close facts still carry. REDRESS 120 closes SK-V11 as a
measured fixpoint, not as direct `GO` and not as a grammar-generalization
admission. REDRESS 119 remains the direct residual row authority. The user pin
does not erase those measurements; it changes the campaign target and reopens
the union-substrate and ASM-gen architectural categories for new, materially
different attempts.

## Source Map

Required authority read for this Alpha-D re-bracket:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 96-120
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A7-sheets-execution-scout.md`
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`

Pin commit anchor: `a1f6496b` (`docs(sk-v12-user-pin): CSS L4 authoritative,
>lightningcss target, union+ASM-gen unblocked`).

W0 gate-lock anchor carried from the running SK-V12 packet: `f788eb97`
(`feat(sk-v12-waveW0): admit telemetry lock companion gate surface`). Under
this re-bracket W0 may be re-validated; it is not re-authored here.

## Validated Ledger

### V1 - Banked JSON Guard State Still Binds

The JSON guard surface is banked and remains the guard rail for SK-V12 under
the pin. It is not the primary admission target.

| Family | Current state | SK-V12 role |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only; no SOTA admission |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | admitted guards plus REDRESS 119 fixpoint residuals |
| `real_typed_struct` | 7 `A / GO` | product-plane guard surface |
| Overall | `N-direct / NoGo` | unchanged seed outcome |

Banked direct guard rows from `skinny/RESULTS.md`:

| Row | Track 1 Mbps | Track 2 Mbps | sonic direct Mbps | Guard floor carried by SK-V12 synthesis |
|---|---:|---:|---:|---:|
| `citm_catalog/direct_to_struct` | 18563 | 17787 | 15530 | 18191 / 17431 |
| `apache_builds/direct_to_struct` | 11254 | 10189 | 10995 | 11028 / 9996 |
| `marine_ik/direct_to_struct` | 8938 | 9437 | 8473 | 8759 / 9248 |
| `unicode_basic/direct_to_struct` | 2299 | 2227 | 2353 | 2253 / 2182 |

Banked typed guard rows:

| Row | Track 1 Mbps | Track 2/oracle Mbps | sonic typed Mbps | Guard floor carried by SK-V12 synthesis |
|---|---:|---:|---:|---:|
| `twitter/real_typed_struct` | 17740 | 15912 | 15010 | 17385 / 15593 |
| `citm_catalog/real_typed_struct` | 30539 | 17675 | 20726 | 29928 / 17321 |
| `apache_builds/real_typed_struct` | 8478 | 6892 | 8106 | 8308 / 6754 |
| `github_events/real_typed_struct` | 11871 | 12275 | 12224 | 11633 / 12029 |
| `update_center/real_typed_struct` | 11851 | 10358 | 12467 | 11613 / 10150 |
| `mesh/real_typed_struct` | 9403 | 7897 | 8923 | 9214 / 7739 |
| `marine_ik/real_typed_struct` | 11788 | 10096 | 9010 | 11552 / 9894 |

Carry-forward rule: SK-V12 may demote a JSON guard only by measured gate
disposition. The CSS L4 >lightningcss target is first priority, but JSON guard
floors cannot silently regress.

### V2 - REDRESS 119/120 Fixpoint Evidence Remains Valid

REDRESS 119 still closes the 13 direct residual rows as a measured SK-V11
fixpoint. REDRESS 120 still records the SK-V11 close as unchanged
`N-direct / NoGo`. The pin re-prioritizes CSS and unblocks categories; it does
not make old JSON direct rows unexamined backlog.

Carry-forward rule: a JSON direct residual may re-enter only with fresh profile
evidence, a material differential beyond REDRESS 114-119, strict comparator
evidence, independent Track 2/oracle, and same-wave gate consumption.

### V3 - W1a Non-JSON Gate/Report Lane Remains Useful Infrastructure

REDRESS 111 remains validated as a companion non-JSON report lane. It is not a
generated Track 1 baseline and not a grammar-generalization admission.

Carry-forward rule: W1 CSS L4 must produce a real generated Track 1 parser row,
same-plane independent oracle or Track 2, strict equality, finite same-host
throughput, run/build/host/sample provenance, and gate consumption. W1a-style
producer-only or placeholder evidence is insufficient.

### V4 - Strict Comparator / Output-Plane Discipline Still Binds

The strict-vs-strict comparator gate survives the pin. The new CSS target is
strictly stronger: generated CSS L4 must beat `lightningcss_mbps + 1` on the
same corpus and same output plane. Permissive comparators, absent sidecars,
parse-only wins, and output-plane relabels remain non-admissible.

### V5 - W0 Telemetry Lock Is Revalidatable, Not Redone Here

W0 telemetry/gate lock at `f788eb97` remains a valid SK-V12 packet artifact
unless a later gate check falsifies it. This alpha-D lane records that W0 must
be reconciled against the pin, but does not re-run or edit W0 artifacts.

## Invalidated Ledger

### I1 - Old Sheets-First W1 Selection Is Invalidated

The pre-pin W1 V2 plan at commit `e24a7e01` selected Sheets as the immediate
generated non-JSON baseline. The user pin invalidates that selection. Sheets is
now fallback-only after a CSS L4 redress attempt fails; it is not
preflight-equivalent to CSS.

The tactical Sheets scout remains useful as fallback research, but not as W1
authority. Its `sheets_direct.rs` plan also carries a Lock 14 risk: a
grammar-named codegen module in a generic crate is not admissible as the
campaign's generic route.

SK-V12 implication: S-P3 must re-derive W1 around CSS L4, semantic parity with
lightningcss, and the `lightningcss_mbps + 1` floor. Sheets can appear only as
a post-CSS-redress fallback with its own measured REDRESS disposition.

### I2 - Baseline-Only Threshold `ceil(baseline_mbps * 1.01)` Is Invalidated

The original SK-V12 close bar admitted a generated non-JSON baseline and then
required a selected-baseline intervention to clear `ceil(baseline_mbps * 1.01)`.
The pin raises the close target: CSS L4 must beat lightningcss, not merely
itself.

SK-V12 implication: any SPEC or dispatch text that still uses baseline-plus-1%
as the material close condition is stale for the CSS row.

### I3 - "CSS Preflight Failure Equals Sheets Fallback" Is Invalidated

The pin distinguishes CSS redress attempt failure from preflight failure.
Sheets and BBNF-self are fallbacks only after the campaign makes and records a
CSS L4 redress attempt. Existing CSS owner-surface blockers from REDRESS 112
are real, but they route into a CSS unblock/redress wave rather than automatic
fallback.

### I4 - Generic-Crate JSON Policy Leaks Block CSS Emission

The value/API audit identifies seven Lock 14 leaks in the JSON generated
template surface: structural alphabet, value dispatch, string escape/quote
policy, number policy, key quoting, `OffsetFlags` semantics, and `JsonSink`.
Before CSS L4 emission is legal, W1 must extract the grammar policy surface
through a `GrammarConfig`-style trait and per-grammar generated config modules.

Required precondition:

- `GrammarConfig` owns escape and number policy.
- Generated CSS L4 metadata owns structural alphabet, dispatch, string/escape,
  number, and sink/view policy.
- Generic crates do not add grammar-named match arms, grammar-named feature
  flags, or JSON policy copied under a neutral name.

SK-V12 implication: CSS L4 redress cannot legally emit by cloning or widening
`json_provider` / `JsonSink`. The first legal implementation path must clear
the `GrammarConfig` precondition and the Lock 14 §2.1 scan.

### I5 - `escape_mask_64` NEON Is A Correctness Blocker

The totality fold scout records a verified `escape_mask_64` NEON correctness
bug on backslash-run boundary cases. Falsifier:
`0xCAFEF00DBAADF00D` (xorshift seed, iter 0, 128-byte JSON-pool). Root cause is
state handoff confusion between `new_carry` and `scan_json_tail`'s `escaped`
argument.

SK-V12 implication: no new SIMD admission may land before this is verified and
resolved through the Lock 16 checkasm gate. Any SIMD wave must carry scalar
reference parity, checkasm/differential coverage, corpus parity, and a
same-wave consumer.

### I6 - Orphan SIMD Is Invalid At Campaign Close

The aarch64 audit identifies five orphan or effectively orphaned primitives:
`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
`byte_context`, and `cache_hints`. The pin makes zero orphan aarch64 primitives
a campaign close target.

SK-V12 implication: each orphan must either be removed/demoted as inventory or
wired to a same-wave consumer with scalar reference and checkasm. Leaving an
orphan at close fails the pin's SIMD-utilization target.

## Category Reopened Ledger

### R1 - Union / Substrate Category Reopened, Historical Implementations Still Rejected

The user pin rescinds the category-level pre-block on Rust union substrate /
event-model / class-column / streaming cursor / retained structural projection
routes. REDRESS 96, 97, and 98 remain historical measurements:

- REDRESS 96 rejected the full class-column substrate plus move-consumed
  structural-index implementation. It was correctness-green before measurement
  but failed every W3 must-improve row and every W10b maintain row.
- REDRESS 97 rejected the materially different allocation-free streaming
  cursor variant. It was correctness-green before measurement but again failed
  every W3 must-improve row and every W10b maintain row.
- REDRESS 98 retired the SK-V9 `G-W3-UNION-SUBSTRATE` gate for that contract.

New union attempts are admissible only if they cite REDRESS 96/97/98, name the
material differential, pass CHALLENGE, provide scalar/reference or parity
coverage, land a same-wave consumer, and measure against a CSS L4 hot leaf or a
JSON guard hot leaf. For campaign fixpoint closure, at least one new measured
union implementation attempt must be cited in REDRESS for the closing tranche.

### R2 - ASM-Gen Category Reopened, Historical Implementations Still Rejected

The pin rescinds the category-level pre-block on ASM-gen routes tied to
REDRESS 88, 89, and 90. The rejected implementations remain historical:
PMULL prefix-XOR default body, CSSC CTZ bulk consumer, and canary hardening as
row movement are not silently admitted.

New ASM-gen attempts are admissible only with micro-prove-first evidence,
scalar reference, checkasm/differential parity, same-wave hot-path consumer,
and measured row or guard impact. Candidate surfaces include ARMv9.2-A NEON
TBL/TBX, CSSC, PMULL, UDOT, and SHA3 EOR3 only where a CSS L4 hot leaf or JSON
guard hot leaf consumes the primitive. x86 remains out of scope.

For campaign fixpoint closure, at least one new measured ASM-gen
implementation attempt must be cited in REDRESS for the closing tranche.

## Demoted Ledger

| Item | Old status | Pin-adjusted status | Handling |
|---|---|---|---|
| Sheets W1 baseline | Immediate W1 target in plan V2 | Fallback only after CSS redress attempt | Preserve scout as fallback research; no dispatch authority. |
| BBNF-self W1 baseline | Fallback after CSS/Sheets preflight | Fallback only after CSS redress attempt, then Sheets disposition | Do not use as CSS avoidance. |
| JSON direct residual rows | Exhausted SK-V11 fixpoint | Guard/fixpoint ledger under CSS-first campaign | Reopen only with fresh material evidence. |
| W1a non-JSON lane | Useful schema/report support | Non-admitting infrastructure only | Must be consumed by generated CSS row gate. |
| Union substrate category | Pre-blocked by REDRESS 96/97/98 | Reopened category; prior variants rejected | Requires material differential plus measured attempt. |
| ASM-gen category | Pre-blocked by REDRESS 88/89/90 | Reopened category; prior variants rejected | Requires micro-proof, checkasm, consumer, measurement. |
| Orphan aarch64 primitives | Inventory/support | Close blocker unless removed or consumed | Target zero orphan kernels at close. |

## Still-Open Ledger For SK-V12 Under Pin

### O1 - CSS L4 Generated Baseline And >Lightningcss Admission

The primary target is a generated CSS L4 row with semantic parity against
lightningcss and Track 1 throughput greater than `lightningcss_mbps + 1`. The
row must include same-corpus, same-output-plane strict equality, independent
oracle/Track 2, gate-consumed provenance, Lock 14 cleanliness, and JSON guard
maintenance.

### O2 - GrammarConfig Extraction Before CSS Emission

CSS L4 emission is illegal until generic JSON policy leaks are extracted into a
grammar-neutral `GrammarConfig` surface plus generated per-grammar metadata.
This is a W1 precondition, not optional cleanup.

### O3 - `escape_mask_64` Correctness Repair Before SIMD Admission

The xorshift falsifier must be codified in checkasm/differential coverage and
the NEON state-handoff bug must be fixed before any SIMD wave can admit.

### O4 - Zero-Orphan SIMD Close Target

The close packet must report the final SIMD coverage state. Every aarch64
primitive must be either consumed with scalar/checkasm/same-wave evidence or
demoted/removed as inventory. Orphans at close are a pin failure.

### O5 - New Union And ASM-Gen Attempts For Fixpoint Closure

If CSS L4 >lightningcss admission cannot close, campaign fixpoint closure
requires one full Pass Alpha bracket with per-route REDRESS evidence and at
least one new measured union-substrate implementation attempt plus at least one
new measured ASM-gen implementation attempt in the closing tranche.

### O6 - JSON Guard Preservation

The JSON guard rows listed in V1 must hold or receive measured demotion
dispositions. `parse_only` remains diagnostic only.

## Alpha-D Re-Bracket Verdict

VALIDATED:

- SK-V11 close remains a measured fixpoint under REDRESS 120.
- REDRESS 119 remains the direct residual row authority.
- JSON direct/typed guard rows remain banked maintain gates.
- W1a non-JSON lane remains useful non-admitting infrastructure.
- Strict comparator, output-plane, Track 2/oracle, and gate-consumption
  discipline remain binding.

INVALIDATED / SUPERSEDED:

- W1 Sheets-first selection from commit `e24a7e01`.
- Any SK-V12 close condition based on `ceil(baseline_mbps * 1.01)` instead of
  `lightningcss_mbps + 1` for CSS L4.
- Any path that treats CSS preflight failure as equivalent to a CSS redress
  attempt.
- Any CSS emission route that leaves the seven JSON policy leaks unresolved.
- Any new SIMD admission before `escape_mask_64` correctness is verified and
  resolved.

REOPENED WITH MEASUREMENT REQUIREMENT:

- Union-substrate category: unblocked at category level, but REDRESS 96/97/98
  remain rejected implementations and must be cited by new attempts.
- ASM-gen category: unblocked at category level, but REDRESS 88/89/90 remain
  rejected implementations and must be cited by new attempts.

NEXT S-P3 INPUT:

Re-derive SK-V12 W1 as CSS L4 authoritative: first clear the GrammarConfig /
Lock 14 precondition, then land a generated CSS L4 row with lightningcss
semantic parity and `lightningcss_mbps + 1` admission bar. Carry
`escape_mask_64` as the SIMD blocker, zero-orphan SIMD as close target, and
the reopened union/ASM-gen categories as required measured-attempt routes for
any eventual fixpoint close.
