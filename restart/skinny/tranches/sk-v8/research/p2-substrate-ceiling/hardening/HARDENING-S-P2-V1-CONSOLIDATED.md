# HARDENING-S-P2-V1-CONSOLIDATED
Date: 2026-05-17.
Pass: S-P2 Research — substrate-ceiling cohort.
Cycle: V1.
Cycle verdict: REVISE.
ACCEPT-rate: 0/6 (0%).
Convergence status: not converged; no consecutive ACCEPT cycle.

## §1 — Lens Summary

| Lens | Raw disposition | Score | Blocking themes | Required V2 fold |
|---|---:|---:|---|---|
| CH1 CORRECTNESS | REVISE | 72 | Current P1/profile evidence is placeholder-only; strict/deferred/sidecar/SK-V6 rows are mixed; gates are qualitative. | Bind candidate claims to current W0/W3 rows, profile artefact ids, owners, thresholds, and strict same-run comparator planes. |
| CH2 GENERALITY | REVISE | 82 | StructuralAlphabet examples leak grammar semantics; SC-1/SC-2 wording risks JSON-shaped kind/sidecar policy. | Rewrite to fixed neutral structural roles, opaque `StructuralClass` ordinals, generated per-grammar byte-set data, and tape-internal facts only. |
| CH3 REGRESSION | REVISE | 82 | The union is distinct only if it replaces the tape; facts/parity/K-demotion language can reopen sidecars, PMULL/CTZ defaults, or hidden parse losses. | State cardinality=1, scan-written mandatory class state, no parser-patched aux/event tables, PMULL/CTZ remain blocked defaults, and demoted rows keep deltas. |
| CH4 COST | REVISE | 72 | W3 cost compresses a narrow class-column migration and a larger quote/backslash/parity/CostFacts union; same-wave consumers and Omega fork are underpriced. | Split narrow W3 from multi-wave string-index work; enumerate consumers, scalar/checkasm names, LOC/risk, rerun budget, and post-Omega/pre-Omega path. |
| CH5 HIDDEN COUPLING | REVISE | 82 | SC-4 retained bitmaps are sidecars as written; SC-3 `facts` and SoA wording lack containment. | Retained output must be only `Tape` offset/class/facts columns; masks are transient; `StructuralIndex` is moved and has no post-build API; SoA means internal co-indexed tape columns, not old columnar substrate. |
| CH6 ANTI-PAPER-CLOSE | REVISE | 68 | S-P2 research is promoted into W3 prescription before W0/W1 evidence, S-P3 planning, owner paths, revert protocol, and challenge acceptance. | Demote "prescribes/right move/closed" to "lead hypothesis/candidate/pre-blocked absent fresh evidence"; no W3 selection from SC-1..SC-6 alone. |

## §2 — Consolidated Critical Defects

1. **No current hot-leaf antecedent yet supports prescription.** The S-P2 contract rejects candidates without named P1 antecedents (`restart/prompts/skinny/PASS-2-RESEARCH.md:99`-`restart/prompts/skinny/PASS-2-RESEARCH.md:105`), while current rows still say `Hot leaf` is `unprofiled in W0b; no kernel prescription from this row` (`skinny/RESULTS.md:5`, `restart/skinny/tranches/sk-v8/HANDOFF.md:35`-`restart/skinny/tranches/sk-v8/HANDOFF.md:38`). CH1 flags SC-3/SC-4 overreach at `hardening/V1/CH1.md:19` and CH6 flags paper-promotion at `hardening/V1/CH6.md:22`.

2. **Strictness and comparator planes are mixed.** SC-4's string-density table uses `n/a` sonic rows and SK-V6 deltas as support (`SC-4-string-plane-gap.md:174`-`SC-4-string-plane-gap.md:200`), but current RESULTS marks strictness `deferred` and comparator sidecars as planning signals (`skinny/RESULTS.md:5`-`skinny/RESULTS.md:42`, `skinny/RESULTS.md:219`). CH1 requires split strict same-run rows from sidecar/SK-V6 rows (`hardening/V1/CH1.md:21`, `hardening/V1/CH1.md:27`).

3. **The gates are not executable enough.** SC-1 asks structural-dispatch self-time to "collapse toward zero" (`SC-1-offset-tape-teardown.md:281`-`SC-1-offset-tape-teardown.md:286`), and SC-4 asks the string-fraction knee to move toward 1.0 (`SC-4-string-plane-gap.md:293`-`SC-4-string-plane-gap.md:296`). SPEC already requires selected rows, maintain budgets, scalar/checkasm parity, and substrate-cardinality proof (`restart/skinny/tranches/sk-v8/SPEC.md:416`-`restart/skinny/tranches/sk-v8/SPEC.md:429`).

4. **Lock 14 is not yet clean in SC-6.** SC-6 generic claims say generic crates consume `&StructuralAlphabet` and never name grammar (`SC-6-lock1-amendment-generalisation.md:360`-`SC-6-lock1-amendment-generalisation.md:367`), but its instance tables use `Open(Object)`, `DeclTerminator`, `RangeDelim`, and similar grammar-semantic roles (`SC-6-lock1-amendment-generalisation.md:370`-`SC-6-lock1-amendment-generalisation.md:405`, `SC-6-lock1-amendment-generalisation.md:424`-`SC-6-lock1-amendment-generalisation.md:437`). CH2 identifies this as the blocker and points to SC-6's own neutral-role repair (`hardening/V1/CH2.md:19`, `SC-6-lock1-amendment-generalisation.md:619`-`SC-6-lock1-amendment-generalisation.md:630`).

5. **Sidecar language can re-enter rejected routes.** SC-2 calls `(flag_cursor, flag_value)` a side-channel (`SC-2-two-stage-sota.md:263`-`SC-2-two-stage-sota.md:270`); SC-3 admits sparse `facts` with recovery/layout side facts (`SC-3-union-substrate-design.md:147`-`SC-3-union-substrate-design.md:153`, `SC-3-union-substrate-design.md:238`-`SC-3-union-substrate-design.md:240`); SC-4 asks for retained quote/backslash/parity bitmaps (`SC-4-string-plane-gap.md:270`-`SC-4-string-plane-gap.md:283`, `SC-4-string-plane-gap.md:339`-`SC-4-string-plane-gap.md:342`). REDRESS blocks retained parser-side aux/projection routes (`skinny/REDRESS.md:756`-`skinny/REDRESS.md:767`, `skinny/REDRESS.md:807`-`skinny/REDRESS.md:813`), and CH5 marks SC-4 as a Lock 1 blocker as written (`hardening/V1/CH5.md:19`-`hardening/V1/CH5.md:23`).

6. **W3 cost and consumer scope are under-specified.** SC-3 sizes the union at about +210 LOC and W3/medium risk (`SC-3-union-substrate-design.md:330`-`SC-3-union-substrate-design.md:387`), while SC-4 says CostFacts alone is about 830 LOC and the full tape-index union is multi-wave (`SC-4-string-plane-gap.md:350`-`SC-4-string-plane-gap.md:353`). SPEC caps W3 at 450 LOC, or 650 only with template parity (`restart/skinny/tranches/sk-v8/SPEC.md:200`-`restart/skinny/tranches/sk-v8/SPEC.md:219`). CH4 requires a narrow/multi-wave split and a full consumer list (`hardening/V1/CH4.md:19`-`hardening/V1/CH4.md:31`).

7. **S-P2 is selecting and sequencing too much.** S-P2 "selects nothing and sequences nothing" (`restart/prompts/skinny/PASS-2-RESEARCH.md:6`-`restart/prompts/skinny/PASS-2-RESEARCH.md:10`), yet SYNTHESIS and SPEC say W3 is no longer unprescribed or prescribed (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:159`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:164`, `restart/skinny/tranches/sk-v8/SPEC.md:390`-`restart/skinny/tranches/sk-v8/SPEC.md:403`), and HANDOFF repeats "prescribes W3 ahead of time" (`restart/skinny/tranches/sk-v8/HANDOFF.md:67`-`restart/skinny/tranches/sk-v8/HANDOFF.md:71`). CH6 requires demotion to lead hypothesis (`hardening/V1/CH6.md:28`-`hardening/V1/CH6.md:38`).

## §3 — Required V2 Fold Plan

**SC-1 — `SC-1-offset-tape-teardown.md`**

- Replace "node kind" / "kind tag" with opaque `StructuralClass` ordinal or fixed neutral role language; cite SC-3 as the canonical expression (`hardening/V1/CH2.md:29`, `SC-1-offset-tape-teardown.md:266`-`SC-1-offset-tape-teardown.md:271`).
- Reframe §3 as candidate research only: no implementation wave is selected here; S-P3/W3 must supply owner paths, same-wave consumer, revert protocol, and numeric thresholds (`hardening/V1/CH6.md:10`, `hardening/V1/CH6.md:38`).
- Replace the qualitative fusion gate with a W3 gate table: rows, workloads, comparator plane, strictness source, baseline Track 1/2 Mbps, threshold, maintain budget, profile artefact, hot leaf, owner file, pass/fail rule (`hardening/V1/CH1.md:27`).

**SC-2 — `SC-2-two-stage-sota.md`**

- Add exact upstream source anchors for sonic-rs, yyjson, asmjson, and simdjson claims, or downgrade claims to cited secondary research; reconcile sonic-rs as no persistent structural index unless exact source proves otherwise (`hardening/V1/CH1.md:29`, `SC-2-two-stage-sota.md:116`-`SC-2-two-stage-sota.md:147`).
- Rewrite `(flag_cursor, flag_value)` from "side-channel" to sparse tape-internal facts with same producer, same cursor domain, and no independent lifetime (`hardening/V1/CH2.md:31`, `hardening/V1/CH5.md:11`).
- Replace "exactly the right move" and one-wave language with a falsifiable candidate list: scalar reference, checkasm expectation, same-wave consumer placeholder, and "not selected until S-P3/W3 challenge" (`hardening/V1/CH6.md:11`, `SC-2-two-stage-sota.md:250`-`SC-2-two-stage-sota.md:270`).

**SC-3 — `SC-3-union-substrate-design.md`**

- Add a cardinality invariant in §2/§5: one retained `Tape`; `StructuralIndex` consumed by move into `Tape`; no query, clone, cache, attach, parser-owned cursor, or post-build API (`hardening/V1/CH5.md:29`, `SC-3-union-substrate-design.md:216`-`SC-3-union-substrate-design.md:225`).
- Define `class` as mandatory primary structural identity, scan-written only, never parser-patched; row-falsify against REDRESS 50 aux-side-table regressions (`hardening/V1/CH3.md:11`, `skinny/REDRESS.md:718`-`skinny/REDRESS.md:740`).
- Narrow `facts`: admitted legacy escape/control flags and EventTape-required recovery/layout facts only; ban density tables, quote caches, skip caches, profile counters, parser-owned slots, and per-consumer caches (`hardening/V1/CH5.md:31`-`hardening/V1/CH5.md:33`).
- Replace "structure-of-arrays" with "co-indexed internal tape columns"; explicitly distinguish from Lock 1's dead columnar SoA (`hardening/V1/CH5.md:23`, `restart/locks/LOCKS.md:34`).
- Split §5.2/§5.3 into Tier A narrow W3 and Tier B multi-wave string-index union; Tier A must include exact consumer list, scalar `compact_mask` oracle file/function, checkasm test name, LOC, risk, and rerun budget (`hardening/V1/CH4.md:25`-`hardening/V1/CH4.md:27`).
- Downgrade "W3 candidate" and REDRESS exoneration to "candidate requiring W3 challenge proof" (`hardening/V1/CH6.md:12`, `SC-3-union-substrate-design.md:376`-`SC-3-union-substrate-design.md:385`).

**SC-4 — `SC-4-string-plane-gap.md`**

- Split density evidence into strict same-run sonic rows, sidecar planning rows, and SK-V6 historical rows; only strict same-run rows may carry admission (`hardening/V1/CH1.md:13`, `hardening/V1/CH1.md:27`).
- Replace "closed to kernel work" with "pre-blocked absent fresh W0 evidence, same-wave consumer, no-regression gate, REDRESS citation, and challenge acceptance" (`hardening/V1/CH6.md:36`, `SC-4-string-plane-gap.md:263`-`SC-4-string-plane-gap.md:268`).
- Rewrite quote/backslash/parity bitmaps as transient masks or co-indexed tape facts only; no retained document-wide bitmap outside `Tape` may survive parse (`hardening/V1/CH5.md:27`, `SC-4-string-plane-gap.md:270`-`SC-4-string-plane-gap.md:283`).
- State PMULL prefix-XOR and CTZ/bulk remain rejected production defaults unless fresh W0/W3 evidence proves a narrowly gated same-wave consumer with no-regression (`hardening/V1/CH3.md:26`, `skinny/REDRESS.md:2535`-`skinny/REDRESS.md:2540`, `skinny/REDRESS.md:2573`-`skinny/REDRESS.md:2579`).
- Route full quote/backslash/parity + CostFacts/template work as follow-on/multi-wave unless SC-3's W3 plan explicitly fits the 650 cap and verification budget (`hardening/V1/CH4.md:33`, `SC-4-string-plane-gap.md:350`-`SC-4-string-plane-gap.md:353`).

**SC-5 — `SC-5-k-classification-adjudication.md`**

- Preserve `parse_only` demotion, but require substrate-guard rows to retain all strict comparator deltas and named residuals; twitter/unicode/distinct losses must remain visible (`hardening/V1/CH3.md:28`, `SC-5-k-classification-adjudication.md:293`-`SC-5-k-classification-adjudication.md:300`).
- Keep `tape_vs_tape` as W1/W0-plan telemetry or gate-binding work, not a W3 production consumer; name LOC, focused tests, comparator harness source, one allowed gate refresh, and same-run structural-index competitor requirement (`hardening/V1/CH4.md:29`, `SC-5-k-classification-adjudication.md:280`-`SC-5-k-classification-adjudication.md:287`).
- State `tape_vs_tape` cannot support SOTA admission until same-run structural-index competitor rows exist (`hardening/V1/CH1.md:33`, `SC-5-k-classification-adjudication.md:307`-`SC-5-k-classification-adjudication.md:310`).

**SC-6 — `SC-6-lock1-amendment-generalisation.md`**

- Rewrite §4.1-§4.4 to fixed neutral roles only: `Open`, `Close`, `Separator`, `PairDelimiter`, `StringDelimiter`, `Terminator`, `AtomBoundary`, plus data-only `EscapeKind` (`hardening/V1/CH2.md:25`-`hardening/V1/CH2.md:27`, `SC-6-lock1-amendment-generalisation.md:623`-`SC-6-lock1-amendment-generalisation.md:630`).
- Amend SC-6-L1-R1 text after that rewrite: generated per-grammar byte-set table plus fixed neutral role ordinals; grammar meaning interpreted only in generated grammar modules (`hardening/V1/CH2.md:27`, `SC-6-lock1-amendment-generalisation.md:263`-`SC-6-lock1-amendment-generalisation.md:280`).
- Add the explicit W3 fork: either W3 waits for Pass Omega ratification, or W3 includes Lock-1-as-written proof plus routed Omega residual (`hardening/V1/CH4.md:31`, `SC-6-lock1-amendment-generalisation.md:632`-`SC-6-lock1-amendment-generalisation.md:639`).
- Preserve same-wave consumer requirement: cursor, `ValueRef`, `path!`, and any retained-view/direct consumers touched must migrate in the same wave (`hardening/V1/CH4.md:27`, `SC-6-lock1-amendment-generalisation.md:593`-`SC-6-lock1-amendment-generalisation.md:602`).

**SYNTHESIS.md**

- Replace "W3's parse candidate is no longer unprescribed" with "lead hypothesis for W3 challenge after W0/W1 closure"; W0 confirms or falsifies the substrate finding executably (`hardening/V1/CH6.md:32`, `restart/skinny/tranches/sk-v8/SYNTHESIS.md:159`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:164`).
- Bind generic-crate proof to the neutral-role repair: CSS L4, Sheets, and BBNF-self must not require JSON structural roles to compile, lower, cost, or run (`hardening/V1/CH2.md:33`, `restart/skinny/tranches/sk-v8/SYNTHESIS.md:178`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:182`).
- Quote/import SC-6's cardinality test for W3/Omega posture: if the old offset append path survives beside a retained index, Lock 1 fails (`hardening/V1/CH3.md:30`, `restart/skinny/tranches/sk-v8/SYNTHESIS.md:221`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:230`).

**SPEC.md**

- In §6, replace "prescribes the W3 parse candidate" with "nominates the lead W3 hypothesis" and add: this nomination does not select W3; selection requires W0/W1 closure, fresh plan owner paths, same-wave consumer, revert protocol, measurement thresholds, and challenge acceptance (`hardening/V1/CH6.md:30`, `restart/skinny/tranches/sk-v8/SPEC.md:390`-`restart/skinny/tranches/sk-v8/SPEC.md:403`).
- Preserve the W3 cardinality gate, but make it explicit that `tape_vs_tape` telemetry is not the production same-wave consumer (`hardening/V1/CH4.md:21`, `restart/skinny/tranches/sk-v8/SPEC.md:423`-`restart/skinny/tranches/sk-v8/SPEC.md:429`).
- Keep W3 hard caps visible: 450 default, 650 only with template parity, scalar/checkasm if primitive, one full gate refresh ceiling (`restart/skinny/tranches/sk-v8/SPEC.md:200`-`restart/skinny/tranches/sk-v8/SPEC.md:219`).

**HANDOFF.md**

- Replace "prescribes W3 ahead of time" with "nominates a lead W3 hypothesis"; add an explicit line that SC-1..SC-6 authorize no W3 plan before W0/W1 closure (`hardening/V1/CH6.md:34`, `restart/skinny/tranches/sk-v8/HANDOFF.md:67`-`restart/skinny/tranches/sk-v8/HANDOFF.md:71`).
- Preserve no-dispatch posture: G-Alpha is still required; if closed, only W0 dispatches; W1-W6 need W0 closure and plan augmentation (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`-`restart/skinny/tranches/sk-v8/HANDOFF.md:7`, `restart/skinny/tranches/sk-v8/HANDOFF.md:160`-`restart/skinny/tranches/sk-v8/HANDOFF.md:167`).

## §4 — Accepted Core Claims To Preserve

- The live tape is offsets plus sparse flag vectors plus payload arena, not a 16-byte `TapeToken` stream (`skinny/crates/runtime/src/tape/mod.rs:90`-`skinny/crates/runtime/src/tape/mod.rs:97`, `hardening/V1/CH1.md:39`).
- `attach_structural_index` is a no-op; the generated parser dispatches from source bytes and revalidates structural bytes before emitting offsets (`skinny/crates/runtime/src/grammars/json/generated.rs:14`-`skinny/crates/runtime/src/grammars/json/generated.rs:17`, `skinny/crates/runtime/src/grammars/json/generated.rs:37`-`skinny/crates/runtime/src/grammars/json/generated.rs:43`, `skinny/crates/runtime/src/grammars/json/generated.rs:292`-`skinny/crates/runtime/src/grammars/json/generated.rs:306`).
- The structural scan exists and is currently unconsumed as the parser substrate; `structural_capacity_for` only uses positions length under `OneShotSimd` (`skinny/crates/runtime/src/grammars/json/scan.rs:22`-`skinny/crates/runtime/src/grammars/json/scan.rs:53`).
- Payload arena writes are not the current parse-plane explanation; current rows report `0 payload bytes` and `0/0 writes/allocations` (`hardening/V1/CH1.md:45`, `skinny/RESULTS.md:153`-`skinny/RESULTS.md:215`).
- The union thesis is viable only as replacement: structural projection becomes the tape, old scalar rediscovery is deleted, substrate cardinality remains one (`hardening/V1/CH3.md:46`, `SC-3-union-substrate-design.md:191`-`SC-3-union-substrate-design.md:225`, `SC-6-lock1-amendment-generalisation.md:176`-`SC-6-lock1-amendment-generalisation.md:236`).
- The grammar-neutral shape is byte-set/class-table/tape columns with opaque ordinals; generic crates must not encode JSON roles (`hardening/V1/CH2.md:39`-`hardening/V1/CH2.md:43`, `SC-3-union-substrate-design.md:265`-`SC-3-union-substrate-design.md:281`).
- `parse_only` is not a valid strict SOTA gate against DOM builders, but its deltas remain useful substrate-guard telemetry and must not hide losses (`hardening/V1/CH3.md:42`, `SC-5-k-classification-adjudication.md:90`-`SC-5-k-classification-adjudication.md:136`).
- SC-6-L1-R1 is an Omega candidate, not already-ratified law (`SC-6-lock1-amendment-generalisation.md:256`-`SC-6-lock1-amendment-generalisation.md:261`, `hardening/V1/CH5.md:49`).
- No SK-V8 implementation wave is authorized before G-Alpha; even closed G-Alpha authorizes W0 only (`hardening/V1/CH6.md:42`, `restart/skinny/tranches/sk-v8/SPEC.md:171`-`restart/skinny/tranches/sk-v8/SPEC.md:172`).

## §5 — Re-Challenge Criteria

- **CH1 ACCEPT** only if every candidate claim cites current row/profile evidence or is explicitly provisional; strict same-run evidence is separated from sidecar/SK-V6 signals; every falsifiability gate has rows, thresholds, owners, profiles, and pass/fail rules.
- **CH2 ACCEPT** only if all generic-substrate language uses fixed neutral roles, opaque ordinals, generated per-grammar byte-set tables, and non-JSON proof for CSS L4, Sheets, and BBNF-self.
- **CH3 ACCEPT** only if V2 proves no REDRESS route is reopened: the union replaces rather than shadows the tape, facts cannot become aux tables, PMULL/CTZ/string kernels remain pre-blocked defaults, and `parse_only` demotion preserves residual telemetry.
- **CH4 ACCEPT** only if narrow W3 and multi-wave string-index work are separately costed; same-wave production consumers are enumerated; scalar reference/checkasm names are supplied; Omega timing is a priced fork.
- **CH5 ACCEPT** only if one retained substrate remains, `StructuralIndex` has no post-build life, retained bitmaps disappear or become tape facts, and internal co-indexed columns are distinguished from forbidden columnar SoA.
- **CH6 ACCEPT** only if S-P2 language stays research-only: no "selected", "prescribed", "closed", or "right move" claims without live W0/W1 evidence, S-P3 plan, owner paths, revert protocol, measurement, and accepted challenge proof.

## §6 — Cycle Disposition

V1 hardening must be folded before any S-P3 dispatch. Under `ORCHESTRATOR.md` §3Z, hardening without folding is paper-hardening and the pass cannot advance (`restart/prompts/ORCHESTRATOR.md:117`-`restart/prompts/ORCHESTRATOR.md:132`).

No SK-V8 implementation wave is authorized by this consolidation. G-Alpha remains as the current packet states: user sign-off is required, and if closed it authorizes W0 only (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`-`restart/skinny/tranches/sk-v8/HANDOFF.md:7`). Do not dispatch waves.
