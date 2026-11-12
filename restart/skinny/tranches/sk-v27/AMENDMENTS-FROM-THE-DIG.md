# Amendments from the dig: proposed LEDGER rows for sk-v27

Written 2026-11-11 by the final seat of the archive dig. Source: `ARCHIVE-DIG-2026-11-11.md`, beside this file (cited below as DIG §n).

**Status of every row: PROPOSED.** These are proposals for the orchestrator to rule and fold at the LW3 R0 checkpoint. Nothing here edits `LW3-ORDER.md`, `PLAN.md`, `RULINGS.md` or `LEDGER.md`, and no running LW3 seat needs to stop for it. Rows that touch a unit already running in LW3 (DIG-CSP-B4b, DIG-G3, DIG-K-B6) are worded as additions for the LG-3 gate or the next wave's order, not as changes to a seat's current work.

**How to fold.** Each row is written to drop into `LEDGER.md` as `| 2026-11-11 | **<id> <title>** <text> | <status> |`. Shorthand: `P` = `PLAN.md`, `O` = `LW3-ORDER.md`, `L` = `LEDGER.md`, `R` = `RULINGS.md` (at `origin/sk-v27-lw3`), `CC` = `evidence/LW1.COMP/CRATE-CENSUS.tsv`. "RED today" is the reading the dig took by reading the tree; no seat built or ran anything. A row marked † needs its reader built by the unit that owns it.

**The owner's words these rows serve.** "The CSP and e-graph items are hard requirments" (2026-11-11 22:30). "this was a megamegatranche with MANY conestellation and check point reqs, like besting simdjson in full generality, full CSP implementation and optimization passes, backends for TS and Rust with IR and generalized IR, a tape union facility, etc." and "NO shortcuts. Full and grand implementations." (2026-11-11).

---

## 0. Act first: custody

| Id | Row | Status |
|---|---|---|
| DIG-CUST-1 | **`bounded_sum` custody.** The propagator, its tests and two examples are uncommitted in three workflow worktrees under `bbnf-lang/.claude/worktrees/`: `wf_63419230-36d-28` (staged; 168 lines, 7 tests, `examples/scan_plan_budget.rs`) and `wf_63419230-36d-30` (staged; 178 lines, 6 tests) at `d2b32f6e3`; `wf_fce2ec2b-53a-32` (unstaged; the same 178 lines, 6 tests, `examples/scan_plan_solve.rs`) at `ce51c36fc`. LW1 custody captured none of it, and its old path `crates/csp-solver` is gone. Work: commit all three variants to a pushed custody ref (`sk-lineage/bounded-sum-custody`) with the base commit of each recorded, then port to upstream `csp-solver` under LW5.COMP-3. Falsifier: `git log --all --oneline -- '**/bounded_sum.rs'` is empty today (RED); GREEN when it lists a commit on a pushed ref and the three source hashes match `~/.dev-logs/bbnf-lockstep/archive-dig/custody-bounded-sum/SHA256SUMS`. Must not: clean `.claude/worktrees/` before this row is GREEN. | PROPOSED, urgent |
| DIG-CUST-2 | **The fail-closed `csp_strategy` rewrite is ruled before LW5.** `d2e169794` on `origin/sk-lineage/dirty-preserve` (+2,017/−1,225 over 12 files; adds `constraints/token.rs`) is the cure for the silent fallback. Ruling X-6 sets its fate at LW7, after LW5 builds the organ. Work: adjudicate it at LW4 entry (adopt, adopt in part, or reject with reasons). Falsifier: RULINGS carries a row naming `d2e169794` before the LW5 order seals. | PROPOSED |

---

## 1. Guard rules against the recurring failure causes

Each guard names the cause it blocks (DIG §2) and a reader a gate seat can run.

| Id | Row | Status |
|---|---|---|
| DIG-G1 | **Existence of the organs is not on trial at any wave (hard requirement; blocks W13).** No trial verdict, at any wave, removes the CSP stage, the e-graph stage, the shape-mining stage or a kernel slot from the spine. A LOSS or UNDECIDED may switch one named rewrite family, constraint family or kernel variant off by default; it opens a redesign unit with a re-trial. A NOISY record decides nothing in either direction. Amends: `designs/compiler.md:32` (C1-4), I6, the "(only if CB-T kept …)" guards at `:182-183`, CB-13 `:189`, CB-14 `:190`; `P:227` (LW5.COMP-2); `designs/rust-perf.md:131-132` (G-K1, G-Q1); `designs/programme.md:414` (O012); `bench/PREREG.md:135`. Extends delegate ruling OWN-1 (`R:130`), which covers the LW3 port only. Falsifier: `git grep -nE 'simpler organ wins|keep/delete|trial losers|or the kernel is deleted|off by default or removed' <wave-head> -- restart/skinny/tranches/sk-v27 bench/PREREG.md`; every hit is amended text citing this row. RED at `28c09a0a5`. LG-g reads it at every gate. | PROPOSED; owner ruling O-1 |
| DIG-G2 | **Amendment-9 units are callers (blocks W8).** Either the fourteen amendment-9 unit ids are written into `PLAN.md`, or ruling Z-32 / LW2 amendment A5 (`L:182`) is amended so that a ROUTE may cite any unit in LW2–LW7 that this ledger admits. Until then a substrate whose only consumer is one of those units can only be fated DELETE by `crates/ir/tests/substrate_audit.rs:99`. Falsifier: `git grep -cF <id> -- PLAN.md` ≥ 1 for each of LW4.PERF-2, LW4.COMP-4, LW4.CON-2, LW5.COMP-1-OPTIR, LW5.COMP-3, LW7.CON-1-REQ and the eight smaller ids (all 0 today), or the Z-32 amendment row exists. | PROPOSED; owner ruling O-6 |
| DIG-G3 | **A paired perf cell on every substrate change (blocks W2).** Any change to the tape, the emitter's output shape, the dispatch substrate or a kernel carries, before merge, a paired cell against the AU table (`docs/benchmarks/post-AU.json`; tape-only and decoded planes kept apart) on JSON, CSS, BBNF and Sheets, in bbnf-lang and in parse-that. A regression beyond the floor blocks the merge. This widens LW3.CI-IAI-0/1, which reads `json_callgrind` on bbnf-lang PRs against the previous head only. Falsifier: a seeded +2 % regression on a CSS cell and on a parse-that kernel cell is refused by CI (neither job exists today: RED). Must not: baseline against the previous head alone, since a slow drift passes. | PROPOSED, for the LW4 order |
| DIG-G4 | **No substrate before its consumer, and no deletion of a named requirement on a zero-caller census (blocks W1, W8).** A unit that adds a surface names the unit that consumes it in the same wave, and its falsifier reads the consumer. A zero-caller census may not delete anything a DIG row, an owner quote in DIG §5–§7 or an amendment-9 row names; it files a ROUTE or an ask instead. Falsifier: the census checker refuses a DELETE fate for a path listed in `DIG-CARRY.tsv`† (the carry list of DIG-H1). | PROPOSED |
| DIG-G5 | **No silent fallback, anywhere (blocks W6, W7).** Budget exhaustion in the CSP, a failed saturation in the e-graph, or a kernel tier that is not selected returns a typed result that the caller must read, and fails the build where the plan depends on it. Falsifier: `git grep -n 'falling back to per-variable trivial pick'` = 0 (RED: `csp_strategy/mod.rs:612-618`); `git grep -n 'let _ = .*propagate()\|let _report'` = 0 on the spine and root; a forced-budget regen exits non-zero. Restores programme IF-2 (`designs/programme.md:233-236`: "Silent fallback is forbidden", an `Exhausted` certificate), which was cut before PLAN was formed. | PROPOSED |
| DIG-G6 | **Claims name their plane, unit, run and build (blocks W11).** Every figure in a ledger row, a commit message or a record carries: the value plane (P1–P7) on both sides, MB/s computed from `ns_per_byte` (megabits never printed), same-run and same-harness for both arms, the allocator, the LTO and debug settings, and the load average. A cross-plane, cross-run or cross-harness ratio is labelled as such and certifies nothing. Falsifier: `admit.py` refuses a cell missing any of these fields (seeded: a lazy plane against a DOM; two `twitter.json` hashes). Corrects the record: "76 % / 94 % of sonic-rs" at AU is cross-plane (same plane: 0.52× / 0.61×); R15's thin-LTO figure is confounded. | PROPOSED |
| DIG-G7 | **Every re-founding carries a requirements crosswalk (blocks W13).** A new order, plan or spec opens with a table that maps each row of DIG §5–§8 and of this file to a unit, or to an owner ruling that retires it. A row with neither blocks the seal. Narrowing a requirement is an owner act: an agent or delegate may propose it as an ask and may not write it into a plan. Falsifier: the seal seat's checker lists `unmapped=0`. Also closes the open promise at `L:171` (the crosswalk for design text cut at dispatch; none exists). | PROPOSED |
| DIG-G8 | **A gate may not pass by exclusion, and a label reports what ran (blocks W12).** A lint or grep gate carries no exclusion list, or each excluded path has a dated ruling. The dispatch label is written by the kernel that executed. Falsifier: a seeded excluded violation turns the generality gate RED; a trace on corpus bytes shows the label equals the variant run (see DIG-K-B6). | PROPOSED |
| DIG-G9 | **Model receipts on judgment seats (blocks W15).** Every seat that rules, adjudicates or audits records the model that served it, and a gate reads the census. Falsifier: the gate seat's census shows 0 judgment seats without a receipt (sk-v24's final pass: 49 of 60 on the wrong tier). | PROPOSED (standing law; made a gate row) |
| DIG-G10 | **Custody before work, pushed (blocks W17).** A wave does not close while any unit's work lives only in a worktree, a local branch or `/tmp`. Falsifier: the gate seat lists dirty and unpushed worktrees, including `.claude/worktrees/*`, with `uncaptured=0`. | PROPOSED |
| DIG-G11 | **Code closes a wave, not documents (blocks W10, W14).** A unit deferred from one wave to the next carries its original falsifier and a count of deferrals; a third deferral is an owner ask. No unit closes on a document alone where its falsifier names code. Falsifier: the ledger checker lists deferral counts; any ≥ 3 without an ask row is RED. | PROPOSED |
| DIG-G12 | **Rewrite soundness is a proof obligation (blocks W4).** "The tests pass" admits no rewrite. Each rule needs a declared observation class, a property-tested witness against the reference interpreter with on and off arms, and a seeded unsound mutant that the differential catches. Read by DIG-EG-A1..A3. | PROPOSED |

---

## 2. The CSP: definition of done as gate rows (hard requirement)

No trial verdict may remove this organ (DIG-G1). Full text of each condition: DIG §5.1.

| Id | Unit | Condition | Reader | GREEN | Today |
|---|---|---|---|---|---|
| DIG-CSP-A1 | LW5.COMP-3 | Solver complete upstream: nogoods in search, conflict-directed backjumping, Luby restarts, `bounded_sum`, cardinality and global-sum, soft constraints or a penalty encoding, LCV, lex-leader symmetry breaking, incremental re-solve, min-conflicts local search, Unsat explanations. The owner's Python suite passes unchanged | `cargo test -p csp-solver` at the release tag, one named test file per feature; `ls csp-solver/src/solver` | every module present; laws pass; semver-probed release | RED (csc411 `65804fa7`: `ac3, adjacency, gac, monotonic, optimize, propagate, search` only) |
| DIG-CSP-A2 | new: LW4.TOPO-PIN | Both consumers re-pinned to a release containing the CH-69 cure `58014efd` | `cargo tree -p csp-solver` in bbnf-lang and parse-that; upstream `gac_kernel_beats` and `killer` against the pin | both ≥ that release; tests pass | RED (0.4.0 at bbnf-lang `Cargo.toml:37` and parse-that `rust/egraph/Cargo.toml:18`) |
| DIG-CSP-A3 | LW5.COMP-1 | Solve ceiling; exceeding it fails the build | `BBNF_CSP_REPORT=1 cargo xtask regen --check` per grammar | ≤ 1 s per grammar; `budget_exceeded=false` on all 9+3 | RED (CSS L4 exhausted 1,000,000 nodes in a 52.53 s compile) |
| DIG-CSP-B4 | LW5.COMP-1 | Types, FIRST, FOLLOW, nullable, span, dispatch eligibility and call mode on one `propagate()` inside the spine | `cargo tree -i csp-solver` from the spine crate; `facts.json` differential against root | non-empty; 0 diffs over 9+3 | RED (the spine has no CSP) |
| DIG-CSP-B4b | LG-3 addition | LW3.COMP-7's FIRST/FOLLOW fixed point is recorded as provisional: LW5.COMP-1's row states that it replaces COMP-7's fixed point with the CSP's, so "FIRST in one place" does not lock the CSP out | text of the LW5 order | the sentence is present | RED (`O:116` forbids a second FIRST; `O:19` puts the CSP at LW5) |
| DIG-CSP-B5 | LW3.COMP-6 → LW5 | Lattice laws on all five domains and on each new one | law oracle at 10⁴ cases per domain, plus a mutant | 5/5 GREEN; mutant RED | RED (2/5) |
| DIG-CSP-B6 | new: LW5.COMP-1-PAT | One CSP pass for all structural pattern recognition (owner 04-07) | †census script over `crates/ir/src/passes/recognizers/` (37 files, 7,179 lines) | `unhomed=0`; one CSP instance per grammar carries every pattern variable | RED (`StructuralPattern` 0 hits) |
| DIG-CSP-B7 | new: LW5.COMP-4 | The type-system CSP: finite choice for overloads, layout, materialisation, recogniser eligibility, recovery and backend erasure; HM and bidirectional inference with the CSP as its substrate (owner 05-04: "the backbone") | †type-choice suite; PROVE-08 schemas re-derived | pass ×2 | RED (first-order only, `CC`) |
| DIG-CSP-C8 | LW5.COMP-1 | The plan decides dispatch shape, regex engine and tier, materialisation, call mode, payload layout, scan plan and backend shape; the emitter decides nothing | per-family removal mutant → pin test and `plan.json` / emit diff | each family flips | partial at root (`0cfc3663b`); RED on the spine |
| DIG-CSP-C9 | LW5.COMP-3 + LW5.COMP-1 | Per-component exact optimisation (the `csp_strategy` COP, named) plus the coupled sub-problem over resources measured to bind: hot-path instruction bytes (~20 KiB), shared table / Teddy / shufti slots, tape and payload slots, inline budget | witness-grammar test plus a bench cell | per-site minimum busts the budget; the solve is legal and has lower cycles per byte; with slack the plan equals the per-site optimum byte for byte | RED (`bounded_sum` on no pushed ref; DIG-CUST-1) |
| DIG-CSP-C10 | LW5.COMP-1 | No silent fallback (DIG-G5) | the grep; forced-exhaustion regen | 0 hits; exit ≠ 0 | RED |
| DIG-CSP-C11 | new: LW5.COMP-5 | Objectives read a measured cost table | coefficient-flip mutant → `plan.json` hash changes; the 648-run sweep re-run | changes; sweep non-null | RED (null sweep `05364225b`; hand weights) |
| DIG-CSP-D12 | new: LW5.COMP-1-BRIDGE | CSP↔e-graph bridge at `passes/csp_egraph_bridge.rs`, with a justification per accepted or denied rewrite | `ls`; justification count = accepted + denied | equal | RED (0 hits on three refs) |
| DIG-CSP-D13 | LW5.COMP-1 | The saturation scheduler uses real solver results, and its gain over the backoff scheduler is recorded | paired cells (nodes, iterations, wall) | recorded on a QUIET record | RED (never measured) |
| DIG-CSP-D14 | LW5.RX | Regex tier and prefilter selection on the same solver and cost substrate | `cargo tree -p parse-that-regex -i csp-solver`; tier plan per leaf | non-empty | RED (local table) |
| DIG-CSP-E15 | PREREG addendum, before any run | A discriminating trial: CSS L4, BBNF, Sheets and the three H-1 grammars; JSON as non-regression only. Four arms: solver; independent per-site minimum; the old priority cascade; per-component against per-component plus coupled | PREREG addendum hash; `admit.py`; `decide.py` | admitted; decided on a QUIET record | RED (JSON only; the "greedy" arm is the cascade) |
| DIG-CSP-E16 | new: LW5.COMP-6 | Tuning, validation and visibility (owner 04-12) | †tune xtask; `CostDecision` count = decision count; playground view | equal; sweep non-null | RED |

---

## 3. The e-graph and shape mining: definition of done as gate rows (hard requirement)

No trial verdict may remove these (DIG-G1). Full text: DIG §5.2 and §5.3.

| Id | Unit | Condition | Reader | GREEN | Today |
|---|---|---|---|---|---|
| DIG-EG-A1 | LW3.COMP-4c | Each rule declares its observation class and failure contract | the COMP-4c registry, read by the stage verifier | `missing=0` | RED (no `obs()` in code) |
| DIG-EG-A2 | LW5.COMP-1 | A property-tested witness per rule, on and off arms, with a seeded unsound mutant | one proptest per rule | all pass; mutant caught | RED (0 witnesses) |
| DIG-EG-A3 | LW4.RX-2 + new LW4.COMP-5 | Refutations first, as failing tests, then repair or removal: `WrapOfEpsilonScalar`; `FuseAltRegexBranches` (back only with the `L_cut` certificate; still default on leftmost-first leaves); `SupersetAbsorbAlt`; `UnionMergeAlt` (repetition and metacharacter halves); `CommonSuffixFactor` (`Alt(a c, ab c)` on "abc"); `RepeatOfSingle`; `ConcatLiterals`; the HIR repetition merge | born-RED tests | RED ×2 → GREEN ×2 | RED; three regex-tier tests `#[ignore]`; five targeted witnesses unwritten |
| DIG-EG-B6 | LW5.COMP-1 | Saturation outcome consumed; fail-closed to un-rewritten IR; numeric budgets (≤ 1,024 steps and ≤ 256 merges per rule, 64 iterations, 100,000 nodes, a wall-clock limit) | adversarial grammar per pool; `PassReport.stop_reason` | fails closed | RED (`let _report`; no time limit) |
| DIG-EG-B8 | LW4.COMP-2 | No surface without a consumer: `crates/ir/src/rewrites/` wired or deleted; the `Map{fn}` 1e6 cost hack removed | `git grep MAP_PRESERVE_BONUS`; caller census | 0; 0 orphans | RED |
| DIG-EG-C9 | LW5.COMP-1-OPTIR | The e-graph subsumes the destructive passes (owner 04-08: "These should SUBSUME the destructive passes"); saturation also runs in OptIR islands between Facts and Plan | destructive-pass list against a verdict file | `missing=0` | RED (normalizer runs first, no verdict) |
| DIG-EG-C10 | LW5.COMP-1 | The seven README §6 categories at their paths, creating classes with ≥ 2 real forms | `ls passes/egraph_rewrites/*.rs`; firing report | 7 present and applying, or a QUIET verdict each | RED (~2/7) |
| DIG-EG-C11 | LW5.COMP-5 | Cost fitted from hardware counters on real hit distributions | held-out grammar-family cell | no pick loses > 10 % | RED (hand weights; the only fit evidence is a uniform synthetic corpus) |
| DIG-EG-C13 | LW5.COMP-1 | Every default rule applies on ≥ 1 production grammar, and its form is reached on the parse hot path | `egraph_fire_probe` / spine `PassReport`; attribution | every rule > 0 | RED (1/14, `7620cac2a`) |
| DIG-EG-C14 | LW5.COMP-1 | Changes emitted code; deterministic; write-back keeps sharing; a `CostDecision` per decision | regen diff on against off; `regen --check` ×2 | ≥ 3 grammars differ; byte-identical ×2 | RED |
| DIG-EG-D15 | LW5.COMP-1 (parse-that-egraph) | One grammar e-node; rules declared as patterns (an e-matching language); explanations recorded; regex and grammar tiers mirror each other and share engine, cost and `L_cut` | `git grep -c 'enum GrammarENode\|struct DecisionNode'`; module-tree diff of the two tiers | 1; `unexplained=0` | RED (2; rules imperative, `rewrite.rs:28`) |
| DIG-EG-D17 | PREREG addendum | F-TRIAL-EG re-registered on the DIG-CSP-E15 grammar set; a third arm (normalizer-only against e-graph-subsumes) once C9 lands | as DIG-CSP-E15 | decided on a QUIET record | RED (JSON only; 13 of 14 rules never fire there) |
| DIG-EG-REC | any LW3 docs seat | The misleading records are corrected: the +8 % spot file credited to rule G3 (the emitter predicate did it), `docs/GESTALT.md:58`, the `egraph_semantic.rs` docstring | grep for an erratum line in each | 3/3 | RED |
| DIG-SM-1 | new: LW5.COMP-1-MINE | Shape mining is a stage on the spine (owner 04-12, 04-15, 05-04; README §6 pass 4). Every recognizer becomes a `ShapeMiner`, a constraint emitter (DIG-CSP-B6) or a widening e-graph rule | †census script | `unhomed=0` | RED (37 files; no miner stage) |
| DIG-SM-2 | same | The March techniques survive the move: delimiter-driven scanning, key/ident dispatch, quoted-string scanning, operator chains, separator lists | a pin test per technique on emitted code | all present | RED |
| DIG-SM-3 | same | Miner hints feed the shared cost model; the tape decision pools CSP, e-graph and miner facts | miner-removal mutant | pin test flips; `plan.json` changes | RED |
| DIG-SM-4 | same | No language name in any miner | generality grep, no exclusions | 0 | RED |

---

## 4. The tape union (unit with falsifiers)

Unit: **LW4.PERF-2** (admitted by ADD-2). Amendment 9's text reads "one tape … as a plane"; these rows restore what it left out (DIG §6 E, N-f).

| Id | Condition | Falsifier (RED today → GREEN) |
|---|---|---|
| DIG-T-14 | One tape: `TapeToken{kind, flags, start, end, payload, sibling_skip}`, a payload arena, a `ValueRef` cursor, `Document{tape, root, metadata}` | `git grep -c 'struct TapeRec\|structural_offsets'` = 0. RED: the root `TapeRec` and skinny's tape are two unrelated tapes |
| DIG-T-15 | Direct-to-struct values carry the tape node identity (`SinkOnly` excepted by Lock 1) | struct built directly ≡ struct projected from the tape on every P4 witness and under fuzz; mutant: an off-by-one node id is caught |
| DIG-T-16 | All five shapes (offset tape, event tape, sink-only, collapsed stage, eager tape) are members; the CSP chooses one per rule from grammar facts, e-graph extraction and miner hints; every extraction result declares `substrate_target` | removing a fact family flips a pin test; `git grep` for a shape-selecting directive = 0. RED: 2 of 5 shapes ever had a consumer |
| DIG-T-17 | Lazy numbers are a flavour; no sidecar, no column layout, no parallel offset vectors | the three-vector offsets are gone from the tree; a DIG-G3 paired cell shows no regression against AU |
| DIG-T-18 | LSP, path/select, visitor, diagnostics, playground and bench attribution key on the same node IDs (DAP per O-12); mutation only through the visitor | one witness per consumer resolves the same ID |
| DIG-T-19 | **The value API is built** (new unit LW4.COMP-6; today only the certification cell LW7.CON-1-REQ names P5, and a LOSS satisfies it): `as_<T>()`, `try_into()`, typed property, visitor; `path!` and `select!` over one path core; lazy and eager evaluation; for JSON every value direct-to-projection. `bbnf-path` and `bbnf-path-ts` are ported, not deleted by census default | P5 cell admitted with a generated builder; `cargo tree` shows the path crates on the plan. The tape is not the navigation substrate for `get` (measured 150–1,700× the stop-early cost): a `get` that walks the tape is refused |

---

## 5. The dav1d-derived assembly programme (unit with falsifiers)

Units: **LW4.PERF-1** and LW3.PERF-1a/b, plus the new rows named. The owner's words: "a perfected union between backend agnostic code, with an emitter that specializes in a hybrid of Rust and hand-rolled ASM, inspired by notions from the DAV1D project" (06-10; repeated 07-05, 07-10, 07-15); "esoteric and black-magic ASM" (11-05); "for both parse-that (regex engine, etc) and herein" (07-10). Record: live, executed hand assembly across the programme is zero.

| Id | Condition | Falsifier (RED today → GREEN) |
|---|---|---|
| DIG-K-B5 | One kernel table chosen at start-up; a scalar oracle per kernel; checkasm over random, corpus and adversarial inputs, lengths 0..256, page-boundary and alignment sweep, stack canary, guard pages; 10⁷ clean fuzz executions per target; Miri; nightly ASan; clobber tests on every `asm!` block; a status per aarch64 primitive with its `FEAT_*` gate | all clean, status TSV complete. Partial today (the harness exists in `parse-that-simd` and caught two real bugs) |
| DIG-K-B6 | **Liveness.** Every planned kernel is reached on corpus bytes in the generated parser, and the dispatch label names the variant that ran | †trace counter per kernel on the corpus: every planned kernel > 0; a seeded dead kernel turns the gate RED. RED today: the JSON path is scalar while a green harness certified the kernels |
| DIG-K-B7 | The kernel set: stage-1 classification with the `vshrn` movemask; whitespace skip; `vqtbl` byte classes; shufti/truffle; accel self-loops; keyword PHF; fused string scan; UTF-8 validation; escape mask and `\uXXXX`; Eisel–Lemire with a correctly rounded fallback; prefix-XOR string mask; stop-early `get`; for the regex engine the start-state prefilter, Teddy and the DFA inner loop. NEON first, wasm32-simd128, x86 per O-2 | each kernel beats its scalar oracle by the ruled threshold (O-11) and moves a JSON, CSS or regex cell on a QUIET record. A miss opens a redesign unit (DIG-G1). RED: CB-9's string/escape scan, keyword PHF and `vqtbl` are absent from `P:208` |
| DIG-K-B8 | The measured negatives stay negative without new evidence: NEON digit parse, multi-block pipelining, a separate index prepass with no amortisation, one-shot presize, the DTA table walker | LW3.PERF-1a's "own stage-1 indexer" opens with a committed cycle split that answers these by name |
| DIG-K-C9 | **The hybrid emitter.** Generated Rust calls kernels through one kernel ABI; an assembly emission path exists in one seam module and is exercised; each assembly kernel has a claim-grade record against its intrinsic twin with the algorithm held equal | `git grep -cF 'asm!('` in the seam module ≥ 1 if O-3 says so, 0 elsewhere; one record per block. RED (0 `asm!`; the x86 layer never ran) |
| DIG-K-C10 | **The charter.** Assembly is admitted where no intrinsic exists, where a disassembly diff shows ≥ 10 % left, or where state crosses a chunk boundary. Flagship: msac-style cross-chunk refill (new unit LW4.PERF-3) | a QUIET paired record for the refill kernel against its intrinsic twin, win or recorded negative. RED (never built). Note: PROTO-A2 answers one kernel only; with the algorithm held equal the intrinsic led hand assembly by ~1–8 % |
| DIG-K-C10a | **ISA census.** Every `hw.optional.arm.FEAT_*` that reads 1 (PMULL, DotProd, I8MM, CSSC, SHA3, …) has a named candidate and a verdict | census TSV against `sysctl hw.optional.arm`: `unverdicted=0`. RED |
| DIG-K-C12 | **The 05-13 question** ("Is our new path matierally different? For both skinny and greater?") answered by measurement: why this path will not repeat the automaton overhead LLVM could not fold | one committed experiment isolating recursive descent against an emitted table walk on the same grammar. RED (observed, never isolated) |
| DIG-K-D13 | **Kernel cost table** (kernel × microarchitecture → measured cycles per 64 bytes), filled by the harness, read by the CSP and the extractor (new unit LW5.COMP-5, shared with DIG-CSP-C11) | mutating one entry changes `plan.json`. RED |
| DIG-K-F | Build discipline: fat LTO, one codegen unit, forced inlining of hot rules, a ~20 KiB hot-function ceiling with a diagnostic, a panic-free hot path | symbol-size reader on the emitted object; a seeded oversize function raises the diagnostic. The LTO effect is to be measured in isolation (the record on file is confounded) |
| DIG-K-PROVE | **PROVE landings** (new unit LW4.PERF-4): each PROVE GO result (01 class runs, 03 fused string scan, 06 stop-early `get`, 07 direct-to-struct, 10 regex prefilter, 12 const-folded delimiter mask) names a landing unit | the HEAD re-measure on the same plane is ≥ the verified PROVE ratio, or a QUIET negative is recorded. RED: 03, 06, 10 and 12 have 0 hits in plan, order, amendments or rulings |

---

## 6. "Besting simdjson in full generality": the certification matrix

Units: LW4.CON-1/2, LW5.CON-1, LW7.CON-1-REQ. A claim is certified only when every required cell below is admitted and decided. Today: 0 certified cells, and simdjson has never been measured on a like-for-like plane.

**Rules for every cell (DIG-CERT-0).** Generated track only, never a hand prototype. Each plane against the opponent's own plane (a lazy plane never against a DOM). Owned against owned, borrowed against borrowed. Same flags, same run, fresh processes, ABBA order, ≥ 200 pairs, Holm within the goal's family, BCa intervals, a checksum walk inside the timed region, output validated structurally against a reference parse, allocator named, MB/s from `ns_per_byte`. QUIET window only (owner ruling O-7); a NOISY record decides nothing.

| Id | Plane | Our arm | Opponent arms (same plane) | Corpora | Bar | Status in sk-v27 |
|---|---|---|---|---|---|---|
| DIG-CERT-P1 | P1 check / validate | generated validator | simdjson `validate`-class API, sonic-rs, yyjson read-and-discard; each row admitted with a named API and a work-equivalence note | all eight registered JSON corpora, plus escape-heavy, unicode-heavy and number-heavy | > the fastest opponent on that corpus | G-J1; no comparator fixed |
| DIG-CERT-P3 | P3 tape | the tape union (DIG-T) | simdjson DOM tape, simd-json 0.19 `to_tape` (borrowed) | same | > fastest per corpus | G-J2; required by LW7.CON-1-REQ; prerequisite LW4.PERF-2 |
| DIG-CERT-P4 | P4 DOM / Value | eager value | simdjson DOM, sonic-rs Value, yyjson, simd-json DOM | same | > fastest per corpus. The archive's "≥ 5/8 corpora" is a narrowing the owner has not ruled (O-13) | G-J3 |
| DIG-CERT-P5 | P5 get / path (lazy) | stop-early `get` and `path!` | simdjson On-Demand, sonic-rs `get` / lazy value | shallow, deep and missing keys on each corpus | > fastest on all three depths. Known loss to cure: deep keys at 2.01× sonic's time (PROVE-06) | G-J4; required cell with no builder (DIG-T-19) |
| DIG-CERT-P6 | P6 typed (direct-to-struct) | generated direct-to-struct | sonic-rs typed, simd-json serde, simdjson On-Demand hand-typed, on the citm and twitter serde structs at pinned commits | citm, twitter, canada | > fastest. Baseline: 1.27–1.34× faster than sonic-rs typed (PROVE-07) | G-J5; required |
| DIG-CERT-P7 | serialization / transform | generated printer through pprint | sonic-rs, yyjson, simdjson minify | same | > fastest | G-J6 |
| DIG-CERT-NUM | numbers | Eisel–Lemire with a correctly rounded fallback | every opponent on canada and a number-heavy corpus | canada is the standing loss (+1.48 cycles per byte) | ≥ parity, exact rounding on the full test-vector set | LW4.PERF-1-U5, LW5.PERF-1 |
| DIG-CERT-ERR | error path | generated diagnostics | each plane's primary opponent | invalid-early, -mid and -late corpora | error byte offset equals the oracle on 100 % of cases; throughput ≥ 1.0× | G-E1 |
| DIG-CERT-CSS | CSS P2 events and P7 typed | generated CSS parser | cssparser (full-consumption event checksum); lightningcss typed tree at equal depth on the hashed coverage manifest | bootstrap, tailwind and the registered CSS set | > each, at equal work and equal depth ("the FLOOR"; "Full semantic parity with lightningcss") | G-C1, G-C2; no equal-depth cell has ever been run |
| DIG-CERT-GEN | generality across grammars | the same kernels driven by generated tables | the reference parser named by ruling H-1 for each | TOML 1.0.0 (toml-test), GraphQL October 2021 (graphql-js corpus), R7RS datum, plus BBNF and Sheets | each generated with no `@generated` verbatim blob; no grammar name in a generic crate (lint with no exclusion list); each kernel live on at least two grammars | H-1; amendment 7 |
| DIG-CERT-REG | regression floor | every plane above | our own AU table and verified PROVE ratios, same plane | AU corpora | no cell below its AU or PROVE baseline (DIG-G3) | new |

**Opponent facts the matrix must respect (measured, sidecar profiles, not gated).** No single opponent is fastest on this machine: yyjson leads on twitter (3,687 MiB/s against sonic-rs 2,782 and simdjson DOM 2,642 under load) and canada; simdjson DOM leads on citm (4,252), apache_builds, github_events and update_center. "Beats simdjson" therefore means beating the fastest of the four on each corpus.

---

## 7. Constellation checkpoints, each with its gate

| Id | Checkpoint (owner's requirement) | Gate | Today |
|---|---|---|---|
| DIG-C1 | Best simdjson in full generality | the matrix of §6, every required cell CERTIFIED | Unmet; 0 certified cells |
| DIG-C2 | Full CSP implementation | DIG-CSP-A1..E16 all GREEN; DIG-G1 | Unmet |
| DIG-C3 | Optimization passes: e-graph and shape mining | DIG-EG-*, DIG-SM-* all GREEN; DIG-G1 | Unmet |
| DIG-C4 | Backends for TS and Rust over the IR and a generalized IR | both emitters read all six LIR fields from `plan.json` and refuse a plan missing one (LW4.COMP-4); TS output byte-equal to Rust on the conformance corpus; the TS backend receives the optimised plan | In part (TS 0.3.0; parity GREEN ×2, 2 divergences carried) |
| DIG-C5 | Tape union facility | DIG-T-14..19 | Unmet |
| DIG-C6 | The new IR as the build | `cargo tree -i bbnf-ir` empty; 9+3 runtimes regenerated byte-deterministically; parity ≥ root on 9/9 with the carry list pinned (DIG-H1) | Unmet |
| DIG-C7 | Regex for parse-that and bbnf | leftmost-first agreement with V8 and JSC on the full leaf set; hand parser deleted (LW3.RX-5); lookaround, `/iu` and backreferences GREEN; determinizer per O-4 | In part |
| DIG-C8 | pprint and formatting | LW3.PP-1a/1b/3a GREEN; TS renderer byte-equal on STREAM-v1; `fmt∘fmt = fmt` on corpus plus 10⁵ fuzz | In part |
| DIG-C9 | Type inference at the state of the art, CSP-backed | DIG-CSP-B7; scope per O-5 | Unmet; no unit |
| DIG-C10 | Everything grammar-derived; no custom grammar unless BBNF | generality lint with no exclusion list = 0; regex front end generated; DIG-CERT-GEN | In part |
| DIG-C11 | dav1d-style Rust + ASM union; x86 per his words | DIG-K-B5..D13; O-2, O-3 | Unmet |
| DIG-C12 | The TS emitter benefits value.js and keyframes.js | USE-1..4 GREEN at each wave; LG-d timings read | In part |
| DIG-C13 | Release cascade bbnf-lang → parse-that → value.js → keyframes.js | LW6 gate; the early publish of 14 crates ruled (O-8) | Not started |
| DIG-C14 | value.js release condition (six conjuncts, size ceilings, mirror defects 0) | value.js `RELEASE-CONDITION.md` | Open |
| DIG-C15 | Parser speed floor for value.js | O-9 (≥ 10×, or 3× / 2×) | Narrowed, unratified |
| DIG-C16 | WASM union between BBNF and parse-that | LW4.COMP-3 is a trial that may switch a VM off; it may not delete the wasm face without an owner ruling | Open |
| DIG-C17 | Lockstep gate at each wave | LG-n all rows GREEN ×2, with DIG-G1, G2, G3, G7 added to LG-g's reading list | LG-2 taken at its eighth reading |
| DIG-C18 | Organ trials | DIG-CSP-E15, DIG-EG-D17 | Registered, never run; cannot discriminate |
| DIG-C19 | Certification seam | a QUIET window (O-7); more than one frozen floor | One floor frozen |
| DIG-C20 | Model-tier receipts | DIG-G9 | Standing law |
| DIG-C21 | TS ↔ Rust isomorphism; regex and low-level single-homed in parse-that | divergence table = 0 or each row ruled | 2 carried |
| DIG-C22 | No V2, no deferral, no stubs, no dual paths | DIG-G11; `#[ignore]` census trends to 0 with each row homed | Standing law |
| DIG-C23 | Nothing proven is lost | DIG-H1..H3 | RED |

---

## 8. Carry rows

| Id | Row | Status |
|---|---|---|
| DIG-H1 | **Carry list with pin tests** (owner 04-17: "do not lose any optimization or implementation in this restructuring"). `DIG-CARRY.tsv`† lists, with a pin test on emitted code for each: sibling skip; the grammar-emitted visitor path (`c1e86ab32`); delimiter-driven flat scanning; regex-as-fused-scanner recognition; key/ident dispatch; IIFE elimination; the lazy-offset tape; the 23 recognizer modules; Lock 15; `soft.rs` and `cardinality.rs` at `d2b32f6e3`; the `token` constraint family at `d2e169794`. Falsifier: `crates/ir` and skinny are not deleted (LW7) while any row lacks a GREEN pin test on the spine. | PROPOSED |
| DIG-H2 | **The post-cut design crosswalk** promised at `L:171` is written: programme IF-2, W4, W8, W9 and `competition.md:173` each map to a unit or an owner ruling. Falsifier: `unmapped=0`. | PROPOSED |
| DIG-H3 | **DAP and source maps** are named: kept on the spine with a witness that DAP steps a generated parser, or let go by owner ruling O-12 before LW4.COMP-2 cuts the LSP off the root VM. | PROPOSED; owner ruling O-12 |

---

## 9. Rulings owed by the owner

Each is a place where an agent or the delegate chose for him. The rows above stay PROPOSED on these points until he rules.

| Id | Question |
|---|---|
| O-1 | Confirm that no trial verdict deletes the CSP, the e-graph, shape mining or a kernel slot, at any wave (DIG-G1). |
| O-2 | x86: his words are "no x86" (05-31) and "Full X86 abrogation" (07-10); delegate ruling X-4 keeps the AVX2 and AVX-512 tiers. |
| O-3 | What the ASM "union" obliges: an emitter able to emit assembly under the measured charter, or at least one live assembly kernel as a condition of done. |
| O-4 | Regex determinizer: bespoke (his words of 04-01, 04-02), or `regex-automata` (Route A, provisional). |
| O-5 | Type system scope: full HM with CSP-backed choice, or first-order. |
| O-6 | Amendment 9: make the fourteen units law, and put their ids in PLAN.md or widen Z-32 (DIG-G2). |
| O-7 | A quiet certification window. Nothing certifies under load 10–40. |
| O-8 | The early publish: 14 crates went to crates.io at LW2 against `P:93`. |
| O-9 | The value.js parser floor: 3× / 2×, or restore ≥ 10×. |
| O-10 | Whether any generated-size ceiling binds the solve, given "a larger binary is acceptable for now" (04-08). |
| O-11 | The kernel speed threshold: ≥ 3× the scalar oracle (G-K1) or ≥ 2.0× (CB-9). |
| O-12 | DAP and source maps: kept, or let go with the root VM. |
| O-13 | "Every path >SOTA" (05-21), or "≥ 5/8 corpora". |
