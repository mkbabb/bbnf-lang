# AY-II-AUDIT-A — Plan Coherence, Pre-close Pause

Date: 2026-04-21. Agent: Audit Agent A (AY-II pre-close triumvirate).
Scope: READ-ONLY audit of AY-II plan coherence against landed W0 at
HEAD `b5bbda6c`. Worktree:
`/Users/mkbabb/Programming/bbnf-wt-ay-ii-audit-a`. Not a
re-iteration of AY-I-era AUDIT-A at
`audit/AUDIT-A-plan-coherence.md` (that is the INPUT to this pass).

## 1. Scope + methodology

Cross-refs: `AY-II.md`, `waves/W{0,1,2,3,4,5}.md`, `PROGRESS.md`, W0
commit ledger (`a13840a0`, `b2ac3cf5`, `61d0338c`, `487b17b7`,
`4f42f6bb`, `2ddb8c33`, `f2e458ec`, `2b24b0a4`, `1f97a8cc`,
`db979564`, `58271da1`, `f8ac2cd7`, `c9142405`, `f372e7ef`), source
under `crates/{tape,core,ir}/src`. `target/expand/ay-*.rs` timestamps
02:00–03:37 predate compose commits (11:36–12:20) — stale; reported
as a gate-evidence gap. Method: grep + source read + commit diffs.
No benches, no regen, no code edits.

## 2. AY-II thesis → landed substrate

| # | Invariant | Verdict | Evidence |
|---|---|---|---|
| 1 | `Parsed::to_value()` sans reparse | PASS | `runtime/parsed.rs:348-353` body is `R::project_value_output(&self.value_builder_output, self.input)`; 0 grep hits for `parse_with_visitor`/`<.*Parser>::parse`. |
| 2 | `push_compound` absent from TapeBuilder API + emitter `quote!` | FAIL | `crates/tape/src/builder.rs:247` still exposes `pub fn push_compound`; `tape/src/visitor.rs:365,389,515` still calls it. Emitter `quote!` blocks clean. |
| 3 | `note_push` + `SIB_SKIP_STAMPED_BIT` retired | PASS | 0 grep hits across `crates/tape/src`. |
| 4 | `Columns::rollback_to` sole retry primitive | PARTIAL | Present, but retry-IIFE sites invoke no `value_builder.rollback_to` counterpart — atomic-both-substrates semantics is single-substrate. |
| 5 | Fused pipeline real: parse builds tape + value in one walk | **FAIL** | `emitter/grammar.rs:1107-1112` threads only `&mut builder` into `#dispatcher`. 25 shape-fn signatures take `builder: &mut TapeBuilder` only; zero take `value_builder`. `grammar.rs:1154` calls `value_builder.finish(root_off.0)` on an empty `ValueBuilder`. |
| 6 | `navigate_tape` absent from `runtime/path.rs` | PASS | 0 grep hits; retired by `4f42f6bb`. |
| 7 | Projection totality 1:1:1 per grammar | PARTIAL | `projection_totality.rs` authored (`58271da1`); not run against composed substrate. `STRUCTURAL_SCAN_POLICY` emitted `#[allow(dead_code)]` at `generated.rs:12381` — zero consumers; same-wave-consumer invariant (§7) violated. |
| 8 | Zero grammar-name dispatch | PASS | 0 dispatch-predicate hits for `JsonParser|CssL4Parser|BbnfParser|GoogleSheetsParser` across `backend/runtime/tape/ir`; `__named_type_shim` absent from `generated.rs`. |
| 9 | Typed CSS parity vs lightningcss | DEFERRED | W2 scope; W0.d emission landed. |
| 10 | `make ay-bench-close` clean on full 5-bench matrix | **FAIL** | No `docs/benchmarks/post-AY-II-W0-bench.txt`; W0 hard-gate (`W0.md`§Hard gate 3) unrun. |
| 11 | Competitor benches | DEFERRED | W1 scope. |
| 12 | Workspace green every commit + regen cycle-1≡cycle-2 | **FAIL** | PROGRESS.md §Pause §1–§2: workspace check incomplete; double-regen undemonstrated. `f372e7ef` hand-patches `generated.rs` (explicit SPEC §clean-regen violation). |
| 13 | No recorded misses; cannot-close → relinquish | IN-FORCE | Pre-close pause IS the relinquish; audit dispatched. |

Key PARTIAL/FAIL notes:

- **§5 load-bearing.** `ValueBuilder` is substrate without write-side
  consumer. Emitter allocates it, passes it nowhere, calls
  `.finish(root_off.0)` on an empty arena. Read-side
  (`value_materialize.rs:270-286`) emits a `project_value_*` that
  **panics** on `output.is_empty()`. Any `Parsed::to_value()` call
  on a parsed input hits the panic path. PROGRESS.md §3 confirms.
- **§2 mild.** `TapeBuilder::push_compound` is retained as visitor-
  lane dependency. Invariant text reads as full API removal; the
  landed carve-out is implicit.
- **§7 + W0.e is the recorded-misses reprise.**
  `STRUCTURAL_SCAN_POLICY` emits `#[allow(dead_code)]` —
  consumerless same-wave substrate is exactly the pattern AY-I-era
  AUDIT-A §5 named as the AV anti-pattern; AY-II was authored to
  prevent it.

## 3. W0 sub-phase ledger + hard-gate status

| Phase | Commits | Landed | Closed | Deferred |
|---|---|---|---|---|
| W0.a | `a13840a0`, `b2ac3cf5`, `f8ac2cd7` (fix) | `open_stack`/`note_push`/`SIB_SKIP_STAMPED_BIT` retired; `rollback_to`, `begin_compound`, `end_compound`, `end_compound_post_order` added | nm absence + compile clean | spot bench vs AY-I baseline — no artefact |
| W0.b | `2ddb8c33`, `f2e458ec`, `2b24b0a4`, `1f97a8cc`, `c9142405` (fix) | tape-method migration; retry-IIFE sites use `rollback_to` on tape only | `cargo expand` zero `push_compound` in emitter (implicit) | **fused-pipeline lockstep not threaded** (§Mechanism 2 not landed) |
| W0.c | `4f42f6bb` | `ValueBuilder<R>`, `ValueCheckpoint`, `ValueBuilderOutput`, `Parsed::new_fused`, `project_value_*` emission, parse-count test instr | `navigate_tape` retired; parse-count test authored | **fused write side** — read side panics on empty write side; hand-patched stub in generated.rs |
| W0.d | `db979564`, `58271da1` | `__named_type_shim_*` retired; admission → struct + marker + materializer; wire-contract test | test authored | tests not run against composed substrate |
| W0.e | `61d0338c`, `487b17b7` | cursor `object_key_seek`/`bounded_lookahead`/`scan_structural_bounded`; per-grammar `STRUCTURAL_SCAN_POLICY` const | emission present | **no consumer** — `#[allow(dead_code)]` at emission site |
| compose | `f372e7ef` | hand-patched `BbnfBootstrap::project_value_output = unreachable!()` | lib compiles | SPEC §clean-regen violation acknowledged |

## 4. W1–W5 validity against W0 actual

Every wave spec was authored before W0 landed (PROGRESS.md §Scaffold
landing). Each carries "W0 lands fused pipeline" as precondition.

| Wave | Assumption | Reality | Verdict |
|---|---|---|---|
| W1 | `bbnf_value_*` rides fused lane to ≤1.15× sonic; `W1.d` parse-count test asserts exactly-one parse | `to_value()` panics on empty write side; lane unreachable | **INVALID** — cannot open |
| W2 | W0.d projection totality + grammar-derived typed CSS projection from fused slab (`W2.b`§3-4) | materializers read `view.cursor().tape().payload_bytes(rec, …)` (e.g. `generated.rs:25686-25693`), NOT ValueBuilderOutput — the "project-from-fused-slab" thesis didn't land | **PARTIAL INVALID** — grammar-annotation layer works; fused-slab path absent |
| W3 | Sheets fused-slab projection + SheetsGValue consumer via fused pipeline | same failure mode as W1 | **INVALID** |
| W4 | Bootstrap double-regen; typed BBNF projections via fused slab | `f372e7ef` hand-patch on master; cycle-2 unverified; BbnfBootstrap::parse threads tape only | **INVALID** |
| W5 | Compose-only aggregate | predicates all invalid | **INVALID as planned** |

## 5. Plan-declared parallelism vs actual dispatch

| File | Declared | Actual touches | Overlap |
|---|---|---|---|
| `shapes/value_materialize.rs` | W0.c + W0.d disjoint regions | `4f42f6bb` + `db979564` | Clean — W0.c head region, W0.d totality region |
| `shapes/dispatcher.rs` | W0.b + W0.e disjoint | `487b17b7` only | W0.b's §Mechanism-4 audit was zero-diff or absorbed by W0.e |
| `tape/src/cursor.rs` | W0.a + W0.e disjoint | `61d0338c` (W0.e) | W0.a §Mechanism-7 Cursor signatures landed via W0.e alone; cross-owner sub-gate attribution wrong |
| `generated.rs` | orchestrator regen at close | `f372e7ef` hand-patch; never regen'd | Declared SPEC violation |
| `emitter/grammar.rs` | W0.b | `1f97a8cc` + `c9142405` + `f372e7ef` | Held |

Dispatch timing: 4 commits at 04:29–04:52 UTC, W0.c at 11:36 UTC
(~7h later). Stated "5 parallel" was "4 + 1 later" in practice.
Orchestrator compose mass: 3 / 13 commits = 23%. Boundaries held;
decomposition axis (shape-emission region) was a proxy for the real
seams (IR contract × shape kind × consumer surface) — composition
clean through agent convergence on the IR contract, not disjoint-
bound discipline. Same lazy-axis pattern AY-I-era AUDIT-A §4 named.

## 6. Recorded-misses discipline check

PROGRESS.md §Pre-close pause enumerates five outstanding items.
Absorb-vs-new-letter split per SPEC §Scope-reveal:

| Item | Absorb at W1 open? | If no, why |
|---|---|---|
| §1 Hand-patched `generated.rs` | YES | orchestrator regen at W1 open |
| §2 Idempotency unverified | YES | double-regen at W1 open |
| §3 **Fused pipeline write side** (value_builder not threaded; 25 shape-fn signatures need `&mut ValueBuilder<R>` parameter; lockstep compound/leaf/retry calls; that's emitter surgery across the whole shape family) | **NO** | scope equals W0.b's tape-method migration — a full wave |
| §4 `STRUCTURAL_SCAN_POLICY` consumerless | NO | invariant §7 rejects substrate-without-consumer at close; retroactive consumer-wire OR substrate retirement — either requires a wave |
| §5 tests unrun | YES | test suite at W1 open |

Three of five are orchestrator close-ceremony items pre-empted by
the pause. Two (§3, §4) are substrate+consumer gaps needing a wave
each. **Risk of recorded-misses resume**: if W1 opens with §3
deferred to "W1 absorbs," W1's scope (JSON peer parity + bench)
becomes parity + fused-pipeline substrate — the exact compression
that broke AY-I.W5–W6.

## 7. Verdict — coherence gaps + root causes

1. **Fused pipeline write side absent.** Parallel decomposition put
   `ValueBuilder` type (W0.c) and shape-body emission (W0.b) on
   separate agents without a shared threading contract; each landed
   its half; compose closed the types, not the call graph.
2. **`generated.rs` hand-patch on master.** Orchestrator-owned close
   regen pre-empted by pause; compose bridge never replaced.
3. **Structural-scan policy consumerless.** W0.e shipped substrate
   half; consumer declared in `W0.md:303-305` but not a separate
   commit — same-wave-consumer invariant violated.
4. **Dispatcher disjoint-region declaration unhonored.** W0.b
   declared `shapes/dispatcher.rs` as modify-scope; only W0.e
   touched it. Either zero-diff or silently absorbed — evidence gap.
5. **Cursor primitive contract asymmetry.** W0.a declared cursor
   signatures; W0.e shipped them. Sub-gate attribution wrong —
   W0.a's §7 closed via W0.e's commit, not W0.a's.
6. **Hard gates 3 (bench) + 5 (tests) unverified.** No
   `post-AY-II-W0-bench.txt`; test suite unrun post-compose.
7. **`TapeBuilder::push_compound` public API retained.** Invariant
   §2 reads as removal; landed state keeps it for visitor-lane.
   Wording or substrate needs alignment.
8. **W1–W5 spec assumptions invalid.** Waves authored as scaffold
   atop W0 intent; W0 landed half; every downstream gate reads
   against a precondition that doesn't hold.

## 8. Forward-path contributions

Structural decisions the orchestrator must make. Each with evidence
needed.

1. **Close W0 as-landed (→ AY-II-FINAL on a partial thesis + AY-III opens on fused pipeline write side) OR complete in-tranche (→ W0.f + W0.g sub-phases before W1)?**
   Evidence: cost estimate for §3 write-side landing across 25 shape-fn signatures + lockstep compound/leaf/retry calls. The architectural thesis hasn't changed — per SPEC §Multi-pass rule, "thesis stays → split" — so in-thesis AY-II resumption with W0.f + W0.g is the clean choice; but W0.f is a full wave by file-bound count.

2. **`STRUCTURAL_SCAN_POLICY`: wire consumer in W0.g OR retire entirely (revert `487b17b7`)?**
   Evidence: samply attribution on JSON twitter + CSS tailwind + Sheets + BBNF showing where scan-policy admission would materially accelerate. If samply shows no hot path, retirement is cleaner than wiring — policy's only surviving job is invariant §7 satisfaction.

3. **W1–W5 wave specs: rewrite-in-place OR re-author against landed W0?**
   Evidence: precondition line per wave needs update from "after W0 lands fused pipeline" to "after W0 lands tape substrate". §Scope bullets + §Hard-gate clauses are all fused-pipeline-present assumed; inline edits would scatter. **Recommendation: re-author**, because the wave specs' assumption foundation moved; clean re-authorship concentrates the resequence.

4. **Hand-patch in `generated.rs` (`f372e7ef`): revert before any further work OR carry through next regen?**
   Evidence: can emitter emit the stub from a real source edit? If so, inline the stub in the emitter, regen, hand-patch evaporates. Any scenario where `f372e7ef` persists past next regen is SPEC violation in force.

5. **`TapeBuilder::push_compound` public API: retire (visitor lane rebases onto `begin_compound`) OR keep as `#[doc(hidden)]` + invariant §2 carve-out?**
   Evidence: visitor lane's forward path. W1.md:67-69 treats it as opt-in bench-only; BA scope or retire-with-AY-II. Invariant §2 text tightens either way.

6. **Scope-reveal beyond audit-A scope (relinquishing to sibling audits):** `materialize_projection_*` fns read from `view.cursor().tape().payload_bytes(rec, ...)` (e.g. `generated.rs:25686-25693`) — the TAPE, not `ValueBuilderOutput`. AY-II.md §1 + §5 read as materializers projecting from the fused-value substrate. Landed materializers project from the tape. This is a second, distinct fused-pipeline gap beyond §3: even if `ValueBuilder` were populated at parse time, materializers would not consume it — they consume tape payload bytes. W0.c / W0.d seam neither agent owned; worth explicit attention in forward-path synthesis.

7. **Dispatch audit model.** Whether AY-II-AUDIT-{B,C,D} should re-scope vs carry AY-I-era briefs — orchestrator decision; flagged here, out of scope for this audit to decide.
