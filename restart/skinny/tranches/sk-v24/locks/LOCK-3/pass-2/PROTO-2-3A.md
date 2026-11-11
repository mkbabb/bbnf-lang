# PROTO-2 (3A) — LOCK-3 pass-2 · PROTO BRIEF 2: the per-edge `differential(edge, corpus)` refinement oracle

**Family:** 3A — CONTRACT-TOWER · the *refinement oracle* organ (the one thing distinguishing 3A
from a tautological `verify()`; FAMILY-SPECS §3A open question (i), disposition "sharpest UNRUN
uncertainty in the lock").
**Probe:** t2 (family 3A), pass-2 spine. Answers whether the runner **localizes an injected
single-arm fault to its edge — never merely "a failure."**

**BASE-PIN.** HEAD measured on **`ce51c36fcbdf5108eb028005586672d9a6d2ca1b`** — the pin itself.
Worktree HEAD was stale at `d2b32f6e3` (an ancestor of the pin); fast-forwarded cleanly with
`git merge --ff-only ce51c36fc`; `git merge-base --is-ancestor ce51c36fc HEAD` ⇒ **OK, descends
(equals).** **No wrong-artifact-risk.** arm64; no x86. parse-that untouched (this is a bbnf-lang IR
probe; READ-ONLY rule not engaged). loadavg 4.6–4.8 (1-min) throughout; 18 cores; machine shared
with sibling agents. Isolated `CARGO_TARGET_DIR` (scratchpad) — no shared-target contention.

**Filename note.** Delivered as `PROTO-2-3A.md` to avoid clobbering the already-committed
`PROTO-2.md` (family **3B**, the fold-precondition trace). Two different probes both carry index 2
in different family tracks; this one is 3A. The 3B `PROTO-2.md` was **not** touched.

---

## QUESTION

3A's distinctive organ is a per-edge **refinement oracle** `differential(edge, corpus)`,
`edge ∈ {E1 infer, E2 materialize, E3 sink}` — translation-validation at edge granularity, the
generalization of the two specialized instances already shipping (`bbnf-bench/benches/json_parity.rs`
track digests; `runtime/tests/css_scan_differential.rs` scalar oracle). The killer uncertainty
(FAMILY-SPECS §3A (i)):

> Does the runner **localize an injected E2/E3 fault to the injected edge (never merely "a
> failure")?**

with the sub-claim that **E3 is a *free* oracle** — `lower_expr` (`codegen/src/lower/sink_only.rs`)
is a 1:1 arm map `BackendExpr ↦ SinkOnlyExpr`, so `correspond(x, lower(x))` needs **no reference
implementation**. Two things must both hold for the organ to earn 3A: (a) the oracle is GREEN on the
shipping map over a full-arm witness census (G-3A-b: a zero-witness arm reports `unverified`, never
`sound`); (b) an injected single-arm mis-lowering is **named to its arm/edge**, and an *upstream*
(E2) fault trips a *different* edge oracle while E3 stays GREEN.

---

## METHOD (what ran)

A running `differential`-runner probe in `crates/codegen/tests/differential_edge_oracle_probe.rs`
(4 tests), driving the **real** shipping E3 map via a thin new edge entry point
`codegen::lower_expr_probe` (the sink map callable on one node, no `LowerCtx`):

1. **Witness census (G-3A-b).** A synthetic corpus with **one witness per BackendExpr arm**;
   asserted to cover all **13** arms. No arm is zero-witness by construction — the census is
   `sound`-eligible for this corpus, never `unverified`.
2. **E3 correspondence oracle** — `e3_correspond(&BackendExpr, &SinkOnlyExpr, path)`, reference-free
   arm-by-arm translation validation. It returns *localized* mismatches (each names the tree `path`
   + arm), and correctly **does not demand the two fields the map erases** (`SpanMark.kind`,
   `TapeEmit.kind` — the documented lossy arms), so the oracle is honest about the map's actual
   contract.
3. **GREEN floor.** Run the oracle over `lower_expr_probe` for every census witness → must be empty
   (the shipping map corresponds today).
4. **Fault matrix.** A verbatim copy of the arm map with a **single injected arm-fault** (5 faults:
   discriminant flip, same-discriminant payload corruption, arity change, payload-string corruption,
   child-order corruption). For each: run the oracle over every witness, record which witnesses
   flag, assert (i) the faulted arm's own witness flags, (ii) **every** flagged witness genuinely
   contains that arm (no false localization), (iii) the coarse terminal digest also breaks.
5. **E2-vs-E3 separation.** Corrupt the *input* IR (empty `DirectBuild.shape` — a materialize/E2
   fault). Show the E2 structural-contract oracle `O2` flags it (localized), while the shipping E3
   map faithfully lowers the corrupt input so the E3 correspondence oracle stays **GREEN** — the
   fault is named at E2, not misattributed to E3.
6. **Live grounding.** Drive the real pipeline `grammar::parse_grammar(json) → passes::compile →
   BackendIr`; run the E3 oracle over every live rule; record E1 (`node_types`) and E2 (`backend_ir`)
   reference digests.

`cargo test -p codegen --profile ax-iter --test differential_edge_oracle_probe -- --nocapture`,
single invocation, isolated target dir.

---

## WHAT RAN — THE NUMBERS (measured)

**4 tests, 4 passed, 0 failed, 0 warnings.** `EXIT_CODE=0`.

### E3 oracle GREEN on the shipping map (census + live)

| corpus | arms/rules | E3 correspondence oracle |
|---|---|---|
| synthetic all-arm census | **13 / 13 arms witnessed** | **GREEN** (shipping map corresponds on every arm) |
| **live `json` `BackendIr`** | **15 rules, 100 backend nodes** | **GREEN** (every live rule corresponds) |

Live reference digests (invariant to any pure-E3 fault, since computed upstream):
`E1_digest = fb8040054db8cf4e` (`node_types`), `E2_digest = 53f80c57f44b665c` (`backend_ir`).
This validates the "free oracle" claim on live IR: the sink map **is** a faithful 1:1 arm
translation today, checkable with no reference implementation.

### Localization matrix — injected single-arm E3 fault (measured, `LOCALIZE` lines)

| injected fault (arm) | fault class | E3 oracle flagged | false positives | terminal digest |
|---|---|---|---|---|
| `ByteLiteral → Return` | discriminant flip | **5** = `{Seq, Alt, RepeatLoop, OptionalBranch, ByteLiteral}` — exactly the ByteLiteral-bearing witnesses | **0** | broke (`fafd…→84b2…`) |
| `RegexProgram` span `String→Number` | **same-discriminant payload** corruption | **1** = `{RegexProgram}` | **0** | broke (`fafd…→192f…`) |
| `Alt` drop last branch | arity change | **1** = `{Alt}` | 0 | broke |
| `ValueProject` target rename | payload-string corruption | **1** = `{ValueProject}` | **0** | broke (`fafd…→20c4…`) |
| `Seq` reverse children | child-order corruption | **1** = `{Seq}` | **0** | broke (`fafd…→e2b7…`) |

Every fault is **named to its arm** (path text contains the arm), flags **only** witnesses that
truly contain it (verified by an independent structural `contains_backend_arm`), and the terminal
digest independently breaks. The `RegexProgram` row is the load-bearing one: a **same-discriminant**
payload corruption that a shape/tag-only check would miss — the correspondence oracle catches it,
proving the organ is more than a discriminant walk.

### E2-vs-E3 edge separation (the "never merely a failure" result)

| edge oracle | on E2 input fault (empty `DirectBuild.shape`) |
|---|---|
| **O2** (materialize structural contract) | **RED, localized** — `"corrupt/DirectBuild: empty shape"` |
| **E3** (`e3_correspond` over shipping map) | **GREEN** — the map faithfully lowered the corrupt input |
| terminal digest | changed (detection, no localization) |

The E3 oracle validates the **map**, not the value; so an upstream fault leaves it GREEN and is
localized to **E2**. A single terminal bool would have said only "output differs." This is the
concrete discriminator that answers open question (i): **the runner names the edge.**

---

## VERDICT LINE

**RAN_MEASURED — the 3A refinement oracle RUNS and LOCALIZES.** `differential(edge, corpus)` was
executed (4/4 tests green, `EXIT_CODE=0`, HEAD = pin `ce51c36f`, loadavg ~4.7): the E3
correspondence oracle is a **reference-free** translation validation that is GREEN on the shipping
map over a **13/13-arm** census **and** over the **live `json` BackendIr (15 rules / 100 nodes)**;
an injected single-arm E3 fault is **named to its arm with zero false-positive witnesses** across a
5-fault matrix (including a same-discriminant payload corruption a tag-only check would miss), the
terminal digest breaking independently; and an **E2 input fault trips the E2 oracle while E3 stays
GREEN**, so the runner **localizes to the injected edge, never merely reports "a failure"**
(FAMILY-SPECS §3A open question (i) = **YES**). This earns 3A's distinctive organ beyond a
tautological `verify()`.

**Scope-honest limits.** (1) E1/E2 here are grounded as *reference digests* + one E2
structural-contract oracle `O2`; a full E1(infer)/E2(materialize) **injected-fault** matrix with
their own per-edge oracles (mutating `infer`/`materialize` and showing E3 GREEN under each) is the
natural next widening — the E3 free-oracle and the E2↔E3 separation are proven, the E1 arm is only
digest-grounded. (2) The oracle's honesty about the two **lossy arms** (`SpanMark.kind`,
`TapeEmit.kind` erased by the map) means those two fields are outside E3's contract — a fault that
*only* flips an erased `kind` is invisible to E3 and must be caught at E2 (where the kind is still
live); this is correct edge-attribution, not a gap, but it is a named property of the map. (3) The
census is synthetic-per-arm (guarantees `sound`-eligibility); the live `json` corpus does **not**
witness every arm (e.g. `ValueProject` is Sheets-only), so on live grammars the per-arm verdict is
`sound` where witnessed, `unverified` elsewhere — exactly the Y.13 discipline G-3A-b prescribes.

---

## PATCH / DIFF

`PROTO-2-3A.patch` (git diff on pin `ce51c36f`). **Production footprint: +11 lines**, zero generated
files, zero behavioural change to emit:
- `codegen/src/lower/sink_only.rs` (+8): `pub fn lower_expr_probe(&BackendExpr) -> SinkOnlyExpr` —
  the E3 edge entry point (calls the private shipping `lower_expr` with a throwaway facts sink; this
  is the callable "edge" the per-edge oracle needs).
- `codegen/src/lib.rs` (+3): `pub use lower::sink_only::{lower_expr_probe, DirectShape,
  SinkOnlyExpr, SinkOnlySpanKind};` (widen the E3 surface for the differential runner).
- `codegen/tests/differential_edge_oracle_probe.rs` (+607, new): the four-test runner.

**Next falsifiable step (pass-3):** promote this runner to a shipping `differential(edge, corpus)`
CI contract and add the E1/E2 injected-fault arms (mutate `infer`/`materialize`; assert E3 stays
GREEN under each, E1/E2 localize) — every byte-identity gate in the lock is then an instance of one
edge oracle, which is 3A's stated deliverable.
