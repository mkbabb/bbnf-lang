# AY-III — Progress Log

Dated execution log for tranche AY-III (pass III of AY; see
`../AY-I/FINAL.md` and `../AY-II-I/AY-II-I.md` for the
predecessor passes, and `../AY-II-I/audit/AY-II-AUDIT-{A,B,C,D}-*.md`
for the triumvirate that informs this pass on the post-B5
substrate).

- `Status`: planned (pre-W0 dispatch)
- `Current wave`: tranche open
- `Next wave`: W0 (JSON closure on the post-B5 substrate)

---

## 2026-04-27 — AY-III opens against post-B5 / post-B6 substrate

AY-III opens against master HEAD `9c0bc392` (post-B6.W2
status-stamp close). Pass II (AY-II-I) closed its W0' ceremony
inside B4.W1 and authored five wave specs (W1-W5) against the
welded `FusedBuilder` substrate; the predecessor B-series
restored the substrate to one type, named correctly.

### Pre-tranche audit synthesis — AY-II-I → AY-III

The AY-II-I audit triumvirate findings carry into AY-III with
the following disposition:

**Phase 1 findings** (AY-II-I W0 landed state):

- AUDIT-A §5 FAIL — `ValueBuilder` allocated but never threaded
  through shape emitters. **Closed at B4.W1.** The unified
  atomic-rollback path lands the contract; the welded
  fused-builder type dissolves at B5.W1 into `Tape<R>` over
  `Columns`. AY-III does not redo this work.
- AUDIT-A §7 PARTIAL — `STRUCTURAL_SCAN_POLICY` consumerless.
  **Inherited.** AY-III.W0.c either wires the policy at emit
  time per AUDIT-C §3.2, OR retires the surface entirely. The
  decision lands at W0 close on samply evidence — policy
  retention is load-bearing if and only if a measurable
  hot-path admission justifies it.
- AUDIT-B §5 — three substrates dead at the scan-policy axis
  (no call sites, zero activation bits, no `__path_walk`
  consumer). **Inherited; same disposition as AUDIT-A §7.**
- AUDIT-C Path B (FusedBuilder collapse) — **executed at
  B5.W1.** The substrate dissolution AUDIT-C prescribed
  arrived through a different mechanism than the audit
  forecast: the welded `FusedBuilder` retired entirely,
  promoting value-side state into `Columns` alongside the
  structural columns and reducing `Parsed<'p, R>` to a 3-field
  record. AY-III dispatches against the post-B5 surface, not
  the AUDIT-C predicted FusedBuilder absorption point.
- AUDIT-D §1 debt ledger (15 items) — **6 closed, 4 routed to
  AY-III, 2 BA-scope, 3 AZ-I/AZ-II-scope.** The closed items
  retired across B4 / B5 / B6; the routed items appear in this
  tranche's wave specs.

**Phase 2 findings** (predecessor lineage):

- B1 closed: divan harness live; nextest CI; pinned toolchain;
  cross-repo pin triad in sync.
- B3 closed: parser-baseline restoration via five forward fixes
  (γ–η).
- B4 closed: `syn::parse2` emit-correctness (W0); unified
  atomic-rollback path + transitional alias retirement (W1, the
  AY-II-I.W0' close ceremony absorption point).
- B2 closed: build-time codegen transposition; `cargo xtask
  regen` is canonical; `crates/derive/` deleted; pre-B2 80-min
  cold rustc-side wall ceases to exist.
- B5 closed: substrate restoration across eight waves; one
  type, two-method parser boundary, single-writer
  `frame_depth` invariant, six god modules + seven W3b
  extensions split.
- B6 closed: dev-loop annex; W0 content-equality skip on regen
  file write delivers 192× cold-wall speedup; W1 + W2
  rationale-satisfied per SPEC §Plan-time miscalibration.

**Predecessor lineage AY-II-I → B-series → AY-III:**

```
AY-II-I (pass II) ─┐
                   ├─→ AY-III (pass III, this tranche)
B1 → B3 → B4 → B2 ─┤
                   │
B5 → B6 ───────────┘
```

The substrate symbol map below applies systematically across
AY-III scope text (post-B5 retirement of pre-B5 weld surfaces):

| Pre-B5 surface | Post-B5 surface |
|---|---|
| `FusedBuilder<R>` | `Tape<R>` |
| `FusedOutput<R>` | (deleted; output IS the tape) |
| `ValueFramesOutput<R>` | (deleted) |
| `ValueBuilder<R>` | (deleted) |
| `value_frame_at` | `frame` |
| `value_payload_for` | `payload_for` |
| `value_children` | `children` |
| `value_frames_output` (Parsed accessor) | `frames` |
| `into_value_frames_output` | `into_frames` |
| `columns_mut()` | `Tape::position() -> u32` + `Tape::rollback_to(open)` |
| `frame_depth_mut()` | (retired; depth managed via `enter_post_order_children`) |
| `extern crate self as bbnf;` | (retired) |
| `Parsed::new_fused_output(...)` | `Parsed::new(tape, input, root_offset)` |
| `note_push` | (retired) |
| `SIB_SKIP_STAMPED_BIT` | (retired) |
| `parse_with_visitor` | (retired; the fused parse IS the visitor lane) |

The AY-II-I wave specs cite the pre-B5 symbols as nm-absence
gates ("nm output: `value_frame_at`, `note_push`,
`parse_with_visitor` absent"); those gates STAY (they verify
post-B5 retirement holds). The mechanism descriptions update.

### Audit consolidation summary

Per `AY-III.md` §Audit consolidation: 9 DROPS, 7 MERGES,
1 PROMOTE, 6 REFINES enumerate the AY-II-I → AY-III mapping.
The drops retire items the substrate already retired; the
merges consolidate parallel waves into denser per-grammar
closes; the promote elevates per-artefact wire-contract tests
to a universal invariant; the refines tighten gate language
against the post-B5 / post-B6 surface.

---

## Planned wave-status table — 2026-04-27

| Wave | Status | Agents | Expected open | Expected close |
|---|---|---|---|---|
| **W0** | planned | 4 parallel (W0.a admission totality; W0.b competitor lane; W0.c samply + nm; W0.d wire-contract + parity-test tightening) | tranche open | W0 close |
| **W1** | planned | 5 parallel (W1.a CSS audit; W1.b CSS materialiser regen; W1.c CSS parity-test tighten; W1.d Sheets parity; W1.e CSS+Sheets samply + AZ-I baseline) | W0 close | W1 close |
| **W2** | planned | 3 parallel (W2.a self-host identity; W2.b BBNF totality; W2.c BBNF samply + bench) + 1 serial closer (W2.final close ceremony) | W1 close | tranche close |

### Wave dependency graph

```
W0 (JSON closure)
  └─→ W1 (CSS L4 + Sheets + AZ-I baseline)
        └─→ W2 (BBNF + close ceremony)
              └─→ AY-III FINAL
```

Each wave gates on the predecessor's close artefact set; no
parallel waves. AZ-I.W0 baseline-bench numbers fold into W1's
final batch fat-LTO matrix run; AZ-I.W0 itself remains a
separate tranche but inherits the baseline artefact AY-III.W1
captures.

### Per-wave open conditions

**W0 opens when:**

- `AY-III.md` on master.
- `waves/W0.md` describes 4 parallel agents with disjoint file
  bounds.
- Each agent's allow-list / forbidden-list verified disjoint at
  plan time.
- Worktrees pre-created (`bbnf-wt-ay-iii-w0-{a,b,c,d}`).
- Master clean (`git status --short` empty).

**W1 opens when:**

- W0 close artefacts present (`post-AY-III-W0-*` benchmark
  files; samply profiles; nm capture).
- W0 hard-gate items 1–12 closed against artefact paths.
- Master clean post-W0 cherry-pick.
- Worktrees pre-created (`bbnf-wt-ay-iii-w1-{a,b,c,d,e}`).

**W2 opens when:**

- W1 close artefacts present (`post-AY-III-W1-*`).
- W1 hard-gate items 1–13 closed against artefact paths.
- `docs/benchmarks/post-AZ-I-W0-baseline.json` exists.
- Master clean post-W1 cherry-pick.
- Worktrees pre-created (`bbnf-wt-ay-iii-w2-{a,b,c,final}`).

### Post-tranche close conditions

AY-III closes when the SPEC §Closing ceremony list is
satisfied: FINAL.md exists; `docs/benchmarks/post-AY-III.json`
covers the matrix; `cargo test --workspace --no-fail-fast`
returns 0 failures; every invariant in `AY-III.md` is verified
against an artefact citation in FINAL.md; every planned item
landed (commit hash) or appears in FINAL's deferred ledger
with named destination.

---

## Floor and escape clause posture

**Declared:** twitter `bbnf_value_twitter / sonic_value_twitter`
≤ 1.15; canada / citm ≤ 1.20; geomean ≤ 1.20.

**Floor:** twitter ≥ 1500 MB/s (≤ 1.50× sonic); 5-fixture
geomean ≤ 1.50; CSS / Sheets / BBNF parity tests green; no
recorded misses on totality.

**Below floor:** AY arc closes on escape per FINAL's deferred
ledger; the architectural lever (direct-to-struct, specialised
inner loops, IR-derived dispatch activation) routes to AZ-I.W2
with named destination.

**Above floor + below declared
(1500 ≤ twitter < 1900 MB/s):** AY-III closes on floor; AY-IV
opens iff a measurable architectural lever surfaces and is not
AZ-I scope. The discriminator is mechanical: a lever whose
prescribed work fits in a single AY-IV wave without silent
deferral opens AY-IV; otherwise AZ-I.W2 inherits.

---

## Operational discipline

1. **Worktree isolation.** Every sub-agent operates inside
   `bbnf-wt-ay-iii-<wave>-<tag>`; the orchestrator owns master
   HEAD; cherry-pick discipline applies. The
   `bbnf-wt-ay-iii-plan-author` worktree is the planning
   context, not an execution surface.
2. **Hard cap per dispatch.** Every dispatch carries an
   explicit time cap per SPEC §Per-role wall-time caps:
   research 20 min; plan 15 min; redress 30 min. At 0.9× cap,
   commit the deliverable; at cap, halt and return per
   §Diagnostic-loop relinquish.
3. **No polling.** Per `feedback_no_polling_loops`, dispatches
   ride `run_in_background=true` plus a Monitor call; no
   `ps aux | grep cargo` loops, no `tail -f` re-checks, no
   sleep-poll for backgrounded cargo runs.
4. **Single cargo per CARGO_TARGET_DIR.** Concurrent cargo
   invocations inside the same target directory serialise on
   the build lock and silently degrade. Sub-agents in
   neighbouring worktrees with shared `target/` symlinks
   coordinate or stagger.
5. **Status tick cadence.** The orchestrator emits a one-line
   status tick every ~5 min of orchestrator-silent wait;
   reconcile TaskList versus `ps aux` plus JSONL mtime before
   every user-facing status reply.
6. **`#[allow(dead_code)]` prohibition.** AY-III invariant 7.
   AY-II-I.W0.e shipped `STRUCTURAL_SCAN_POLICY` with the
   attribute as a stop-gap; AY-III prohibits the pattern in
   source code (macro outputs are exempt where the macro itself
   carries the attribute).
7. **No new features.** Per the user directive (binding); CSS
   L4 grammar extensions land only when an admission already
   shapes the rule. The W1 plan enumerates the carve-out
   discipline.
