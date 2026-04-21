# AUDIT-A — AY Plan Coherence, Scope, and Architectural Ambition

Date: 2026-04-20. Agent: Audit Agent A. Read-only audit of AY plan
and B0/BA/BB/BC neighbours against
`docs/instructions/tranche/SPEC.md`, the AW synthesis, and the AX
handoff. Worktree: `/Users/mkbabb/Programming/bbnf-wt-ay-audit-plan`.

## 1. Was AY's thesis architecturally sound from the start?

**Score: 2/5 — plan under-budgeted scope at least 2×.**

AY's opener declares "one parser, one substrate … grammar-derived
parser writes one canonical packed substrate directly, then drives
the default eager JSON path to near sonic-rs parity" (`AY.md:1-18`).
W8 gates at `bbnf_value_twitter / sonic <= 1.15`, `canada <= 1.20`,
`citm <= 1.20`, 5-fx geomean `<= 1.20` (`waves/W8.md:73-77`). The
floor is "canonical packed substrate + direct JSON write +
view()/to_value()/get() unified + parity gates" (`AY.md:122-135`).

The scope is catastrophically under-budgeted against that floor.
The inherited substrate — flat `Vec<TapeRec>` + `sib_skip` + three
payload columns + arena + `packed_cache` sidecar + `TapeCursor` +
`structural_scan` — was purpose-built through AK-AU-AV-AW-I-AW-III-
AW-IV-AW-V-AX for compatibility, not direct-write.
`AY-critique-path-forward-2026-04-20.md:109-122` flags five
simultaneous debts: `child_off` overloaded across child pointers
and payload interpretations, `packed_cache` as transpose cache not
canonical form, `TapeCursor` carrying pre/post-order compatibility
burden, finalization as second structural pass, `to_value()`
allocating `Vec<...>` compounds. `AYW-SYNTHESIS.md:94-108` shows
the hot-path owner is `<JsonParser>::parse` at 55.19% self-time —
the **emitted parse body**, not any tape-substrate symbol.
Substrate rewrite alone cannot close the sonic gap; the synthesis
explicitly routes "fused parse+value emission" to BA
(`AYW-SYNTHESIS.md:102-107`, `168-170`).

The plan also stacks aspirational levers without floor-checking
them. W2 declares G1-G9 e-graph rewrites + detector retirement +
Named preservation + wrap-elision in one wave at `twitter >= 0.85
bytes/cyc` + `>= 40% record-count reduction` (`waves/W2.md:5`,
`162-171`). Post-execution `AYW-SYNTHESIS.md:28-30` shows **13 of
14 e-graph rules register zero fires** because the normalizer
converges first. The floor-check `SPEC.md:199-213` requires was
never performed.

The invariant stack itself is incoherent. Invariants 1, 8, 13
(`AY.md:45-62`) demand *"no orthogonal parse/runtime paths"*, *"no
substrate addition lands without a same-wave consumer"*, and
*"structural scan/index is first-class same-path infrastructure"*.
Taken together they forbid the in-transit fallback pattern
`SPEC.md:400-409` sanctions for elimination waves. The plan demands
substrate-and-consumer closing at every wave **and** rewriting a
multi-year substrate in-place — one invariant must bend, and
PROGRESS shows which (§5).

## 2. Did B0 and AY's scope boundaries hold under contact?

**Score: 4/5 — B0 held cleanly; AY's internal boundary did not.**

B0 is a textbook prelude annex per `SPEC.md:17-35`. Plan
(`B0.md:1-48`) tightly scoped to command/build/bench/profiling
runway; FINAL (`B0/FINAL.md:86-95`) verifies invariant 1 "No
parser-runtime / semantic-architecture work" via
`git log master --stat 9bff7e7d..7b223cf6 -- 'crates/**/*.rs'` —
empty. Three waves, 14 commits, closed. Every runway artefact AY.W5
needs (`ay-*` Makefile targets, `profiling-prep` profile, idempotent
prebuild scripts) landed. The item B0 did not close — parse-that
SaturationCache retirement — is stashed, not blocking
(`B0/FINAL.md:132-143`).

B0's strength is also the limit of its usefulness. AY.W5's
regression (twitter 746 → 616 MB/s, `PROGRESS.md:596-614`) could
have been diagnosed in 10 minutes with `make ay-samply-json-twitter
WAVE=W5`; W5 admits "not yet diagnosed with samply"
(`PROGRESS.md:614-627`). The command surface was used for hard-gate
evidence but the scope-reveal response in W5 (accept regression as
"recorded miss", hope W6 reclaims) is the anti-pattern B0 was built
to prevent. B0 worked; AY did not pick up the tools.

AY's W0 simultaneously absorbed six parity-critical runtime fixes
(`PROGRESS.md:77-114`, commits `26239370 / a7aded47 / 24d18f42 /
3aa52225 / 4924de74 / acce3a22`) — IR-walker, Pratt detector,
analysis::references collector bugs. That absorption is correct
per `SPEC.md:346-374` but "11 pre-existing test failures" sitting
inside W0's prune scope means AY inherited diagnostic debt no plan
named. The boundary around AY (no runtime-architecture creep into
B0) held; the boundary inside AY (W0 prune vs W2 grammar/IR truth)
did not.

## 3. Are BA/BB/BC the right successors given AY's current state?

**Score: 2/5 — BA weakly motivated, BB adequate, BC orphaned.**

BA's thesis (`BA.md:13-23`) opens *"BA starts from a correct
substrate. It does not redefine the runtime contract landed in
AY."* But at W7 stall, AY has: twitter 548 MB/s (-27% vs W4,
`PROGRESS.md:746-755`), `generated.rs` rolled back to W5-era
because W6 regen broke gorgeous + bbnf-bootstrap
(`PROGRESS.md:727-740`), W7-scoped "BBNF-grammar regen repair"
with no named wave date, `note_push` hook firing on every push
untargeted (`PROGRESS.md:756-762`). The substrate BA starts from
does not exist. `AYW-SYNTHESIS.md:102-107` is blunter: *"The path
to BEAT-sonic runs through fused parse+value emission — a
cross-tranche refactor, not a wave-5-of-8 feature."* BA.md assumes
the inverse (`BA.md:1-11`).

BA's waves compound this. `BA/waves/W0.md:1-9` names *"shared-cost
optimizer second pass"* — but `AYW-SYNTHESIS.md:28-30` shows 13 of
14 e-graph rules dead today. A "second pass" optimiser is a
second-order win on top of an unexecuted first-order optimizer.
`BA/waves/W3.md` file-bounds `crates/jit/src/lib.rs` — a crate
that does not exist in the workspace.

BB is the cleanest successor. Scope (`BB.md:1-23`: generated-code +
cache-key + command surface) is orthogonal to runtime architecture;
contingent on AY close, not predicated on any close state. One
rewrite needed: `BB/waves/W0.md:17-29` lists `generated.rs` as
modify-bound, but `SPEC.md:145-147` forbids hand-patching — BB
needs a regen wave.

BC is structurally orphaned. `BC.md:1-11` opens *"BC is the post-BB
tooling tranche over the BA-close substrate."* The chain AY → BA →
BB → BC presumes each link closed before the next opens. BC's W0
(`BC/waves/W0.md:1-9`) gates *"feature-off regression versus BA
close bounded to <= 3%"* — a number written before anyone could
measure the base. All seven BC waves inherit this deferred-reality
problem.

The correct rewrite: BA re-scopes to AY-closeover per
`AY-critique-path-forward-2026-04-20.md:216-258` (AY.C1 semantic /
C2 honest performance / C3 documentation closure); BB absorbs
current BA compile-time scope plus any orthogonal exceedance; BC
stays planned but not dispatchable until BB closes.

## 4. Was the 3-agent-per-wave parallelism plan feasible?

**Score: 3/5 — The parallelism number is plausible; the file-bound
decomposition is lazy in specific, recurring ways.**

Every AY wave declares 3 or 4 parallel agents (`waves/W0.md:3`, 2
serial + 1 parallel; `waves/W1.md:3`, 4 parallel; `waves/W2.md:3`,
1 serial → 2 parallel → 1 serial; `waves/W3.md:3-4`, 1 serial → 1
serial → 2 parallel; `waves/W4.md:3`, 3 parallel; `waves/W5.md:3`;
`waves/W6.md:3`; `waves/W7.md:3`). `SPEC.md:109-122` caps at 6 and
requires disjoint file bounds; AY's 3-4 is within cap.

The pattern that failed is file-bound decomposition. W1's spec
declared 4 parallel agents (AoS revert + finalise-fuse +
structural-scan + Pratt Option C), but the actual dispatch shape
(`PROGRESS.md:160-186`) was `1+2+1+1 = 5 sub-agents + 1 bench
agent` because W1-A's AU AoS revert (columns.rs) overlapped with
W1-B's finaliser and W1-C's structural-scan consumers on
`crates/core/src/backend/rust/emitter/shapes/*.rs` + `crates/tape/
src/builder.rs`. The PROGRESS entry is explicit: *"The W1 spec
declared 4 parallel agents but file-overlap analysis (columns.rs /
builder.rs / shapes/*.rs) forced phased dispatch per SPEC §Wave
stipulation §Disjoint file bounds."* (`PROGRESS.md:165-168`). That
is not an orchestrator error; it is a plan that declared
parallelism without running the disjoint-bound audit `SPEC.md:114-
116` requires at plan time.

W0's plan dispatched as three parallel agents and needed an
Absorb-mode fourth (`PROGRESS.md:19-21`). W5's plan dispatched
three agents on two phases because "Phase 2 (parallel)" was
actually sequential-within-parallel (`PROGRESS.md:517-523`). W6
dispatched three agents but the regen then broke the workspace
(`PROGRESS.md:727-740`); the consolidator pattern `SPEC.md:156-
166` that was supposed to be named at plan time is not named in
any AY wave.

The failure mode is uniform: AY wave plans declare 3-agent
parallelism over files where the architectural change crosses
every agent's bound (tape substrate work, emitter work, view
work all cross `columns.rs`, `builder.rs`, `generated.rs`). The
decomposition is lazy because it stops at the crate boundary; the
actual seams are payload-layout × shape-kind × consumer-surface,
not crate-by-crate. A plan that split W1 into (AoS layout
change / write-path back-patch / finaliser body retirement /
structural-scan substrate only) would have had disjoint bounds;
AY's did not.

## 5. Is the "close with recorded misses" pattern a drift indicator?

**Score: 1/5 — This is the AV anti-pattern resuming under AY's name.**

`AYW-SYNTHESIS.md:30-43` identifies *"AY added surface (structural-
scan, G1-G4, phf.rs, handle.rs, materialize_* fns) before retiring
the competing surfaces it superseded"*. That is substrate-without-
activation, `AW/audit/SYNTHESIS.md:27-58` anti-pattern #1.

AY's wave table (`AY.md:86-96`) reports **5 of 7 closed waves
closed "with recorded misses"** (W2, W3, W4, W5, W6). Every one
of those misses is a same-wave consumer gap:

- **W2** (`PROGRESS.md:301-311`): `PROJECTION_DIRECT_TO_STRUCT`
  below threshold; record-count reduction below projection;
  detector retirement did not land. The G3 wrap-elision rule fired
  on 3 BBNF rules and zero JSON rules — the target was twitter
  record-count -50% (`waves/W2.md:162-171`), delivered -9%.
- **W3** (`PROGRESS.md:361-370`): eager `bbnf_value_twitter /
  sonic_value_twitter = 3.633×` against a `<= 1.0` gate; lazy lane
  acknowledged-synthetic at 2953× sonic because bbnf still parses
  the full tape.
- **W4** (`PROGRESS.md:429-438`): regex self-time 29.18% vs 12%
  gate; canada f64 gain not observed.
- **W5** (`PROGRESS.md:596-632`): twitter -17% regression; root
  cause speculated (note_push on every push) but not diagnosed
  with samply.
- **W6** (`PROGRESS.md:743-762`): twitter -11% further regression
  (cumulative -27% vs post-W4); regen broke workspace and was
  rolled back.

Every close says "the consumer lands in W{N+1}". This is exactly
the chronic pattern `AW/audit/SYNTHESIS.md:27-58` names in AL-AV:
*"Emission lands; consumer doesn't; hard gate closes on 'code
exists'; runtime never fires."* AY is reproducing it at 5/7.

The discriminator `SPEC.md:218-223` makes runtime-verifiable:
every activation gate requires a same-wave consumer. AY.W5's gate
2 reads *"samply on eager JSON twitter shows `finalise::finalise`
at <= 1% self-time"* → closed as *"SOFT-PASS (samply delegated to
W6 samply hard-gate)"* (`PROGRESS.md:637-644`). That is a
delegation, which `SPEC.md:228-236` §Gate-off commits calls a
deferral. AY.W6's gate 3 *"Samply on JSON twitter path lookup:
child-walk ≤ 1%"* → closed as *"SOFT-PASS (samply delegated)"*
(`PROGRESS.md:766-774`). Sampling has been deferred continuously
since W3.

What breaks the cycle: stop naming the missing evidence a soft-
pass. Every SOFT-PASS in AY is a wave close that the orchestrator
should have refused per `SPEC.md:276-278` — *"Substrate-without-
consumer is rejected at wave close."* The cycle continues because
the plan's wave-status column admits "complete with recorded
misses" as a closed state; the SPEC does not. Either AY's own
wave spec (`AY.md:90-96` table) is lying about what closed, or
SPEC §"Substrate-without-consumer is rejected at wave close" is
being silently waived. The current W7 stall on a rollback-
invariant bug in `TapeBuilder::note_push` (the hook introduced in
W5 without its consumer story proved) is the cycle biting: five
closed-with-misses waves produced a W6 bench that W7 must reclaim
*and* a regen that W7 must restore *and* a shared-fact optimizer
W7 is supposed to land *and* a `note_push` consumer bounds check
W7 now also owns. That is four deferrals landing in a single
wave, which is exactly what `SPEC.md:307-318` §Scope-reveal
protocol names as a new-letter trigger.

## Overall verdict (≤ 200 words)

AY's thesis is internally elegant and historically unsound. The
"one parser, one substrate, one pass, near-parity" framing
re-inherits three separate substrate regressions (AU → AV AoS/SoA
pivot, AW-V wrap-compound overadmission, AX tape-first value bias)
and asks 9 waves to unwind them while holding every invariant
simultaneously. The plan under-budgeted scope at least 2× and
embedded a reviewer-unfalsifiable escape valve ("complete with
recorded misses") that has now fired on 5 of 7 closed waves. B0
held cleanly and delivered its runway; AY did not use the runway
for diagnostic speed. The successor plans (BA/BB/BC) are
architecturally incoherent against AY's actual close state: BA
presumes a substrate AY has not produced, BB is the only
dispatchable successor, BC is two tranches out of reach. The W7
stall is not a local bug — it is the cumulative bill for W2/W3/
W4/W5/W6 closing on un-activated substrate. The correct move per
`SPEC.md:358-374` is to close AY on what landed, open a new letter
for the substrate rewrite that `AYW-SYNTHESIS.md:V` calls
"different emission shape, not wave-5-of-8", and stop carrying
the deferral forward.

**Overall score: 2/5.**
