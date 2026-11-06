# SK-V25 — THE RE-ADJUDICATION AND THE PERFECTED UNION

**A handoff packet, not an execution order for its author.** This packet hands the campaign
to a NEW long-horizon session running in Codex, with the model fleet transposed: everywhere
the prior campaign said **Fable**, the new session uses **ChatGPT Sol 5.6**; everywhere it
said **Opus**, **ChatGPT Luna 5.6**. The packet set is three documents in this directory:

| Doc | Contents |
|-----|----------|
| HANDOFF.md (this) | The mission, the revelation, the re-adjudication protocol, execution order, the tail, operational law |
| PROMPTS.md | The verbatim binding prompts (design loop, tranche formulation, the value.js tranche prompt, the structure edicts) + the transposition table |
| INVENTORY.md | The complete done/remains/taint ledger with exact repo pointers, plus the constellation marks |

Read all three before any dispatch. The prior campaign's durable state lives at
`restart/skinny/tranches/sk-v24/` (CAMPAIGN-STATE.md is its resurrection ledger) and
`docs/tranches/sk-v24/` (the GN/PT codex). Nothing in sk-v24 is deleted by this handoff;
it is the corpus under re-adjudication.

## §0 THE MODEL-TRUTH REVELATION — why this campaign exists

It has become known that **the "Fable-tier" subagent flows of SK-V24 never ran on Fable.**
The orchestration scripts left judgment stages (directive distillers, synthesizers,
agglomerators — and latterly researchers and critics under the owner's Fable edict) without
an explicit model override, on the belief they would inherit the session model. They did
not. Measured census from the final pt pass-3 run (task output wefinh05z, journal
`subagents/workflows/wf_acfc23d2-04b/`): **49 of 60 agents recorded `claude-opus-4-8`; only
6 recorded `claude-fable-5`** — and the recorded-Fable handful does not cover the judgment
stages that stamped every registry verdict. Every convergence percentage, every
ADVANCED/BANKED/BLOCKED/RETIRED status, every cross-lock adjudication, and the framing prose
of the frozen SPEC and the GN/PT codex was authored at a lower tier than the campaign's own
model-routing law demanded.

**THE MODEL-TRUTH GATE (born-RED; the new session's FIRST action).** Before any judgment is
trusted or produced: every agent spawn declares its model EXPLICITLY — inheritance is
forbidden, this entire campaign is the proof of why — and the harness's per-agent model
metadata is verified against the declaration for the first batch of every new workflow
shape. A batch whose recorded models mismatch its declarations is discarded and re-dispatched.
This gate never retires; it is checked at every pass boundary.

## §1 The fleet and its law

- **ChatGPT Sol 5.6** owns ALL orchestration, synthesis, adjudication, critique,
  challenging, novelty, and every cognitively complex call — the full Fable role,
  including the owner's late edict that researchers (novelty developers) and adversarial
  critics run on the top tier.
- **ChatGPT Luna 5.6** takes mechanical sweeps and implementation fanout only.
- Every spawn declares its model explicitly. Dispatch in batches of 5–6 concurrent agents
  against the rate wall (the prior campaign proved walls are real: three session-limit
  walls and one process-exit in one day; its width-throttle semaphore pattern and
  resurrection ledger are described in INVENTORY.md §F and should be re-created in the
  Codex harness's idiom).
- Durability is law: a committed state file updated launch-intent-first, a run ledger with
  exact resume arguments, a walls log, a non-spammy heartbeat, and cache-aware resume.
  A process exit kills schedulers silently — every resurrection re-arms the heartbeat
  FIRST, then reconciles the ledger.

## §2 What was done hitherto (the corpus under re-adjudication)

Full pointers in INVENTORY.md. In one breath: SK-V24 ran recon (7 cartographers) → a
14-prototype proving fleet → adversarial verification → the frozen SPEC
(`restart/skinny/tranches/sk-v24/SPEC.md`: the certified performance swath + three
refutations-as-law) → the ~47k-LoC implementation codex (`docs/tranches/sk-v24/`, GN-0..6 +
PT-1..3, full-implementation depth) → THE LOCKS: nine locks × four families under the
convergent design loop (bbnf {1,2,3,7} through pass 2; pt {4,5,8,9} + weave {6} through
pass 3), nine registries current, a CROSS-LOCK ledger of eight adjudications (XL-A..XL-H),
and a second owner pause sealed with the tail enumerated. The campaign survived three
session-limit walls and a process exit with zero artifact loss.

## §3 The taint census — what re-adjudication targets

Split the corpus into MACHINE FACTS and SUSPECT JUDGMENTS:

**Machine facts (re-runnable, not overturnable by re-labeling):** every measured number —
the PROVE-01..14 cold-paired PMU measurements and their adversarial re-measurements; the
prototype runs inside THE LOCKS (byte-identity gates, conflict walks, parity counts like
the 56/56 grammar-driven regex DFA parity, the 1.56× canada ceiling, N*=3.0); files on
disk; git history. These stand as evidence. Their INTERPRETATION does not.

**Suspect judgments (every one re-adjudicated):** all nine `locks/LOCK-N/REGISTRY.md`
verdicts, statuses, and convergence percentages; the CROSS-LOCK XL-A..XL-H adjudications;
the SPEC's framing and its §3 re-cut checkboxes; the GN/PT codex's architectural choices
and wave shapes; the pass directives; the critiques (the "checklist hits" themselves may
be wrong in both directions — misses and false alarms).

## §4 THE RE-ADJUDICATION PROTOCOL — the apotheosis loop

ALL workflows and work hitherto is analyzed thus, per lock / per work item, in batches of
dynamic, bespoke size cut to the artifact clusters:

1. **TWO Sol critics, independent, who ASSUME OUTRIGHT THE WORK IS WRONG.** Not "review
   for errors" — the stance is that the verdict under examination is false until the
   evidence compels otherwise. Each returns enumerated findings with file:line evidence,
   re-run commands, or named counterexamples; status reports and vague doubt are rejected.
2. **A THIRD Sol adjudicator** agglomerates both critiques and **PROVES** each contested
   fact correct or incorrect — by re-running the probe, reading the substrate at HEAD,
   or exhibiting the counterexample. No fact settles on authority; it settles on proof.
3. The output per item is its **APOTHEOSIS**: a proven-fact registry row set — CONFIRMED
   (with the proof), OVERTURNED (with the disproof and the corrected verdict), or
   UNPROVEN (with the exact experiment that would decide it, scheduled as work). The
   apotheosis registry supersedes the prior registry by named supersession; the prior text
   is never edited, only superseded.
4. Iterate per the convergent design loop (PROMPTS.md, Prompt I) until two consecutive
   passes surface nothing new. The prior campaign's registries become pass-0 input to
   their own apotheoses.

The re-adjudication is not a purge — it is expected that much survives. What it produces
is the right to say so.

## §5 EXECUTION ORDER AND THE CONSTELLATION

**The parse-that uplift comes FIRST.** After (and interleaved with) its re-adjudication,
the parse-that lock set's proven residue becomes the first implementation target — the new
session works parse-that-side, so the old bbnf-side READ-ONLY fence converts to ordinary
care: coordinate with any active value.js/keyframes.js sessions, respect their WIP, and
place coordination letters as a parse-that-side actor lawfully may.

**The constellation is inextricable and must be marked as such in every plan:**
- **bbnf-lang ↔ parse-that**: one organism, two repos. The regex engine's canonical home
  is parse-that (one-regex-home); bbnf-lang vendors for measurement. The lock sets weave
  through both; GESTALT-Ω is defined over the pair.
- **parse-that ↔ value.js + keyframes.js**: parse-that's flagship consumers. parse-that
  master is at 1.0.0 (the S-series legacy cut: *Span excised, SpanParser killed, packrat
  epoch armed behind PACKRAT_ARMED) with the live ask-letter **VALUEJS-PT-E** (value.js
  S.W1 asks for 1.1.0+: diagnostics HIGH, inference MED, Pratt-dormant record). value.js
  is mid-mega-tranche on branch `tranche-u` (Glass UI 7.0.0 adopted across the demo
  surface; the vnext kickoff packet landed as "r2 true-Fable" — that repo has already met
  the model-truth problem and named it). keyframes.js sealed its tranche-u release packet
  and consumes Glass UI 6 from one registry core. Exact marks in INVENTORY.md §E.
- The old XL-G owner question (is value.js divorced from parse-that?) is answered by the
  constellation itself: **the coupling is alive** — VALUEJS-PT-E is an active ask. Carry
  this as a proven fact into the apotheosis of XL-G.

**Isomorphism law for parse-that:** TS ↔ Rust isomorphism is PARAMOUNT — in layout and in
code, to a reasonable point — but the regex engine and the low-level facilities (SIMD
kernels, tape internals, scanners) are NOT duplicated across languages. One home each;
the other language consumes through the narrow certified crossing (the tape contract,
the build-time emitter face per the XL-D evidence, pending its apotheosis).

## §6 THE DOGFOODING MANDATE — value.js as the proving consumer

The most recent value.js tranche development (branch `tranche-u`, and the active Codex
session developing V) shows how these parsers are USED, and is both a source of
requirements and the dogfooding bed for the perfected union. Mark and carry these items
(the full owner prompt is verbatim in PROMPTS.md, Prompt III):

- value.js owns core CSS (July-2026-modern, stabilized experimental/Chrome features):
  total spec coverage, idiomatic and performant parse-that usage.
- The ad-hoc-addressed defects **mixColors** and **parseCSSValue** — re-examined properly.
- The **custom non-parse-that parser** inside value.js: find which tranche begat it and
  why; measure its benefits/downsides; it is held unreadable and is expected to be
  supplanted by proper parse-that adoption — this is the single sharpest dogfooding test
  of parse-that's TS face (tape adoption, allocation reduction, modern V8 idioms).
- keyframes.js parsing facilities ruthlessly examined: value.js owns core CSS, keyframes
  the animation-specific grammar — or all parsing consolidates into value.js with
  keyframes as a consumer; either way the two libraries' compiler/parser directory
  structures achieve isomorphism and perfection (the `src/animation/internal` structure is
  named as disliked; `subpaths/` as a module is named code-smell-supreme; NO SHIMS).
- Color: near-perfect zero-alloc facilities for all spaces and transforms; the iterative
  out-of-gamut algorithm ruthlessly interrogated; best SOTA.
- Frontend focus lands on value.js; glass-ui defect findings are batched and sent to the
  working glass-ui agent without interrupting it; the pre-Glass-7.0.0 audit debt carries.

New features for bbnf-lang and parse-that are brainstormed WITH these consumers in the
room: a feature that does not serve the constellation is substrate without a consumer.

## §7 MODULE STRUCTURE AND THE DAG LOOP — until dirigibility

All deferred colocation and modularization work is addressed NOW, in-repo (bbnf-lang and
parse-that), using the most recent value.js/keyframes.js tranche structures and glass-ui's
flattening and component-structuring idioms as the reference. The edicts (verbatim in
PROMPTS.md, Prompt IV):

- Components/modules COLOCATED with their sub-components, composables, constants,
  recursively; truly module-level items in a `composables/`-style dir; same for backend,
  abstracted befittingly.
- Long-running directories pruned or grouped into logical sub-modules. **Goldilocks
  granularity**: too macro begets god-modules; too fine begets sand.
- **Module-name stripping**: files in a module drop the module's name from their own
  (`src/animation/compile/easing/{easing-option,easing-config}` →
  `easing/{option,config}`) — a general de-duplication mechanism.
- **Tests never colocated**: displaced into a file structure isomorphic to source.
- More code is not better: pithy, laconic, fastidious.
- **The DAG loop**: build the dependency DAG of the library and component structures —
  every node, every edge, every cycle — and analyze it in batches of dynamic bespoke size
  cut to the graph clusters, each batch viewed THRICE: two fresh Sol perspectives that
  ASSUME THE STRUCTURE IS WRONG, and a third Sol adjudicator who proves or disproves the
  findings. Iterate, encapsulated, until library perfection, readability, and
  DIRIGIBILITY is reached.
- Pruning and deletion of entire sectors is granted totally IFF the finding is vacuity or
  superfluity — consumer count alone is NOT enough. NO quick solutions, NO workarounds,
  NO legacy code, no aliases, no shims, no dual paths, no masking fallbacks. Breaking
  library changes are allowed.

## §8 THE NOVELTY MANDATE

For both bbnf-lang and parse-that: brainstorm and develop new, novel, elegant, and
performant features — the certified swath is a floor, not a ceiling; the refutation laws
are fences (pending their own apotheoses). Novelty work routes to Sol by the fleet law.
The prior campaign's proven seeds (the grammar-driven regex at DFA parity, the type plane
at zero runtime cost, the tape crossing, direct-to-struct, the prefilter tier) are the
launch points; the GESTALT tier is where they fuse into the perfected union: a parser
generator that bests every SOTA parser for the targeted structured-data languages.

## §9 THE TAIL — carried whole, so it does not fall off

Every item below enters the new session's registry with an owner. None may silently drop.
(Statuses are the prior campaign's own and are themselves subject to re-adjudication.)

1. The six pending cross-lock cuts from the second pause: the 5C→LOCK-2 SPLIT ruling;
   GRAFT-3 (class-scan-vs-DFA in two homes must become one dispatch surface, born-RED);
   the Axis-1 two-types re-cut of 4D/9C/3C (ownerless — assign a charter); the TS Dfa
   port's owning lock (homeless; G-C1 DAG-blocked until named); the land-to-HEAD
   obligations (X8 clash arm, bounded_sum, TypeDesc::Row — all worktree-earned,
   substrate-absent); the stale-gestalt process guard (thrice recurred: reconcile every
   in-folder prototype before any registry cut — institutional).
2. bbnf pass-3 (convergence talk opens) and pt pass-4 — now folded INTO the apotheosis
   loop rather than run blind atop suspect registries.
3. Per-lock fresh non-author audits; two consecutive clean passes; then
   GESTALT-B ∥ GESTALT-P → GESTALT-Ω: the terminal artifact — the perfected dual-repo
   tranche set with DAG ordering, born-RED gates, π obligations, and a disposition for
   every banked route, superseding the v1 codex by name.
4. Owner gates: SPEC §3 re-cut checkboxes; the BBNF-PT-G letter (its placement law
   dissolves into ordinary coordination now that the session is parse-that-side, but the
   OWNER ratification of its content still gates); XL-G P2.
5. The GN/PT codex execution (old task #54) — awaits the apotheosis-corrected v2 set.
6. Every WITHHELD list in the nine registries (they steer what researchers must not be
   told); every named reactivation experiment for BANKED families (R-C2, B3/B4, the
   /foo$/ Look consumer, PROVE-2-PROMOTE, PROVE-2-DIST, 2D's first fit, the 4A crate-seam
   keystone, the 4C canada crossing, the 4B padded-input contract, 5D's Q7 wired-fraction,
   9C's unblocking on 5D's ByteClass substrate).

## §10 Operational law carried (unchanged unless re-adjudicated)

- **No x86, anywhere, either repo** — aarch64/Apple-M only; NEON-only (no SVE/SVE2/SME).
- **Bench law**: cold per-parse, paired interleaving, PMU cyc/B for certification, loadavg
  recorded, N≥200; warm benches are disinformation; every measurement names its HEAD
  (base law); stale-base numbers are wrong-artifact-risk by definition.
- **Generated files are regen-only** — never hand-patched.
- **Parsimony + implementation-majority**: extreme parsimony and fastidious care; KISS-
  forward; fewer lines; the greater library picture; little time on contrived gates or
  process, the majority on direct code implementation through agent orchestration and
  visual verification. Adhere to the wave spec exactly.
- **Durability**: committed state file, launch-intent-first ledgering, walls log,
  heartbeat re-armed first on every resurrection, cache-aware resume, never
  duplicate-launch, reconcile ledger against live tasks before any launch.
- **Verbatim-binding prompts**: PROMPTS.md's texts are law as written (post-transposition);
  they are not summaries to be re-derived.

## §11 First actions for the new session

1. Read this packet set + `sk-v24/CAMPAIGN-STATE.md` + the nine registries +
   `sk-v24/locks/CROSS-LOCK.md` + `sk-v24/SPEC.md` + `docs/tranches/sk-v24/INDEX.md`.
2. Stand up durability (state file, ledger, heartbeat) in the Codex idiom.
3. Fire THE MODEL-TRUTH GATE on a probe batch; prove Sol is Sol.
4. Open the re-adjudication with the parse-that lock set (4, 5, 8, 9 + weave 6) — the
   parse-that uplift is first — while a parallel arm builds the module/component DAGs of
   both repos for the structure loop (§7).
5. Proceed per PROMPTS.md: the design loop for re-adjudication passes; the tranche
   formulation prompt to cut the apotheosis-corrected tranche set; the value.js prompt
   binding the dogfooding arm.
