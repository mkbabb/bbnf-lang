# SK-V24 SPEC — THE GREENFIELD NOVUM CODEX (FROZEN 2026-07-16)

The plan of record for the perfected union: one parser-generator in Rust emitting hybrid
Rust+NEON, parse-that as the regex home and TS backbone, one unified value surface. Every
load-bearing claim herein was IMPLEMENTED and MEASURED this campaign (stage 2, 14 provers),
then ADVERSARIALLY RE-MEASURED by non-author verifiers (stage 3, 12 skeptics). Artifacts:
`prototypes/PROVE-XX/{REPORT.md, patch.diff, VERIFY.md}` — the patches are the code of
record and land as born-verified waves.

Freeze note (meta-adjudication, per HANDOFF §2): the owner's §1.5 amendments substituted
implementation-majority for pass-count ceremony. This codex froze after one deep
recon→prove→verify cycle because its substance is not speculation to converge on — it is
measured code with independent adversarial certification. The answer to "why will this plan
be executed rather than re-planned": most of its waves BEGIN from working, verified patches;
the plan is the landing sequence, not the invention.

---

## §0 THE CERTIFIED SWATH (the performance-proven features)

Cold per-parse, paired interleaving, PMU cyc/B where macOS allows; ratios certified,
absolutes directional. "Verify" = independent adversarial re-measure, fresh windows.

| # | Feature | Prove | Verify | The number that survived |
|---|---------|-------|--------|--------------------------|
| 01 | NEON shufti/truffle prefilter, fused GATE=16 | PARTIAL | **CONFIRMED** | skip-heavy ~7.5×/4.7× (floor 0.115 cyc/B, fair ceiling ~9× vs 1.03 cyc/B membership scan); dense twitter 1.03-1.05×, bootstrap 1.08-1.15× — dense-neutrality robust |
| 03 | Fused NEON string scan (vshrn narrow) | GO | **CONFIRMED** | twitter parse_tape 0.898-0.931× cyc/B; tape/sonic 0.512→0.445; fused/sonic 0.643→0.556; ins/B −9/−12% deterministic; won in BOTH interleave slots |
| 05 | CSS shufti token kernel | PARTIAL | CONFIRMED_DELTA | tailwind −23.6% (lcss ratio 3.25→**4.16×**), material −16.7%, bootstrap −14.6% (prover's −20.4% overstated); animate +13.4% regression is STRUCTURAL (+30% ins) — redress owed before landing |
| 06 | Stop-early path get (tape-free, NEON skip) | GO | **CONFIRMED** | vs sonic-rs get: shallow 0.78-0.79, missing 0.68 (wins), deep 2.01 (loss, SIMD key scanner owed); resolves shallow path at 0.032% of doc |
| 07 | Direct-to-struct fused sink | PARTIAL | **CONFIRMED** | beats sonic-rs typed 0.746-0.786 paired and serde on all 3 corpora; citm 1.85-1.92× vs serde; twitter-vs-serde 2× bar honestly unmet (thin projection) |
| 08 | Type plane: schema derived from grammar (HM core, 221 LOC) | PARTIAL | CONFIRMED_DELTA | derives validated DirectSchemaSet in ~230µs compile-time, ZERO runtime cost (held); the three walls are REAL and are the GN-3 spec (below) |
| 10 | bbnf-regex prefilter tier (Dfa::find) | GO | **CONFIRMED** | 8.8-29.5× on prefilterable patterns, 0.97-1.01× controls, compile-neutral, 44/44 tests, 245-line additive patch |
| 11 | parse-that TS lazy diagnostic alloc | GO | CONFIRMED_DELTA | then-chain +13.6%, fused-all +15.6% (exceeds claim), fused alloc −60.1% (exact); dispatch lane only ~+5% (claim halved); 124/124 tests both arms |
| 12 | Emit-spine residualization (const-fold delimiter mask) | GO | **CONFIRMED** | CSS cyc/B 0.80-0.91× (tailwind full 0.825 fresh, better than claimed), ins/B −4.8/−6.8% deterministic; regen bit-for-bit; mask-as-immediate disasm-verified; +18 generated LOC |
| 13 | CSV through the one generator, name-gate cured | GO | UNVERIFIABLE (perf) / **verified (generality)** | correctness (RFC4180 quoted/escaped/CRLF) reproduced PASS; shape-predicate cure verified genuine; prover's 0.948× vs csv-core stands DIRECTIONAL — fat-LTO re-measure is GN-5.W1, born-RED |
| 14 | Offset-tape decoded in TS | PARTIAL | **CONFIRMED** | byte-exact node-for-node across 320 cold children; stop-early get 4.4-4.6× vs JSON.parse; full-walk ~0.91× (2× honestly unmet — native parse is near-optimal for full materialization) |

### Refutations that are now LAW (negative results, measured)

- **02 NEON Mula/Lemire digit parse — NO_GO.** −37/−48% instructions but latency-bound
  inversion (CPI 0.13→0.30) regresses the product lane. The canada redress targets FLOAT
  CONVERSION (the materialize_f64 / Eisel-Lemire tax, +1.48 cyc/B), never the digit scan.
- **04 Multi-block software pipelining — refuted as a lever.** 1.05-1.06× only: the M-class
  OoO core already pipelines the single-block loop; classify is ALU-bound on the movemask
  bit-stitch. The F1 lever is the MOVEMASK NARROW + vectorized compaction, not block scheduling.
- **09 portable_simd→NEON shim perf claim — REFUTED by the verify fleet.** The 1.34-1.57×
  did not reproduce (parity-to-slower; the shim emits 2× the movemask reduces). The
  de-nightly PORT survives as clean engineering with honest value = stability, not speed.
  Standing lesson: two-arm window asymmetry is the classic self-deception; paired
  interleaving is mandatory even for "obvious" wins.
- Carried from prior campaigns, still binding: fold-over-lazy-cursor dead (2.3-4.9× behind);
  tape-navigation is NOT the get lane (06 measured it at 150-1700× the stop-early cost);
  AoS-vs-SoA question is MOOT at HEAD (tape is offsets+sparse-flags+arena; do not relitigate);
  M-series is NEON-only (no SVE/SVE2/SME for byte work); warm benches are disinformation.

---

## §1 THE UNION ARCHITECTURE

**bbnf-lang** = the generator: grammar (.bbnf, `->` projections) → typed IR → decision
driver → emit spine that residualizes (12) → hybrid Rust+NEON parsers over one offset-tape +
value fabric. **parse-that** = the one regex home (bbnf-regex: HIR→NFA→DFA→prefilter tier
(10)) and the TS combinator backbone (11), consumed not duplicated. **The value surface** is
one API with four proven lanes: eager tape parse, stop-early tape-free get (06),
direct-to-struct fused sink (07, schema grammar-derived per 08/GN-3), and the cross-language
tape (14) seeding the TS backend.

**The differentiator** (the named answer): no other parser generator emits
grammar-specialized SIMD kernels behind a cost model with a grammar-derived type plane and a
unified lazy/eager/typed value surface — simdjson/sonic-rs are hand-written single-grammar
engines; lightningcss is a scalar hand lexer; LR/PEG generators emit interpretive automata.
bbnf's proven position: JSON tape 2.2× sonic (with 03), CSS 2.4-4.2× lightningcss (with
05+12), CSV at csv-core parity from a GRAMMAR (13), stop-early get 1.3-1.45× sonic (06),
struct decode beating sonic typed (07) — one generator, five grammars, zero name-gates.

**Skinny is RETIRED** as a tree: its proven organs (bbnf-simd kernels, codegen emit spine,
runtime tape, xtask regen, canon benches) port into the greenfield workspace under the
colocation edict; everything else archives. The 14 patch.diffs apply to those organs during
the port — the greenfield is born with its wins installed.

---

## §2 THE TRANCHES — bbnf-lang (GN series)

Execution order: GN-0 → GN-1 → {GN-2 ∥ GN-3} → GN-4 → GN-5 → GN-6. The archival executes
FIRST (the owner's ordering: archival proceeds once tranches are formalized — this freeze IS
the formalization). Every wave lands with its gate; gates are born-RED only where a residual
is named — waves landing verified patches are born-verified and gate on non-regression.

### GN-0 — THE GREAT ARCHIVAL + x86 PURGE
SK-V22 archive mechanics verbatim (umbrella tag `pre-greenfield-2026-07`, 9 navigational
tags, 5 rescue checks, tags-before-removals, `archive/legacy-crates` as a BRANCH, ~202G
reclaim), amended: skinny/ is tagged and retained until GN-1 port-in completes, then
archives with the rest. x86 purge: root `crates/simd-scan/{avx2,avx512,wasm}.rs` (PB-2 never
landed — R5), generated-file AVX stamps, all-x86 CI, workspace lints, aarch64 CI assertion.
Playground archives outright. Gate: rescue checks green; repo top-level is the new shape.
W-final: audit vs sk-v23 research/09 pre-map (carried residual).

### GN-1 — THE LOOM (the greenfield workspace)
The new tree of record, colocation edict normative: directory-modules owning their kernels/
constants/tests-adjacent-structure (tests in tests/ per standing law); no flat sibling
sprawl; long dirs broken and encapsulated. Port-in: bbnf-simd (with 01+03 patches), codegen
(with 12), runtime tape (with 06 template changes), parse-that-regex vendored crate, xtask
(one regen host; CSV target from 13), bench canon (json_canon_bench now TRACKED + css_canon
+ NEW: extend paired mode to CSS — R2 gap). Land 05 behind its animate redress flag OFF
until GN-2 clears it. Gate: full regen bit-for-bit; canon benches reproduce §0 numbers
within noise on the ported tree (non-regression, born-verified).

### GN-2 — THE KERNEL PLANE
W1: prefilter end-to-end (01's disclosed seam residue): passes::derive_recognizers emits
SimdMode::Prefilter for self-loop scan sites; codegen lowers to find_member; the ≥15×
becomes REAL against expensive consumers (string/DFA scanners at 2-5 cyc/B) — born-RED gate:
a wired lane beating 8× on a real grammar scan site. W2: string-kernel follow-on — route the
generated scalar match_tiny_plain_string path to first_string_terminal (template + regen);
gate: twitter parse_tape compound win vs GN-1 baseline. W3: CSS animate redress (05): hoist
per-call LUT build + short-span scalar guard; born-RED: animate ≤1.0× with the 3-corpus wins
retained; then flip 05 ON. W4: the movemask-stitch lever (04's finding): attack the 0.78-0.84
cyc/B classify floor via narrow/compaction, NOT block scheduling. W5: canada float redress
(02's law): the Eisel–Lemire materialize lane (deferred-materialization or SIMD mantissa
assembly); born-RED: canada fused/sonic ≥0.95× (currently ~1.20×).

### GN-3 — THE TYPE PLANE (the three walls, now the spec)
Wall 3 is already half-fallen: ExprKind::Project EXISTS at HEAD (verifier-confirmed) —
thread it into infer and DirectBuild.type_ref (08's patch shows the seam). W1: sum types in
DirectTypeKind (Wall 2) — the grammar's value = object|array|... becomes representable;
HM core (221 LOC, proven) grows unify over sums. W2: document-shape projections (Wall 1) —
typed grammar variants author the shapes (twitter_search.bbnf with `->` chains); the hand
schema real_typed_schema.rs DIES, replaced by derivation; gate: derived ≡ hand schema on the
twitter/citm/canada shapes, then delete the hand file (born-RED until deletion compiles).
W3: 07's lane becomes fully grammar-driven; gate: §0 direct-to-struct numbers hold from
derived schemas; type plane remains ZERO runtime cost (verified invariant).

### GN-4 — THE VALUE FABRIC
W1: productionize stop-early get (06) as the public path API (pointer + typed get + prefix
parse), colocated grammar-side, sheets untouched. W2: SIMD key scanner to close the
deep-path 2.0× loss — born-RED: deep ≤1.2× vs sonic get (from 2.0). W3: direct-to-struct
public API + serde bridge (07). W4: the TS tape decoder (14) graduates to the TS-backend
seed: wasm-bindgen export of the four public tape planes; stop-early get in TS ships (4.4×
proven); full-walk lane documented as non-goal (native parse won, measured). W5: unified
Value docs + oracles: the four lanes, one enum surface, parity oracles as the close gate.

### GN-5 — GENERALITY + THE FLEET
W1: CSV fat-LTO release re-measure (born-RED: ≤1.0× vs csv-core under release; directional
0.948× to confirm) + the csv high-level bulk-lane gap (read_byte_record shape, 1.36×) as a
cost-model target. W2: TOML grammar authored + wired (no TOML exists anywhere — R1). W3:
overfit audit: cure json_sink_direct's internal 'value' residue (13's disclosure) with the
same shape predicate; grep-gate: zero grammar-name predicates in codegen (CI-enforced).
W4: sheets carried + oracle fleet consolidation; 12's const-mask generalized to every
grammar-derived stop-set. Gate: five grammars (json/css/csv/toml/sheets), one generator,
canon fleet green.

### GN-6 — THE SHOPWINDOW + CLOSE
Playground greenfield (concept only until here — wasm on the GN-4 seam), durable docs home
(root docs/: ARCHITECTURE, PERFORMANCE-THESIS with §0 as its evidence table,
NEGATIVE-RESULTS folding §0's refutations, BENCH-PROTOCOL = the standing law), the
differentiator statement published, the all-prompts recap audit, quiet-host re-capture of
the §0 table (carried residual — ratios stand, absolutes get a quiet stamp).

## §2b THE TRANCHES — parse-that (PT series, FULL IMPLEMENTATION)

**SUPERSESSION (owner order, 2026-07-16, post-freeze — binding):** the PT series is elevated
from review-not-execute proposals to FULL IMPLEMENTATION tranches — "fully-upgrade parse-that
(and the regex engine thereof)... a full implementation, not just a skinny pass." The
implementation-depth tranche documents are docs/tranches/sk-v24/PT-{1,2,3}.md, which develop
PT-N1-3 below into executable wave sets. Coordination survives as mechanics, not as a fence:
each PT tranche opens with a W0 handshake (letter BBNF-PT-G + owner acknowledgment + rebase
over any active WIP + consumer baselines); execution runs in the parse-that repo by the owner
or a parse-that-side session after that handshake; bbnf-side agents still never write into
~/Programming/parse-that, and the letter placement law is unchanged. The consumer suites
(value.js, latex-paper, keyframes.js) remain the close gates.

- **PT-N1 — the prefilter tier**: PROVE-10's 245-line additive patch (CONFIRMED 8.8-29.5×,
  compile-neutral, 44/44). Flagship. Generator inherits it by emitting dfa.find.
- **PT-N2 — TS 1.1.0**: the lazy-diagnostics alloc patch (11, CONFIRMED_DELTA: then +13.6%,
  fused +15.6%, alloc −60%); grows into PT-E1 per-parse-scoped diagnostics (the same fields);
  PT-E2 ExtractValue tightening (R4 enumerates the Parser<any> leaks). Consumer gates:
  value.js, latex-paper (^0.7.1 SKEW — flag the 0.9→1.0 jump), keyframes.js suites.
- **PT-N3 — de-nightly, honestly**: 09's port re-framed post-refutation: STABILITY value
  (stable toolchain, smallbox excised), zero perf claim; the generator Debug fix
  (emit derive(Debug)); the arena as a Parser-ownership tranche-scoped refactor (86 sites,
  lifetime-sound or not at all).
- Letter **BBNF-PT-G-2026-07-16** supersedes the never-delivered BBNF-PT-F: carries PT-N1-3
  with patches + VERIFY.md evidence, the PT-R.5 repoint note re-timed to GN-0/GN-1, and the
  non-clobber acknowledgment. PLACEMENT LAW UNCHANGED: only the owner or a parse-that-side
  session places it.

## §3 THE ADJUDICATION CHECKBOXES (RE-CUT; owner ratify/veto, defaults proceed)

1. **Letter framing**: BBNF-PT-G ships as contingent proposals with patches attached
   (default: yes — evidence-first beats ask-first).
2. **release.yml aarch64-only** in parse-that CI when PT-N3 lands (default: yes, mirrors
   the x86 abrogation).
3. **GN-0 timing**: archival executes immediately post-freeze, before GN-1 (default: yes —
   the owner's "once tranches are formalized" is satisfied by this freeze).

## §4 STANDING LAW (unchanged, restated by pointer)

x86 abrogation (4-layer, both repos); profiling standing law (PMU cyc/B, paired, cold N≥200,
multi-window, per-block loadavg; quiet-host cert at execution); colocation grand edict +
extreme parsimony (§1.5); no legacy/workarounds/stubs; generated files regen-only;
one-regex-home; non-clobber; precept codex sk-v23 research/07; durable orchestration
(CAMPAIGN-STATE pattern).

## §5 THE FORWARD PROMPT (execution dispatch — hand this whole to the executing session)

Execute the SK-V24 codex at restart/skinny/tranches/sk-v24/SPEC.md (FROZEN 2026-07-16),
tranches GN-0→GN-1→{GN-2∥GN-3}→GN-4→GN-5→GN-6, with parse-that proposals riding letter
BBNF-PT-G (placement law: owner/parse-that-side only). The proven patches live in
restart/skinny/tranches/sk-v24/prototypes/PROVE-XX/patch.diff with VERIFY.md certification;
prover worktrees (if extant) at .claude/worktrees/wf_386900ed-462-N. Bench law: cold paired
PMU per the standing law; baselines = SPEC §0 + sk-v22 research/04. Edicts: colocation
grand edict, extreme parsimony, implementation-majority, no legacy, aarch64-only. Durability:
maintain CAMPAIGN-STATE.md per the resurrection protocol with an hourly heartbeat cron.
Owner gates: §3 checkboxes (defaults proceed absent veto) and the letter delivery.
