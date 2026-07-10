# SK-V23 SPEC-DRAFT — The Dual-Repo Tranche Codex

> **SUPERSEDED 2026-07-10 — promoted to `SPEC.md` (FROZEN).** Retained as the draft record;
> the frozen spec is the sole authority.

**PASS-1 SYNTHESIS, 2026-07-09 — AMENDED 2026-07-10 (pass-1 agglomeration):** six prototype
verdicts (PB-1–PB-6) + nine critiques folded in place; per-item movement and residuals in
`CONVERGENCE-LEDGER.md`. **RE-AMENDED 2026-07-10 (pass-2, the closing agglomeration):**
PROTO-PB-3b (sheets CARRY, executed byte-parity), research/09 (the 70-row DEFERRED-LEDGER
re-home table), research/10 (the owner adjudication packet), and the two re-critiques
(p2-architecture 94, p2-honesty-charter 97) folded in place; the pass-1 M1-M5 interlock
moderates closed (closing ledger items 18-22). Charter: `sk-v23/PASS-ALPHA.md` (three binding axes: full x86
abrogation both repos; deep modern SIMD within the ASM-hybrid union; greenfield-from-skinny,
delineated honestly). Evidence base: sk-v23 research/01-08 (this pass), the FROZEN sk-v22 SPEC
(graduation GW0-GW6 + PT-R/PT-T + archive sequence §2.1 + the §5 profiling standing law), the
FROZEN sk-v21 SPEC (the settled adjudications: vshrn obviates hand asm — sk-v21/SPEC.md:44-48;
AoS over SoA; fused sink default — sk-v21/SPEC.md:126-128; canada=Eisel-Lemire —
sk-v21/SPEC.md:124; N\*=3 crossover — sk-v21/SPEC.md:607-609; TOML walls dissolvable —
sk-v21/SPEC.md:637-643). This draft develops sk-v22's wave manifest into NAMED TRANCHES — six
for bbnf-lang (TB1-TB6), five for parse-that (PT-R1-R3, PT-T4-T5) — ratifying research/08's
skeleton with the corrections of research/01 (x86 census), 02 (SIMD frontier), 03 (greenfield
delineation), 04/05 (feature triage), and 06 (coordination protocol). It reopens no frozen
SK-V21/22 architecture decision; where it deepens one (the type plane), it says so and why.

**The four skeleton seams (research/08 §G), adjudicated here:**

1. **GW0.b placement — RATIFIED as the EXECUTE/CLOSURE split.** The physical port-in executes at
   TB1 (the ordering law forces it before the `crates/` removal); TB2 owns the port-in CLOSURE
   (vendor byte-identity CI, sync-script enforcement, stable-channel readiness) — research/08:83-89.
2. **The GW4 split — RATIFIED.** GW4 cleaves across TB4 (TW3 value plane) and TB5 (TW4.2
   decision/kernel plane); bbnf stays at six tranches — research/08:359-361.
3. **The x86-guard mechanism — DECIDED (§2.3 below).** The structural token gate (Lock-class
   falsifier on the `lock14_baseline.rs` allowlist + `skv15_w0.rs` rejects-suite patterns,
   research/01:160-175) is the load-bearing layer; workspace lints, CI arch assertion, and the
   emitter guarantee are defense-in-depth. bbnf plants it in TB1 with a dated transitional
   allowlist, emptied at TB2; parse-that's equivalent is a PT-R1 proposal.
4. **The greenfield-vs-carry table — PROMOTED to normative (§1)** and committed as a durable doc
   alongside TB1's GW0.e battery.

---

## §0 — GOVERNING PREAMBLE: the precept codex (research/07, binding)

The full numbered codex of research/07 §1 (items 1-57) governs both repos' tranche content. The
load-bearing subset, restated as this campaign's operating law:

- **Core discipline (both repos, unconditional):** KISS/DRY; no workarounds/stubs/legacy;
  architectural transposition over patching; abrogate-before-patch; ONE path; no silent
  deferrals; substrate lands with a consumer; wire-before-retire; named waves; goal+completion
  criteria per unit; no god modules; directory modules; tests outside `src/`; docs ride the wave;
  hard gates need artefacts (research/07 items 1-19, `docs/precepts/instructions/README.md`).
- **The Y.13 consumer-invariant gate law (item 37):** every substrate/kernel/value wave closes on
  an exhaustive-match runtime gate — liveness AND a measured win; hygiene/stack waves close on
  build+bench parity; the two named relaxations (TW1b, TW4.1's cost-table leg) are closed; no
  further relaxation without a new-letter re-plan (sk-v21/SPEC.md:66-84, 745-750).
- **The profiling standing law (items 43-49, sk-v22/SPEC.md §5, verbatim):** PMU cycles/byte via
  `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` certified; ins/byte the load-invariant check;
  paired per-sample interleaving, mean + 95% CI over N≥200, decision-grade only when every
  window's CI excludes 1.0 same-way; cold-per-parse only, no figure ever quoted warm;
  ≥3 separated windows; per-block loadavg beside every table; the loadavg<2 gate is retired.
  **Every bbnf no-regression clause binds to the sk-v22 research/04 load-accounted baseline as
  the standing ANCHOR; each tranche's PROOF is its own paired same-session before/after capture
  vs the prior end-state — the anchor never moves, the proof always pairs** (sk-v22/SPEC.md:487-491).
- **Engineering laws (items 50-57):** aarch64/Apple-M only; cold-only single-invocation benches,
  every grammar every wave; one cargo per target dir; generated files are fresh-regen output,
  never hand-patched, `generated_loc_budget` enforced; fail-explicit lowering (no silent
  Epsilon); clean instrumentation.

**Contradiction resolutions (research/07 §2, decided normatively):**

- **C1 (fan-out ceiling: 8 vs 6 vs 3).** RESOLVED: batches of **3** remain the implementation
  default (sk-v22/SPEC.md:536); **6** is the hard write-wave ceiling; read-only audit/research
  lanes may reach **7** under the submodule's carved exception
  (`instructions/ORCHESTRATION.md:11,28-34`). PASS-ALPHA's "up to 8" is reduced to 7-or-fewer
  read-only lanes; no write wave exceeds 6.
- **C2 ("never on a document").** RESOLVED per the Y.13 carve-out: SK-V20's rule binds
  substrate/kernel/value waves; hygiene/doc waves (TB1.W4-class) legitimately close on their own
  named gate class, which may be a plan-required doc artefact (sk-v21/SPEC.md:75-79).
- **C3 (why 3 not 6).** INHERITED explicitly: 3 is the measured integration-quality optimum of
  the SK-V22 campaign; this codex carries it as a considered precept, not an accident.
- **Applicability boundary (research/07 §3):** items 32-42 are bbnf tranche-lineage law; they
  govern what PT-R/PT-T tranches CONTAIN, not how parse-that's own sessions execute them. The
  non-clobber law narrows bbnf-side contact with parse-that to exactly one additive coordination
  letter (§4.0, Appendix A).

---

## §1 — THE GREENFIELD DELINEATION (normative per subsystem; research/03 ratified)

The user's verdict withdraws blanket ratification of skinny: it is the SEED, not the ship. The
four named greenfield subsystems and their SEEDS, plus the carry set, are binding on every
tranche below. The differential-oracle law (research/03, "the consumer-invariant gate, defined
once"): for every GREENFIELD subsystem, the frozen skinny output (`sk-v19-frozen` + the
graduated HEAD) is the reference; the fresh implementation must produce byte-identical
observable output for its consumer across the corpus BEFORE the old code deletes. This table
commits as a durable doc in TB1.W4.

| Subsystem | Verdict | Seeded by | Owner |
|---|---|---|---|
| Generator/emit spine | **GREENFIELD** (~2,000 new / −1,500 fork deleted) | `general_descent/` (1,206 LOC) as the structural sketch; PROTO-B1's wall dissolutions as proofs (WsPolicy, leaf-regex interpreter); the frozen `json_templates` parser as the byte-parity oracle (research/03:61-99) | TB3 |
| Type plane | **GREENFIELD (net-new ~1,000 LOC)** — there is NO type system to carry; `RuntimeTypedProjection.type_text` is an opaque byte-slice (grammar_provider.rs:200-202); grep-zero inference machinery (research/03:181-209) | The `->` annotations in every corpus grammar; the sk-v21 §6 (research 28 §6) design order — HM core over the two open surfaces, structural projection for the rest; the frozen text-threaded projections as the never-lose-a-type oracle | TB4 |
| Value plane / sink | **GREENFIELD (~1,400 new / −780 per-grammar retired — JSON-only: `json/sink.rs` 120 + `json/view.rs` 488 + `json/value.rs` 172; `sheets/view.rs` (279) CARRIES per PB-3b executed byte-parity; the ~16-LOC css_l4 declaration-values sink retires per its own oracle, uncounted in the headline)** | `json/sink.rs` (120) + `json/view.rs` (488) as the proven-cold prototypes; the legacy `bbnf-path` cursor alphabet (918 LOC) REBUILT natively, not ported (sk-v22/SPEC.md:82); PT-T4's `Diagnostic` shape for the error surface (research/03:114-143) | TB4 |
| SIMD dispatch + cost-table + SOTA layer | **GREENFIELD dispatch (~500 new); CARRY the ~1,600 LOC aarch64 primitive bodies** (they ARE the SK-V21 win); DELETE x86 wholesale | `bbnf-simd`'s 15-kernel body set + skinny `movemask.rs` (vshrn already in-tree); the dav1d discipline; research/02's landing table as the uplift manifest (research/03:144-179) | TB5 |
| xtask/regen host | **GREENFIELD-REBUILD (~750 new / −2,500 fork deleted)** — B2's per-grammar fork, same sin as WART-C one layer up | `[workspace.metadata.bbnf.grammars]` + the existing conformance feature; clean-regen gates N1/N6/N25 (research/03:228-246) | TB3 |
| Frontend (grammar+IR) | CARRY + FIX (god-file split, inline tests out) + the NEW `->` type-expression parse seam | — | TB4.W0 (seam), TB2 (hygiene) |
| Runtime tape (AoS) | CARRY — settled evidence, untouched by the four problems | — | TB2 (Lock-1 gate) |
| Decision engine (passes) | CARRY + PRUNE (`decision_csp.rs` retires; lib.rs 2,096-LOC god module splits) | — | TB5.W4 |
| Tests/oracles | CARRY (relocate 75 blocks/34 files) + GREENFIELD the differential-oracle harness (~400 LOC) every greenfield gate depends on | The frozen skinny tree as the reference corpus | TB2 (relocate), TB1.W4 (harness) |
| Bench harness | REWRITE-SHRINK (≈30K generated payload de-committed; x86 strings stripped; protocol preserved VERBATIM) | The sk-v22 §5 standing law | TB2.W2 |

**The one true override, flagged:** the type plane. SK-V22 sized TW5 as a "B3 width patch" —
extend the monotone fold. SK-V23 sizes it as greenfield (research/03:204-209). TB4 carries the
scope increase; the B3 width patch re-lands as a WAVE within it, not as the whole.

---

## §2 — THE x86 ABROGATION (normative; research/01 ratified)

### §2.1 The census, dispositioned

- **bbnf legacy (502 x86 hits): DIE WITH THE ARCHIVE.** `crates/simd-scan/{avx2.rs 370,
  avx512.rs 149, wasm.rs 433, parity.rs x86-CLMUL arm, compaction.rs `_pext_u64` branch,
  lib.rs dispatch}`, the two emit sites (`backend/rust/emitter/shapes/dispatcher/support.rs:541-545,
  595-598, 644-645, 683+`; `generate/regex/emit/simd.rs:133-142`), and the ~310 lines of
  generated AVX2 stamps across eight `grammar/generated/*.rs` files all live in `crates/`, which
  `git rm`s at TB1. Deletion by archival; no transposition (research/01:21-97, research/08:30-33).
- **skinny (the graduated tree): ONE live residue + bench strings.**
  `skinny/crates/passes/src/lib.rs:380` — `avx512bw: cfg!(all(target_arch = "x86_64", …))` in
  `TargetFeatures::host()`, consumed at `admits_collapsed_stage` (~:960) — an x86 capability
  modeled INSIDE greenfield; abrogate field + consumer. Plus x86 target-cpu strings in
  bbnf-bench (`json_parity.rs`, `report.rs`, `gate.rs`, `lock14_baseline.rs`) (research/01:99-113,
  research/03:163-167). Owner: TB2.W2.
- **CI: the silent inversion.** Every bbnf workflow runs x86 `ubuntu-latest`
  (`.github/workflows/ci.yml:12,:95`; `bench-iai.yml:25`) — the NEON kernels are never
  CI-exercised; skinny has NO CI at all. Flip to Apple-Silicon/arm runners + hard `uname -m`
  assertion; create the skinny workflow (research/01:81-97). The `release.yml` LSP x86-target
  matrix is ADJUDICATED as packet Item 3 (`research/10-adjudication-packet.md:107-145`;
  pass 2): the PROCEEDING position drops the three x86 (+ Windows-MSVC) LSP targets, retains
  `aarch64-apple-darwin` + `aarch64-unknown-linux-gnu`, and attaches a NAMED-EXCEPTION clause —
  reinstating x86 LSP targets requires an explicit owner grant recorded against the clause,
  the one sanctioned x86 door, editor-distribution artifact only, never the
  parser/kernel/emit plane. A veto retains the five-wide matrix as an owner-granted axis-1
  exception with a scoped, reasoned allowlist entry for the release-workflow target strings.
  Owner: TB1.W3.
- **parse-that: zero literal x86 intrinsics in the combinator SIMD plane except two arms; the
  real exposure is `portable_simd`.** The three `#![feature]` gates
  (`rust/parse_that/src/lib.rs:1-3`) make the scan kernels auto-vectorize per-target — so
  parse-that's x86 abrogation IS the de-nightly `std::simd → core::arch::aarch64` transposition
  (research/08:15-21). The two literal arms purge with it: `scan/quote_parity.rs:42-48`
  (`_mm_clmulepi64_si128` CLMUL prefix-XOR; the aarch64 PMULL arm at :51-55 stays) and
  `scan/number_simd.rs:138-235` (~100 LOC SSE4.2 digit scan; NEON + scalar stay). Doc-claims
  prune (`decode.rs:168,222`; `scanners.rs:229,309`; `state.rs:51,62`) (research/01:127-141).
  Owner: PT-R1 (proposal; non-clobber).
- **Dependencies:** `memchr` KEEP (arch-aware, NEON-capable); `sonic-rs`/`simd-json`
  competitor-bench deps KEEP (bench-only, arch-neutral source) (research/01:115-123).

### §2.2 The enforcement design (so x86 cannot regrow) — four layers, C.2 load-bearing

1. **C.1 Emitter guarantee + regen-drift gate.** The greenfield TB3 emit spine produces only
   `{aarch64 NEON arm, scalar cfg(not(aarch64)) arm}` — no `target_arch = "x86_64"` token is
   ever EMITTED; the existing `cargo xtask regen --check` drift gate then makes any hand-added
   stamp fail CI mechanically (research/01:148-157). **PB-2 status: PROVEN on the LEGACY
   emitter surface** (regen exit 0; generated-tree x86 tokens 310→0; 891 deletions, ZERO
   insertions — the aarch64 arms byte-identical); the greenfield TB3.W1 spine — C.1's real
   vehicle, ~2,000 LOC that does not yet exist — RE-PROVES the guarantee at its own close
   (emitted stamps token-grepped by C.2): proven-for-legacy, gated-for-greenfield, never
   assumed.
2. **C.2 Structural token gate — THE PRIMARY.** A workspace test walking `git ls-files` over
   src + generated + emitters, grepping the forbidden set (`target_arch = "x86_64"`,
   `core::arch::x86_64`, `_mm`, `__m128/256/512`, `target_feature = "avx…|sse…|pclmul…|bmi2"`,
   `target_arch = "wasm32"`, `is_x86_feature_detected!`), asserting zero hits outside a const
   `ALLOWLIST` — built exactly on the `lock14_baseline.rs` allowlist-table shape and the
   `skv15_w0.rs` `rejects_*` falsification discipline, so the gate is PROVEN to reject a
   smuggled arm before it ever reads green (research/01:159-175). **AMENDED (PB-2 critique):
   the bare `target_arch = "wasm32"` token cannot distinguish a wasm32 SIMD gate from a
   legitimate platform/allocator gate (the live `gorgeous/src/lib.rs:1` `cfg(not(wasm32))`
   mimalloc gate is non-SIMD). TB1.W3 OWNS the final rule: wasm32 tokens forbidden in
   SIMD/kernel/emit surfaces; platform/allocator gates enter the allowlist with a per-entry
   reason string; the gate must FIRE on a planted wasm32 SIMD arm AND stay green on the
   mimalloc gate before reading done.** **VENDOR CLAUSE (pass 2, M4): C.2's walk INCLUDES the
   vendored mirrors (`crates-infra/bbnf-regex`, the csp-solver vendor). Byte-identity and
   abrogation never conflict because the pin is gated: a vendor pin advances ONLY to an
   upstream commit that is x86-clean under the same token set, gate-verified at every sync
   (TB2.W3 sync discipline; the TB3.W2 ↔ PT-R3 pairing); if upstream retains an x86 arm, the
   pin holds at the last clean commit and the retention is ledgered, never silently synced
   past the gate.**
3. **C.3 Workspace lint floor.** `[workspace.lints.rust]` with `unexpected_cfgs` in deny mode +
   a check-cfg allowlist naming only sanctioned cfgs — catches feature-flag regrowth and typos
   C.2's token set misses (research/01:177-186).
4. **C.4 CI arch assertion.** aarch64 runners + a first step that hard-fails on `uname -m`
   ≠ arm64/aarch64 (research/01:188-196).

### §2.3 Ownership + the transitional allowlist (normative)

- **TB1.W3 plants layers C.2/C.3/C.4 in bbnf** with a DATED transitional allowlist naming
  exactly the known skinny residues (`passes/src/lib.rs:380` + the four bbnf-bench string
  sites). The gate must FIRE on a planted x86 intrinsic before reading green (the Lock-14
  planted-phantom discipline, sk-v22/SPEC.md:137-141).
- **TB2.W2 empties the allowlist** — abrogates the residues; post-TB2 the allowlist holds only
  the gate file's self-reference. **STANDING META-INVARIANT (pass 2, M3): from TB2 close
  onward, EVERY tranche close gate re-asserts `ALLOWLIST == {gate self-reference}`; any
  re-growth demands a dated, per-entry reason string plus an owner note — the const cannot
  silently re-grow, which is what "regrowth-proof" (axis 1) means for the gate itself.**
- **TB3 delivers C.1** (the greenfield emitter never emits the token; regenerated stamps are
  aarch64-only).
- **PT-R1.W2 is the parse-that mirror** (deny `portable_simd` + the x86 token set; planted
  regrowth fires) — delivered as a PROPOSAL in the coordination letter, executed on parse-that's
  own cadence.

---

## §3 — bbnf-lang: SIX NAMED TRANCHES

Intra-repo order: **TB1 → TB2 → TB3 → {TB4 ∥ TB5} → TB6.** Anchor: sk-v22 research/04
(load-accounted) for every tranche; per-tranche H1 bindings below. Close-gate classes per
research/08 §E: (a) cutover/pure-move, (b) hygiene/stack, (c) substrate consumer-invariant,
(d) cross-repo paired byte-identity, (e) feature-fold consumer-suite.

### TB1 — THE GREAT ARCHIVAL + X86 PURGE

- **Charter.** Cut the umbrella tag set, port-in the `.bbnf` fleet + egraph/egraph-derive + the
  csp-solver vendor mirror, `git rm` the legacy tree (whose 502 x86 hits die with it), re-found
  root on skinny's workspace, land the six durable docs + the delineation table, and plant the
  x86 anti-regrowth guard — one cutover that must not move a number.
- **Entry deps.** None — campaign root. Internal: tags-before-removals normative
  (sk-v22/SPEC.md §2.1); TB1.W1 BLOCKS TB1.W2; the PT-R.5 repoint confirmation BLOCKS TB1.W2
  (§5 edge 1).
- **Waves.**
  - **TB1.W0 — Tags before removals.** Umbrella `pre-graduation-2026-07-09` + the nine
    navigational tags + `archive/legacy-crates` branch; the rescue checks (threaded-fork skim,
    `data/` provenance, `grammar/` reconcile, no-legacy-cross-reference verify, sk-v22 +
    sk-v23 tranche-dir copy) (sk-v22/SPEC.md:538-556).
  - **TB1.W1 — The port-in (GW0.b).** `.bbnf` fleet → graduated `grammar/`;
    egraph/egraph-derive → `crates-infra/`; csp-solver vendor + `sync-csp-solver-vendor.sh`
    (csc411 checkpoint first); xtask consts repointed. Close verifies BOTH repo repoints
    (parse-that's patch table included) before any removal. **AMENDED (PB-4 critique): the
    fleet port-in EXPLICITLY includes the arrow-bearing grammars — the graduated tree today
    holds one 0-arrow json; every arrow-bearing grammar is archival-legacy — sequenced here so
    TB4.W1's every-arrow close gate is non-vacuous by construction.**
  - **TB1.W2 — Removals + re-found root (GW0.c).** `git rm` `crates/` (the 502 x86 hits die),
    `restart/`, the archived trees; worktree purge; skinny `[workspace]` becomes root.
  - **TB1.W3 — The x86 anti-regrowth guard.** Layers C.2/C.3/C.4 per §2.3; transitional
    allowlist dated; skinny CI created aarch64-only; `release.yml` adjudicated per packet
    Item 3 (§2.1): aarch64-only + the named-exception clause, PROCEEDING absent owner veto.
  - **TB1.W4 — Durable docs + seam records + the oracle harness (GW0.e/f).** The six docs
    (ARCHITECTURE, PERFORMANCE-THESIS, NEGATIVE-RESULTS, BENCH-PROTOCOL, ORACLES,
    EVIDENCE-INDEX) + the §1 delineation table as the seventh; the four dated seam records; the
    GREENFIELD differential-oracle harness (~400 LOC, research/03:248-261) — every TB3/TB4/TB5
    greenfield gate consumes it, so it lands here, build-infra-first. **AMENDED (architecture
    critique B2): W4 also owns the DEFERRED-LEDGER continuity audit — every ledger row
    re-homed to a named TB/PT owner or formally retired under the GW→TB re-lettering, no row
    orphaned; every §1 greenfield cleared against the REDRESS rejected-route registry
    (DEFERRED-LEDGER §0) before its tranche opens.** **RE-AMENDED (pass 2): the audit
    VERIFIES, never discovers — `research/09-ledger-rehome.md` is the pre-drafted NORMATIVE
    table (70 rows walked, full TW→GW→TB/PT coordinate chains, no row orphaned outright; the
    pass-1 "~40" figure corrects to 70 — the lower count was the active-residue subset). W4's
    audit output MUST additionally: (i) fold the N5 dead/lying-oracle + W3C-rekey hygiene
    sweep HERE — this wave is already the oracle-mechanism wave (research/09:146-151);
    (ii) name the two standing deferrals verbatim, "authority unchanged, re-verified" — the
    TS/WASM backends (Lock 5) and the TS path runtime (research/09:152-159); (iii) walk
    CONTENT, not labels — the P7 phantom-fork row's literal TW6 label maps by content to
    TB3.W1 (GW2a); a mechanical TW6→TB6 rename would misfile it (research/09:167-171).**
  - **TB1.W5 — Fuzzer + conformance infra (N5, fold-cheap).** The grammar-derived differential
    fuzzer skeleton on the regen host's conformance feature; fuzz-found divergences enter
    NEGATIVE-RESULTS (research/04:102-117).
- **Close gate (class a).** Pure-move parity — paired capture vs 04 shows no movement; full tag
  set resolves (`git show <tag>:path` sampled per surface); Lock-14 re-keyed and FIRES on a
  planted phantom; the NEW x86 guard FIRES on a planted intrinsic; GW0.e delivered with every
  EVIDENCE-INDEX coordinate a full tag-relative path; GW0.f delivered.
- **Greenfield/carry.** Greenfield: the oracle harness, the guard. Carry: everything moved.
- **H1/baseline.** research/04 ESTABLISHED as campaign anchor. PRECONDITION on GW0.e: the H1
  paired twitter sonic/parse_tape reconciliation adjudicates 2.10× vs 1.59× with the +36%
  sonic-side drift disclosed; 33:59 never cited for a 2.10× claim (sk-v22/SPEC.md:444).

### TB2 — STACK/TOOLCHAIN + PORT-IN CLOSURE + THE BENCH SHRINK

- **Charter.** Pinned-stable edition-2024 everywhere; the 75/34 inline-test relocation; the
  `simd-scan` stub delete; Lock-1 falsified two ways; the bench-harness REWRITE-SHRINK; the
  x86 allowlist emptied; the port-in HARDENED (vendor byte-identity discipline).
- **Entry deps.** TB1 (self-contained tree).
- **Waves.**
  - **TB2.W0 — Edition-2024 pinned stable.** PROTO-A3's five edits applied to the LIVE
    workspace and committed.
  - **TB2.W1 — Inline-test relocation.** 75 blocks / 34 files → `tests/`; grep-zero
    `#[cfg(test)]` under src/ (32 files counted live at research/03:14-16; the 75/34 census of
    sk-v22/SPEC.md:71 governs).
  - **TB2.W2 — The bench shrink + residue abrogation.** ~30K generated bench payload
    de-committed to regen-on-demand; harness logic (~3-5K) survives; x86 strings stripped from
    `json_parity.rs`/`report.rs`/`gate.rs`/`lock14_baseline.rs`; `passes/src/lib.rs:380`
    `avx512bw` field + consumer abrogated; **the x86-guard transitional allowlist emptied to
    self-reference**. The W0.11 protocol is preserved VERBATIM (research/03:263-283).
  - **TB2.W3 — Port-in closure.** Vendor byte-identity CI (`diff -r` at pin) for
    csp-solver/egraph mirrors; sync scripts enforced; stable-channel readiness the TB3.GW2b
    cross-repo engine build asserts against.
  - **TB2.W4 — Hygiene tail + channel ratio.** `simd-scan` stub gone; grep-zero retired names;
    Lock-1 two ways (grep-zero `TapeStructBuilder` + runtime single-owner `frame_depth`
    assert); paired old-pin/new-pin cold capture LEDGERED.
- **Close gate (class b).** Whole-workspace pinned-stable edition-2024 build EXIT 0; zero
  inline tests; bench within the load-accounted band vs 04; allowlist empty; guard green.
- **Greenfield/carry.** Carry-with-fixes throughout; the bench harness is REWRITE-SHRINK, its
  protocol untouched.
- **H1/baseline.** GW1's measured channel ratio RE-ANCHORS the floor for TB3+ (04-adjusted,
  adjustment ledgered); delivers the outstanding DEFERRED-LEDGER channel-ratio row.

### TB3 — THE GREENFIELD GENERATOR + WALLS + ONE-REGEX-HOME

- **Charter.** Dissolve the TOML/WART-C walls into ONE greenfield emit path served by one
  metadata-driven regen host — delete-and-refound, not patch-and-bypass — and fold the regex
  engine to one home (the vendored canonical engine; both fork crates delete).
- **Entry deps.** TB2 (final stable channel — the first wave that legitimately regenerates the
  hot path runs on the final toolchain so its paired capture has ONE variable,
  sk-v22/SPEC.md:114-119).
- **Waves.**
  - **TB3.W0 — The one regen host (greenfield-rebuild).** Metadata-driven generator over
    `[workspace.metadata.bbnf.grammars]` (every fleet member declared); const rosters +
    `real_typed_schema.rs` hand-schema DELETE (−2,500 LOC); gates N1 byte-equality + N6
    `generated_loc_budget` + N25 idempotency enforced; docs-from-grammar emitter mode (Q5,
    budget-counted) + fuzzer conformance wiring (N5) ride here (research/03:228-246,
    research/04:177-186).
  - **TB3.W1 — The greenfield emit spine.** ONE path re-authored (~1,800-2,200 LOC): `WsPolicy`
    derived from `?w`; the bounded leaf-regex interpreter (WALL B); grammar-derived
    projections; the WART-C two-site name-gate (`runtime_generator.rs:98`,
    `json_sink_direct/mod.rs:98`) dissolved structurally. Differential oracle: the emitted JSON
    parser byte-identical on tape output to the frozen `json_templates` parser across the
    corpus — THEN `json_templates/` + `json_sink_direct/` delete (−1,500 LOC). Emitted stamps
    aarch64-only (C.1 delivered) (research/03:61-99).
  - **TB3.W2 — One-regex-home (GW2b, PAIRED with PT-R3).** `crates-infra/bbnf-regex` vendor
    mirror byte-identical to the canonical engine at the pinned upstream commit; skinny's
    `parse-that-regex` fork (3,022 LOC) + local recognizer crate delete post-fold; the engine
    builds on the graduated stable toolchain against `crates-infra/egraph` (gate-verified, not
    assumed — sk-v22/SPEC.md:390-399). **AMENDED (architecture critique B3 — the spine must not
    gate on a non-compellable act): the vendor mirror + the skinny fork-crate delete are
    UNILATERAL bbnf-side acts and proceed regardless; ONLY the recognizer-crate delete depends
    on PT-R3.W1's upstream fold. If the fold is unexecuted at TB3 close, GW2b rescopes to the
    vendor-mirror half, the recognizer carries under a dated CONTINGENCY row (named, never
    silent), and its delete re-owns to the first tranche opening after the fold lands.**
- **Close gate (class c + d).** GW2a: ONE emit path serves every in-tree grammar; `regen
  --check` byte-equality + loc-budget + idempotency; closes on in-tree evidence ALONE; paired
  capture, no cold regression vs the TB2-re-anchored floor, per-corpus. GW2b: vendor `diff -r`
  clean at pin; grep-zero fork; `cargo metadata` zero path escapes; paired capture per-corpus.
- **Greenfield/carry (normative).** GREENFIELD: the emit spine (seeded by `general_descent/` +
  PROTO-B1's proofs + the frozen json_templates oracle) and the regen host (seeded by the
  metadata table + conformance feature). CARRY: the frontend parser, the IR, the tape.
- **H1/baseline.** Paired per-corpus cold capture vs the TB2 floor; no regression.

### TB4 — THE VALUE PLANE + TYPE PLANE GREENFIELDS

- **Charter.** Greenfield the Value API — one grammar-neutral lazy/eager/path/direct-to-struct
  surface replacing the prototype-grade per-grammar sinks — and greenfield the type plane: a
  real type system over the frontend's new `->` type-expression AST, replacing the
  monotone-fold-only status quo. The one SK-V22 scope OVERRIDE lives here (§1).
- **Entry deps.** TB3 (one emit path — the value plane emits through it; inference completes
  before emission). TB4 ∥ TB5 permitted.
- **Waves.**
  - **TB4.W0 — The frontend type seam.** The grammar grows a real `->` type-expression parse
    (additive to the carried parser); `grammar/src/lib.rs` (2,142 LOC god file) splits into a
    directory module (research/03:38-59).
  - **TB4.W1 — The type plane greenfield.** `TypeDesc` (Named/Aggregate/Scalar,
    backend-agnostic per precept); the checker over the type AST per the sk-v21 §6 order (HM
    core over the two open-ended surfaces, structural projection for the rest); the
    typed-materialization invariant as an EXECUTABLE falsifier; the B3 width patch re-lands as
    one wave-item within this, not as the whole (research/03:181-209). ~800-1,200 LOC net-new
    on grammar/ir. **AMENDED (PB-4 + honesty critique B2 + architecture critique B1):**
    (i) the §6 ORDER STANDS as frozen — steps 1-2-3 (incl. host-signature unification and
    Record/Sum) are load-bearing and LAND; steps 4-6 are frontier, gated behind first demand —
    PB-4's land-1/3/5 sequencing is REJECTED as an inversion of sk-v21 research/28:404;
    (ii) the falsifier BROADENS from arrow-count to per-rule TypeDesc/registry equivalence —
    arrow-count (PB-4-proven: 5==5 json, 13==13 easing) stays as the cheap first layer, but
    the aggregate surfaces (`value`→HeterogeneousAltJoin at json.rs:392, `object`/`array`→Vec,
    `pair`→Tuple, the easing TODO aggregates) are gated too, never just scalar arrows;
    (iii) the two HM-core surfaces — PB-4-proven 0-hole/dormant corpus-wide — land declared
    SCAFFOLD-DORMANT with a named first-demand trigger, never sold as live (Y.13 honesty);
    (iv) the orphaned `@host`/closure surface (SK-V22 GW5 + DEFERRED-LEDGER N13) RE-HOMES
    here: land-or-delete adjudicated in-wave with the host-signature falsifier gate.
  - **TB4.W2 — The value plane greenfield.** Neutral `Path`/`Cursor`/`Sink`/lazy-`View` + serde
    bridge + `StructRegistry` projection fence; the bbnf-path cursor alphabet REBUILT natively;
    the fused eager sink stays DEFAULT (sk-v21/SPEC.md:126-128); projection-totality invariant
    executable (`PROJECTION_DIRECT_TO_STRUCT.len() == materializers == consumers`)
    (research/03:114-143). **AMENDED (PB-3 + its critique): the seam is ADOPTED as
    `NodeClass`/`Classify` — one plane-wide neutral enum, each grammar bridging via a
    two-method Classify (project, not flatten; PB-3: compiles over the carried AoS tape, all
    four lanes, lazy-view byte-identical to `json/view.rs`, 602-LOC scratch proof). And sheets
    is DECIDED — CARRY, unconditional (RE-AMENDED pass 2, PROTO-PB-3b executed byte-parity,
    13/13): the sheets tape records ZERO Enter/Leave events — 39 no-op `let _ = (Nu8, &mark)`
    tags, one live `mark`, 12 `match_leaf` sites; only leaves seal a pair — so the structural
    events the pass-1 gate named DO NOT EXIST (PROTO-PB-3b.md:24-49); pure two-method
    `Classify` byte-DIVERGES from the live `rule_kind` navigation (`find(Identifier)`→offset 1
    vs `find_by_class(Scalar(Raw))`→offset 0, `assert_ne!` proven — PROTO-PB-3b.md:81-97);
    parity is recoverable ONLY by a `NativeTag` escape re-importing the 39-ordinal across the
    seam — the flattening the "no enum crosses" invariant disavows, REJECTED, do not build
    (PROTO-PB-3b.md:99-127,159-163). `sheets/view.rs` (279 LOC) CARRIES under a named row; the
    SHEETS differential oracle is DELETED from this wave's close gate; the retirement
    recomputes to −780, JSON-only. The delineation, stated honestly: the neutral surface
    serves keyed/indexed value trees (JSON; CSS declaration values, data-tree-shaped);
    flat-tape rich-`rule_kind` grammars (sheets) keep their native node sweep at full
    fidelity — the `tape/cursor.rs:8-19` disclaimer vindicated, and the live
    `sheets_conformance.rs` consumer stays green unchanged. JSON/CSS retire per their own
    oracles.**
  - **TB4.W3 — The value-plane feature folds.** Q1 derive-macro direct-to-struct made
    grammar-agnostic (no facet runtime reflection); N4 the W3C/MDN-derived typed CSS catalog
    WITH the mandatory `var()`/Unparsed escape hatch — catalog breadth only, never
    minify/browserslist/nesting; Q3 the PT-E1 `Diagnostic`-shape mirror as the value plane's
    error surface (SEAM-PT-E1 inbound) (research/04:84-100, 135-167).
- **Close gate (class c).** Value plane: substrate runtime consumer-invariant — liveness AND
  measured win; fused-sink output byte-identical to the frozen `JsonSink` on the JSON corpus;
  lazy-view cursor-walk identical to `json/view.rs`; **the differential oracle is the code
  being retired, FOR EVERY retired member — the retirement set is JSON (+ the css
  declaration-values sink per its own oracle); sheets is OUT of the set (CARRY, PB-3b), so no
  sheets oracle exists or is ever built; parity proves per-member (PB-3 critique, re-scoped
  pass 2)**. Type plane: per-rule
  TypeDesc/registry equivalence on every corpus grammar (arrow-count the cheap first layer,
  the aggregate surfaces included); the frozen text-threaded projections remain a strict
  subset of what the plane derives — never a superset it loses (PB-4: strictly richer, zero
  lost, the 4 opaque arrows gain U8/Bool/F64); zero `unwrap_or(Span)`.
- **Greenfield/carry (normative).** GREENFIELD: type plane (seed: the `->` corpus + the 28-§6
  order + the frozen projections as oracle), value plane (seed: json sink/view prototypes +
  bbnf-path alphabet rebuilt + PT-T4's Diagnostic shape). CARRY: passes+ir analysis, the AoS
  one-tape, the fused-sink adjudication.
- **H1/baseline.** Paired per-corpus cold capture vs the TB3 end-state; no regression; the
  value-plane win reported per-corpus, never geomean.

### TB5 — THE KERNEL/SIMD PLANE + DECISION ENGINE

- **Charter.** Land Eisel-Lemire float decode + the KernelCostTable to close the +1.48 cyc/B
  canada tax; right-size the decision engine over `passes`; execute the deep NEON modernization
  — the vshrn idiom family, multi-block pipelining, padded tails, bulk unescape, the CSS
  tokenize-once index — into the live kernel plane, greenfielding the dispatch and the
  dead/partial kernels.
- **Entry deps.** TB3 (one-regex-home — `eisel_lemire`'s canonical home is the vendored engine
  post-GW2b; the fix lands wherever the module lives). TB4 optional-parallel, with the SEAM
  OWNER NAMED (pass 2, M1): `materialize_f64` is authored ONCE, by TB4.W2 (the value plane);
  TB5.W0 consumes it, never authors it — restating SK-V21's decode-ordering law (the
  TW4.1-before-TW3-closes rule that once cost a tranche split); TB5.W0's Eisel-Lemire wiring
  lands before TB4.W2 closes, so the value plane's close gate exercises the final decode seam,
  and the two greenfields never author the seam concurrently.
- **Waves.**
  - **TB5.W0 — Eisel-Lemire (GW3).** The headline: canada eager cold BEATS sonic — the
    +1.48 cyc/B float tax localized to `materialize_f64` closed (04:149-159, 222-226).
    Post-GW2b the fix lands upstream as a paired PT companion commit; the vendor re-syncs in
    the SAME wave; the gate is measured bbnf-side (§5 edge 3).
  - **TB5.W1 — The KernelCostTable greenfield dispatch.** The hardcoded 6-fn `PrimitiveKernels`
    struct (`dispatch.rs:51-85`) → a pluggable cost-table (pluggable-components precept); the
    ~15 carried kernel bodies wired-or-retired per wire-before-retire (the dead majority is the
    charter's "dead/partial kernels" verdict); the vqtbl ≤32-byte table bound encoded as a
    cost-model guard, not a change (research/02:136-142, research/03:144-179). **AMENDED
    (pass 2): the N20 revival door binds HERE — the twice-buried structural-bitmap pre-scan
    may return ONLY through this cost table with a measured win; this is the general-purpose
    door, distinct from TB5.W3's CSS-specific index, named so no future session re-discovers
    the architecture a third time (research/09:160-165).**
  - **TB5.W2 — The SOTA NEON uplift.** In the dav1d discipline (intrinsics default, per-kernel
    asm gate, checkasm-liveness; vshrn obviates hand asm — do not re-attempt): the
    vshrn-nibble-domain movemask wherever the vaddv anti-pattern survives in the graduated
    plane (the compaction contract moves to the nibble domain — the correctness trap is
    non-optional); multi-block ILP restructure MEASURE-FIRST (PMU per-block IPC before/after,
    ship only on demonstrated lift); `ld1x4` only alongside it; the padded-tail contract
    ADOPTED from parse-that's `structural_bitmap.rs` pattern (delete scalar epilogues); the
    bulk-unescape HasEsc kernel (P1 — opening gate: a production grammar exercising it under
    `cargo expand`, the twice-exhumed-bitmap lesson); the multi-quote-class scalar bubble
    (legacy `neon.rs:626-675`'s 64-iteration per-bit loop) transposed as a VECTOR algorithm in
    the greenfield plane, never re-authored per-bit (research/02 §§1,3, landing table §6).
  - **TB5.W3 — The CSS tokenize-once structural index (P2, FOLD-AMBITIOUS).** A confirmed-empty
    first-mover lacuna; run as a RESEARCH SUB-WAVE with a falsifier corpus
    (comment-inside-anything, `url()` raw tokens, escapes, nested depth — no solved precedent);
    budgeted risk; a negative result enters NEGATIVE-RESULTS + the index in the measuring
    commit (research/04:189-214).
  - **TB5.W4 — The decision engine right-sized (GW4/TW4.2 half).** `passes/lib.rs` god-module
    split; `decision_csp.rs` RETIRED (decision output identical proves it tautological); the
    four NAMED gates: N22 `node_budget` guard before any escalation; N23 Y.5
    connected-components CSP; the precept-21 `perf_cost` production fix
    (`passes/src/lib.rs:590`); the §5 profile-guided-specialization re-examination trigger.
    P3's PGO hook wires into BENCH-PROTOCOL as an available lever, never a shipped default.
- **Close gate (class c).** Canada eager cold beats sonic (the GW3 headline); each SIMD kernel
  closes on checkasm-liveness + per-kernel measured win, byte-exact parity vs its scalar
  reference; the cost-table never regresses cold throughput vs the frozen hardcoded dispatch;
  every kernel win a paired interleaved cyc/byte ratio, decision-grade only when every window's
  CI excludes 1.0 same-way.
- **Greenfield/carry (normative).** GREENFIELD: the dispatch/cost-table + the SOTA uplift layer
  + the new kernels (unescape, CSS index), seeded by the 15 carried bodies + skinny's vshrn
  movemask + research/02's landing table. CARRY: the aarch64 primitive bodies (the SK-V21 win),
  the N\*=3 crossover adjudication, the recognizer tier (folding upstream at PT-R3).
- **H1/baseline.** Canada-beats-sonic binds to 04:149-159, 222-226; kernel wins bind to the
  TB3/TB4 end-state paired.

### TB6 — FLEET / ORACLES / TOML + CSV + THE FEATURE-FOLD CLOSE

- **Charter.** Re-baseline the grammar fleet from 3 toward ten through the one generator;
  execute the TOML + CSV flips; close the feature-fold ledger; NAME the shopwindow.
- **Entry deps.** TB3 (the one generator) AND TB1.W1 (the grammar-source port-in). Terminal
  bbnf tranche.
- **Waves.**
  - **TB6.W0 — Fleet re-baseline.** Every fleet grammar regenerates through the one host; every
    grammar gains an oracle (zero grammars without one).
  - **TB6.W1 — The TOML flip.** WsPolicy + the leaf-regex interpreter exercised for real
    (newline-significance, RFC3339); the emitted TOML parser round-trips the `toml` crate
    field-for-field (the widened positive Lock-14).
  - **TB6.W2 — The CSV flip.** The KernelCostTable's consumer binding exercised live — the
    single-delimiter flip inside the measured |S|≤2 band (sk-v21/SPEC.md:607-609, 635-646);
    this is where the TW4.1 cost-table liveness relaxation's win-clause finally BINDS.
  - **TB6.W3 — Feature-fold close + the shopwindow NAMED.** Residual folds land or formally
    retire (no silent deferrals); the future greenfield shopwindow wave is NAMED with its
    re-bind manifest — N1 grammar DAP/step-debug + Q2 LSP grammar lints elevated from
    dormant-carry to declared showcase deliverables, with the honesty boundary stated at
    naming: the VM debugs the grammar's interpreted semantics, not the emitted NEON kernel
    (research/04:41-59, 147-157). The shopwindow is a POST-codex tranche, not executed here.
- **Close gate (class e).** Fleet parity across the ten grammars; CSV/TOML consumer-invariant;
  every folded feature carries its born-RED proof + EVIDENCE-INDEX row; the generalization
  census names ten grammars, three serious targets.
- **H1/baseline.** Per-corpus cold capture across the widened fleet vs TB5's end-state.

---

## §4 — parse-that: FIVE NAMED TRANCHES (PT-R\*/PT-T\* — REVIEW-NOT-EXECUTE)

### §4.0 The delivery protocol (binding)

Under the non-clobber law (PASS-ALPHA:36-44), NOTHING in `~/Programming/parse-that` is
modified, staged, or committed by this campaign. The five tranches below are
**bbnf-lang-authored PROPOSALS parse-that pulls on its own cadence**, delivered via ONE
additive coordination letter — `BBNF-PT-F-2026-07-09` at parse-that's
`docs/tranches/A/BBNF-PT-F-2026-07-09.md` (the `VALUEJS-PT-E` placement precedent,
research/06:14-52) — whose full content is Appendix A of THIS spec. The letter is placed by the
owner or a parse-that-side session, never by a bbnf agent. All prototyping runs on scratch
COPIES (§7). Lettering stays on the PT-R\*/PT-T\* family; parse-that's native A/B/Q/S series is
untouched. One TIME-SENSITIVE row exists (PT-R3.W0's patch-table repoint, §5 edge 1); every
other row is review-at-leisure and gates nothing in parse-that's own T/U-adjacent fleet work.

**THE CONTINGENT-DELIVERY CLAUSE (charter critique; OWNER-RATIFY).** Under the non-clobber
law, PASS-ALPHA's axis-1/axis-2 "BOTH repos" reads, for parse-that, as DELIVERED-AS-PROPOSAL:
the codex delivers the proposal artifacts (tranche specs, diffs, evidence coordinates) and
claims those; the x86-abrogation and SIMD OUTCOMES in parse-that are CONTINGENT on adoption at
parse-that's own cadence and are never reported as campaign deliveries. **RE-AMENDED
(pass 2): the ratification is realized as Item 1 of the owner adjudication packet
(`research/10-adjudication-packet.md`) — a veto-able PROCEEDING position: absent a veto the
orchestrator proceeds on the contingent framing as written, and this default-proceed authority
is itself the first thing a veto may withdraw. The one non-absorbable branch is disclosed, not
papered: a full veto (insisting parse-that OUTCOMES be reported as DELIVERED) is irreconcilable
with the non-clobber law, and the orchestrator halts for a charter re-issue rather than either
clobber parse-that or misreport (research/10:59-66).**

Two intra-repo chains: **PT-R1 → PT-R2 → PT-R3** (Rust) ∥ **PT-T4 → PT-T5** (TypeScript).
PT baselines: parse-that's OWN divan scanner/regex benches, cold, load-accounted band — no bbnf
baseline binding except at the paired seams. TS gates: consumer suites + `proof:all`, not perf
ratios.

### PT-R1 — PURE-PRIMITIVES CUT + DE-NIGHTLY TRANSPOSITION (the x86 purge)

- **Charter.** Delete the falsified surfaces, then de-nightly as REDESIGN — `std::simd` →
  `core::arch::aarch64` intrinsics, `smallbox` → BumpSlab arena, the toolchain file truthed —
  which IS parse-that's x86 abrogation; plant the anti-regrowth guard.
- **Entry deps.** None (parse-that root; abrogate-before-patch — never transpose code a later
  wave deletes).
- **Waves.**
  - **PT-R1.W0 — The pure-primitives cut (PT-R.1).** `span_parser/` (~1,150 LOC, falsified),
    demo parsers, the `bootstrap` re-export crate, the warm hand-timed CLI, the WDM/LR tier per
    SEAM-NOGO — all delete.
  - **PT-R1.W1 — De-nightly as redesign (PT-R.2) = the x86 purge of the scan files.** The four
    nightly gates (three `#![feature]` at `lib.rs:1-3` + `smallbox[nightly]`) retire;
    `scan/{structural_bitmap,quoted_simd,decode,number_simd}.rs` transpose from `std::simd` to
    explicit NEON + scalar; the two literal x86 arms purge (`quote_parity.rs:42-48` CLMUL —
    PMULL arm stays; `number_simd.rs:138-235` SSE4.2 — NEON/scalar stay); doc-claims prune
    (research/01:127-141). Per-kernel delineation decided HERE: each formerly-portable scanner
    either stays LLVM-lowered (no liveness binding) or graduates to hand-NEON with vshrn + a
    checkasm binding (research/02:57-63) — the largest delineation surface the SIMD axis
    exposes.
  - **PT-R1.W2 — The anti-regrowth guard.** Deny `portable_simd` + the §2.2 token set; the
    guard FIRES on planted regrowth; CI runner arch asserted aarch64.
- **Close gate (class b).** `combinator_test.rs` + regex tests green; workspace members =
  `parse_that` + `regex` ONLY; `cargo +stable build` EXIT 0; `cargo tree -e features` zero
  nightly; scanner bench parity within the load-accounted band; `grep -c bbnf-lang
  rust-toolchain.toml == 0` or file absent; guard fires on a plant. **Backbone-consumer
  clause:** none can red — the backbones consume the TS package only (sk-v22/SPEC.md:273-276);
  recorded, not waived.

### PT-R2 — REGEX ENGINE SOTA + SIMD PREFILTER/DFA MODERNIZATION

- **Charter.** The hot-loop transpositions, the lazy-DFA degrade, the Hyperscan-lineage
  prefilter tiers wired into the search (not merely detected), the SIMD mantissa path, and the
  1.0 `Regex` façade.
- **Entry deps.** PT-R1 (aarch64 intrinsics are the substrate; the surface was first shrunk).
- **Waves.**
  - **PT-R2.W0 — Hot-loop transpositions (PT-R.3).** `is_accept` folds into the transition
    table (tagged StateId / is_match bitset); `padded_bytes()` read in the `find_at` hot loop.
  - **PT-R2.W1 — The lazy-DFA degrade (PTR-F3 SUPERSEDES the plain NFA walk).** The 4096-state
    bail (`automata/dfa.rs:24,36`) degrades to a hybrid lazy DFA — states built on demand and
    cached, reusing the eager subset-construction builder — never `panic!`, never a slow
    interpreter (research/05:58-67; this corrects sk-v22/SPEC.md:331-334's NFA-walk framing).
  - **PT-R2.W2 — Accel consumption wiring (the orphaned shufti), FUSED-TRIGGER GATE
    MANDATORY.** `detect_accel`'s `Memchr1/2/3`/`NibbleLut` strategies
    (`automata/accel.rs:23-131`) are detected and NEVER consumed — grep-negative in `dfa.rs`.
    Wire `StateAccel` into the DFA search inner loop so a self-loop state's strategy drives a
    skip-to-next-exit-byte (research/02:66-93). **AMENDED (PB-1 + honesty critique B1): the
    UNGATED form is REFUTED — it regresses dense/short-token inputs 1.1-1.9×; the
    fused-trigger gate (run-length trigger, GATE=16 proven) is a REQUIRED design element,
    recovering dense to 0.95-1.15× while retaining 18-63× skip-heavy, with exact
    match-position parity. The win claim carries a RUN-LENGTH CAVEAT: 4-10× (PB-1 measured
    18-67×) is a skip-heavy-regime figure; common short tokens — whitespace runs, idents,
    short keys — live in the dense ~1× regime, so the win is quoted per-regime, never blended.
    Close: the gate-cost claim re-measured certification-grade under the §0 standing law
    (PB-1's 9-round loadavg-7-14 deltas are directional only).**
  - **PT-R2.W3 — The truffle tier.** `AccelStrategy::Truffle` for the >8-byte exit sets shufti
    refuses (the `.take(8)` cap at `accel.rs:124`) — two `vqtbl` set-membership lookups,
    consumed by the same W2 wiring (research/02:95-107). **PB-1: PROVEN net-new coverage —
    truffle activates on whitespace+identifier exit sets where the ≤8 shufti path returned
    None; consumed through W2's gated wiring, same parity discipline.** **Teddy stays SCHEDULED-BEHIND-A-
    CORPUS** (speculative until an alternation-of-literals corpus exists); **sheng stays
    research-tier only**, gated behind proof W2/W3 saturated the skip wins (research/02:109-131).
  - **PT-R2.W4 — SIMD mantissa + vshrn (the number path).** `count_leading_digits_neon`'s
    vaddv anti-pattern → the vshrn movemask (`number_simd.rs:31-61`); `accumulate_digits_neon`
    (SWAR today) → the Mula/Lemire in-vector 8/16-digit multiply-accumulate feeding
    Eisel-Lemire — the highest materialization-leverage item in the whole SIMD axis
    (research/02:210-231). **AMENDED (PB-6 GO + critique): ADOPTED as ONE branchless kernel
    feeding Eisel-Lemire unchanged — exact mantissa parity over 3.8M random runs + full
    canada, 53.8% cyc/B win on real canada (69.6% on fractional-mantissa runs), the nibble
    trap (`tzcount >> 2`) handled. The W4 close gate SPECIFIES a COLD full-parse canada cyc/B
    capture under the standing law — PB-6's warm inner=40 kernel figure is never inherited or
    quoted (cold-only law).**
  - **PT-R2.W5 — The 1.0 `Regex` façade (PTR-F2).** `Regex::new/is_match/find/find_iter` +
    explicit `find_at`/`find_anchored`; the unanchored path powered by the EXISTING
    literal-prefix/first-char prefilters (research/05:44-56).
- **Close gate (class c).** Match-position parity across `regex/tests`; paired cyc/byte delta
  under load accounting; prefilter liveness (checkasm-class, dav1d gate); mantissa exact-value
  parity vs the scalar reference across random digit runs; the SOTA tiers confirmed-present;
  lazy-DFA replaces the panic on a pathological falsifier pattern. Backbone clause: none can
  red (Rust-internal); recorded.

### PT-R3 — FOLD-BACK + SEAM HARDENING (highest cross-repo consequence)

- **Charter.** The bidirectional fold-back, the resolution-graph relocation, the hazard
  closures, and the self-hosted regex.bbnf made a LIVE gate.
- **Entry deps.** PT-R2 (the SOTA engine is the fold target) AND bbnf TB1.W1 (the
  `crates-infra/egraph` re-home target must EXIST before the repoint). **DECLARATION
  CORRECTED (pass 2, M2): PT-R3.W0 is a DETACHED, time-clamped wave — its sole entry dep is
  TB1.W1; it is exempt from the PT-R2 gate and must be actionable at TB1 opening (the dated
  ask rides the letter then, per §5 edge 1). The PT-R2 entry dep binds W1-W3 only.**
- **Waves.**
  - **PT-R3.W0 — The patch-table repoint (PT-R.5) — TIME-SENSITIVE.** The gitignored
    8-coordinate `.cargo/config.toml` patch web → `crates-infra/egraph[-derive]` paths; the
    dead `bbnf = crates/core` line deletes. **Temporally clamped inside bbnf TB1's window:
    GW0.b → PT-R.5 → GW0.c** (§5 edge 1). The letter carries the exact old→new path diff.
  - **PT-R3.W1 — The bidirectional fold-back (PT-R.4, PAIRED with bbnf TB3.W2).** Skinny's
    322-LOC recognizer tier upstream as the engine's fast front; `StringMode`/`StringFlags`,
    the Höhrmann UTF-8 decoder, the {ws,quoted,number} family scanners absorb engine-owned (NO
    parse-that dependency on `bbnf-simd`); the 660-LOC `eisel_lemire` duplication collapses to
    ONE engine-homed module.
  - **PT-R3.W2 — Hazard closure (PT-R.6).** `Box::leak(format!…)` diagnostic labels → a
    `&'static` interner (closing PTR-F6's leak half regardless of the combinator crate's
    fate); `parse_padded(&PaddedView)` entry amortizing the per-parse aligned copy.
  - **PT-R3.W3 — regex.bbnf as a live gate (PTX-F1).** Every pattern in the engine's HIR-parser
    test corpus must ALSO parse under the self-hosted `parse_generated`, and vice versa — a
    differential CI gate catching dialect drift; the grammar becomes a maintained truth
    artifact (research/05:169-181).
- **Close gate (class d).** Number/decode/quote-parity suites green in BOTH repos; DFA spans
  identical to the family scanners on {ws,quoted,number}; grep-zero forked modules in the
  graduated tree; vendor byte-identical at the pin; clean `cargo metadata`, zero archived-path
  escapes; diagnostics-feature tests + load-accounted copy-cost profile; the regex.bbnf
  differential gate green. Backbone clause: none can red; recorded.

### PT-T4 — V8 SHAPE / DIAGNOSTICS / INFERENCE (the TS uplift)

- **Charter.** Bench infra first, then inference tightening, diagnostics-as-product, V8
  hidden-class normalization, and the grammar trace — every item additive MINOR toward 1.1.0.
- **Entry deps.** None — the TS track is independent of the PT-R chain and of bbnf.
- **Waves.**
  - **PT-T4.W0 — Bench-harness modernization (PTT-F2, build-infra-first, lands FIRST).** The
    raw comparison scripts (`arcsecond/nearley/parsimmon/ohm/peggy.ts`) consolidate to vitest
    `bench()`, cold-only, one harness (research/05:127-135).
  - **PT-T4.W1 — Inference tightening (PT-T.1 = PT-E2).** `Parser<any>` → `readonly
    Parser<unknown>[]`; `const` type params; `NoInfer`; internal erasure stays.
  - **PT-T4.W2 — Diagnostics as PRODUCT (PT-T.2 + PTT-F1) + the shared schema (PTX-F2).**
    `parseWithDiagnostics()` via the PACKRAT_ARMED latch (one pattern, two tiers); deduped/
    ranked expected-set, span, `renderDiagnostic(src)` caret+ANSI renderer on the extant
    `ansi.ts`; `suggestions`/`secondarySpans` lazily allocated. The `{offset, furthest,
    expected, found, span}` schema FREEZES here — the shape bbnf TB4.W3 copies (SEAM-PT-E1
    outbound) and the worker-safe/structuredClone constraint (PTT-F3) binds to.
  - **PT-T4.W3 — V8 hidden-class normalization (PT-T.3).** Uniform 3-field `Parser.context`
    (kill the shape-splitting `{}` default); retire `mapState` (the `Object.create`
    dictionary-mode hazard). **AMENDED (PB-5 GO + critique): same-map true, zero
    dictionary-mode, floor held. The close RESTATES the bench clause as
    no-regression-beyond-noise, RE-MEASURED on a quiet host (PB-5 ran at loadavg 15-26 with
    ±1.4% noise exceeding the effect — delta≥0 was unprovable there); plus a raw-parser
    debug/trace parity assertion covering the `.context` introspection surface
    (debug.ts:261/365) PB-5 never exercised.**
  - **PT-T4.W4 — Grammar trace (PTT-F4).** Opt-in `trace()` emitting nested spans (dispatch arm
    taken, packrat hit/miss, furthest-offset advance) on the same scoped-latch discipline.
- **Close gate (class e — the backbone-consumer gate, in full).** `.d.ts` inference assertion
  (`any(string("a"),string("b")): Parser<string>`); **cross-repo typecheck of value.js +
  latex-paper against the candidate `.d.ts` green (the semver tripwire)**; born-RED
  `diagnostics-scoped.test.ts` (non-empty `expected` reaches the caller; interleaved scopes
  don't cross-contaminate; retained-heap flat); shape-stability clause (`%HaveSameMap` probe or
  own-keys-order check); json-regression floor holds; `npm run proof:all` green; **the three
  backbone suites — value.js, latex-paper, keyframes-transitive — run against the candidate
  before any publish.**

### PT-T5 — THE 1.1.0 CUT + LEGACY SWEEP

- **Charter.** Retire the unconsumed state tier, delete the live-falsehood doc, carry the
  dispatch lever only-if-measured, guarantee the worker-safe shape, cut 1.1.0.
- **Entry deps.** PT-T4.
- **Waves.**
  - **PT-T5.W0 — Legacy/state-tier sweep (PT-T.4).** `save`/`restore`/`from`/`getLineNumber`/
    `getColumnNumber` retire; `getLazyParser`/`lazy` assessed; `PARSER_STRINGS` bounded; the
    stale `span-combinators.md` (teaching an excised API) deletes; `proof:no-span-surface`
    extended over docs/.
  - **PT-T5.W1 — Dispatch lever, carry-iff-measured (PT-T.5).** 2-arm/≤4-arm dispatch forms
    ONLY if profiled dominant on value.js's real CSS shape; the runtime tagged-switch is
    PERMANENTLY falsified (−10-14% on V8) and shall not be re-attempted.
  - **PT-T5.W2 — Worker-safe shape + the cut (PTT-F3).** The Diagnostic + AST carry no
    closures/`Parser` refs (structuredClone-safe, a discipline gate on W2's schema) + the
    documented `parseInWorker` pattern; 1.1.0 released.
- **Close gate (class e).** `dist == source` barrel; born-RED `proof:no-legacy-state-surface`;
  `proof:all` green; **the three backbone suites against the 1.1.0 candidate tarball BEFORE
  publish**; `proof:perf` real-corpus clause or DO-NOT-CARRY for the lever; published edges
  (`@mkbabb/parse-that`, `@mkbabb/bbnf-lang ^0.1.1`) re-version, never mutate (SEAM-LATEX-PAPER).

---

## §5 — THE INTERLOCK GRAPH

Intra-repo:

```
bbnf:        TB1 ──► TB2 ──► TB3 ──►┬─► TB4 ─┐
                                    └─► TB5 ─┴─► TB6
parse-that:  PT-R1 ──► PT-R2 ──► PT-R3          (Rust chain, proposals)
             PT-T4 ──► PT-T5                    (TS chain, proposals, ∥ Rust)
```

**The archive sequence IS tranche one's opening** — TB1.W0/W1/W2 execute sk-v22 §2.1 steps 1-5
verbatim (tags → rescue checks → port-in → removals → re-found), order normative.

Cross-repo edges (the named laws):

1. **THE ORDERING LAW (highest consequence).** `TB1.W1 (GW0.b port-in)` → `PT-R3.W0 (PT-R.5
   patch-table repoint)` → `TB1.W2 (GW0.c crates/ removal)`. A mis-order bricks BOTH trees'
   builds at once (SEAM-EGRAPH, sk-v22/SPEC.md:245-253). Under the non-clobber law the repoint
   is executed by the OWNER or a parse-that-side session — the letter's one TIME-SENSITIVE row
   carries the exact path diff — and **TB1.W2 does not proceed until the repoint is confirmed**.
   The letter text is frozen in Appendix A NOW so the ask reaches the owner at TB1 opening,
   regardless of when the file is physically placed in parse-that's tree. **AMENDED (charter
   critique): the TIME-SENSITIVE row is DELIVERED at TB1 OPENING — correcting PASS-ALPHA's
   at-close timing for this one row; the full letter still places at close. CONFIRMATION is a
   defined artifact: owner acknowledgement + a read-only grep of parse-that's gitignored
   `rust/.cargo/config.toml` showing `crates-infra` paths (reading violates nothing), ledgered
   in the TB1.W2 opening commit. FALLBACK (bounded — no external SLA holds the critical path):
   if unconfirmed at TB1.W2 open, every removal proceeds EXCEPT `crates/egraph[-derive]`,
   retained under a dated NEIGHBOR-GRACE row — the campaign's only sanctioned duplicate —
   deleted the instant confirmation lands.** **RE-AMENDED (pass 2): the duplicate's sanction
   is packaged as adjudication packet Item 2 (research/10:70-104) — a veto-able PROCEEDING
   position, PROCEEDING absent a veto: one named, dated, bounded, self-deleting row. A veto
   here evaporates the Item-1 fallback and collapses to block-or-brick (TB1.W2 blocks on the
   external repoint, or parse-that's local build breaks); it is weighed as such.**
2. **THE FOLD-BACK PAIRING.** `PT-R3.W1 (PT-R.4)` ↔ `TB3.W2 (GW2b)` — the two halves land
   against each other's parity suites; the vendor mirror byte-identical at the pinned upstream
   commit; {ws,quoted,number} family-scanner parity is the shared gate.
3. **THE FLOAT-TAX COMPANION.** `TB5.W0 (GW3)` ↔ a paired PT companion commit into the
   canonical `eisel_lemire` module; the vendor re-syncs in the SAME wave; the gate is always
   measured bbnf-side.
4. **THE DIAGNOSTICS SEAM.** `PT-T4.W2` freezes the `Diagnostic` schema → `TB4.W3` copies it
   when the greenfield Value API grows its error surface (SEAM-PT-E1).
5. **THE ENGINE-VENDOR DEPENDENCY.** `TB3.W2` gate-verifies "the canonical engine builds on the
   graduated stable toolchain against `crates-infra/egraph`" — requiring PT-R2's engine surface
   stable and PT-R1's de-nightly (gate-verified: the engine crate carries no `#![feature]`; all
   four nightly gates localize to the combinator crate — sk-v22/SPEC.md:390-399; if the engine
   proved nightly-bound, GW2b fails and PT-R1.W1's scope extends).

Independent of bbnf entirely: PT-R2 (self-gated), PT-T4, PT-T5; PT-R1 except as edge-5's
precondition.

**DECLARED SCHEDULE RISK (pass 2, M5).** TB4 and TB5 are the two heaviest tranches — two
greenfields apiece plus the kernel/decision planes. Their `∥` is a permission, not a demand:
if the 6-lane write ceiling binds, TB4 precedes TB5 serially (the M1 seam order is already
compatible) rather than thinning either tranche's gates. The risk is named here so overload
surfaces as a sequencing decision, never as gate erosion.

**H1/baseline bindings, one line each:** bbnf — research/04 is the standing anchor for all six
tranches; TB1 gates GW0.e on the H1 twitter reconciliation (2.10× vs 1.59×, +36% drift
disclosed); TB2's channel ratio re-anchors TB3+; TB3/TB4 pair vs the re-anchored floor; TB5
binds canada-beats-sonic to 04:149-159/222-226; TB6 captures across the widened fleet vs TB5.
parse-that — its OWN divan benches, cold, load-accounted; bbnf-bound only at the paired seams
(edge 2 family-scanner parity, edge 3 canada); TS tranches gate on consumer suites +
`proof:all`, never perf ratios.

---

## §6 — THE FEATURE-FOLD TRIAGE (research/04 + 05, normative homes)

### Folded — bbnf

| Item | Home | Note |
|---|---|---|
| N5 grammar-derived differential fuzzer | TB1.W5 (skeleton) + TB3.W0 (wiring) | build-infra-first; fuzz divergences → NEGATIVE-RESULTS |
| N6 build-time gates (byte-eq/budget/idempotency) | TB3.W0 | harden, no new feature |
| Q5 docs-from-grammar | TB3.W0 | emitter mode on the one host; counts against loc-budget |
| Q1 derive direct-to-struct | TB4.W3 | grammar-agnostic only post-WART-C fold; no facet reflection |
| N4 W3C/MDN typed CSS catalog | TB4.W3 | MANDATORY `var()`/Unparsed hatch; never minify/browserslist/nesting |
| Q3 diagnostics mirror (PT-E1) | TB4.W3 | copies PT-T4.W2's frozen schema |
| P1 bulk unescape kernel | TB5.W2 | opening gate: production grammar exercises it under `cargo expand` |
| P2 CSS tokenize-once index | TB5.W3 | FOLD-AMBITIOUS, research sub-wave, falsifier corpus |
| P3 PGO hooks | TB5.W4 | bench lever only, never a shipped default |

### Folded — parse-that (as proposals)

| Item | Home | Note |
|---|---|---|
| PTR-F2 1.0 `Regex` façade | PT-R2.W5 | start prefilter powers unanchored `find` |
| PTR-F3 lazy-DFA degrade | PT-R2.W1 | SUPERSEDES the plain NFA-walk framing |
| PTR-F4 prefilter tiers (shufti/truffle) | PT-R2.W2-W3 | Teddy scheduled-behind-a-corpus |
| PTX-F1 regex.bbnf live gate | PT-R3.W3 | the fold's own parity corpus |
| PTT-F2 bench harness | PT-T4.W0 | lands FIRST (build-infra-first) |
| PTT-F1 diagnostics product + PTX-F2 shared schema | PT-T4.W2 | the schema bbnf copies |
| PTT-F4 grammar trace | PT-T4.W4 | scoped latch |
| PTT-F3 worker-safe shape | PT-T5.W2 | a discipline gate + doc |

### Elevated to the NAMED shopwindow (post-codex, not SK-V23 execution)

N1 grammar DAP/step-debug + Q2 LSP grammar lints — TB6.W3 converts them from silent
dormant-carry to a NAMED showcase deliverable with the honesty boundary stated (debugs the
grammar semantics, not the emitted kernel) (research/04:41-59, 147-157).

### NOT-NOW (with reasons; each revisitable behind its named precondition)

| Item | Reason |
|---|---|
| N2 tree-sitter-class error recovery | XL redesign; fights the settled fail-fast-is-fast thesis; the cheap subset IS Q3 |
| N3 streaming/incremental parse | zero surviving consumer post-playground-archival; value plane must land first |
| Q4 cargo-generate templates | no stable public API/user until post-TB6 |
| P4 no_std/embedded | no consumer; forks the singular-arena adjudication |
| PTR-F5 capture slots on one-pass DFA | product-shaped; bbnf's DFA-only surface never needs it |
| PTT-F5 streaming/chunked TS input | recorded 1.0.0 non-goal; state-thread rewrite |
| PTX-F3 doc-gen from combinator types | low leverage; rides whichever tranche freezes the types |
| PTR-F1 arena-first combinator API + PTR-F6 error parity | open-question-A is CLOSED by sk-v22 (the graduated tree grows NO combinator surface — sk-v22/SPEC.md:273-282); the interner-leak fix alone rides PT-R3.W2 |
| PT-R2 Teddy / sheng | behind-a-corpus / research-tier only (research/02:109-131) |

---

## §7 — PROTOTYPE BRIEFS (the riskiest new claims, falsifiable)

**PASS-1 RESULTS (2026-07-10):** PB-1 GO-gated (fused trigger mandatory, run-length caveat);
PB-2 PROVEN on the legacy surface (greenfield re-proof gated at TB3.W1; the wasm32 token rule
wave-owned at TB1.W3; its test failures were missing gitignored corpus fixtures, not parity
regressions); PB-3 FEASIBLE for the JSON lanes (`NodeClass`/`Classify` adopted; the sheets
seam OPEN — pass-2 focus); PB-4 PASS arrow-scoped (falsifier broadened, §6 order restored,
corpus migration sequenced at TB1.W1); PB-5 GO load-caveated; PB-6 GO adopted (cold close
clause added). Verdicts folded into §§2-4 above; movement ledger in `CONVERGENCE-LEDGER.md`.

**PASS-2 RESULTS (2026-07-10, the closing pass):** PROTO-PB-3b executed (scratch crate,
471 LOC, 13/13 sheets-lane parity checks) — **SHEETS = CARRY, unconditional**: zero
Enter/Leave events on the sheets tape, pure `Classify` byte-divergent from the `rule_kind`
navigation, the `NativeTag` escape rejected as re-imported flattening; the −1,059 recomputes
to −780 JSON-only (folded at §1 + TB4.W2 + the TB4 close gate). research/09 pre-enumerates
the 70-row DEFERRED-LEDGER re-home with four flags — N5, the Lock-5 TS/WASM pair, the N20
door, the P7 label caution (folded at TB1.W4 + TB5.W1). research/10 packages the three owner
adjudications as veto-able PROCEEDING positions (folded at §2.1, §4.0, §5 edge 1). The two
re-critiques (p2-architecture 94, p2-honesty-charter 97) return ZERO open design questions;
their determinate folds — R1/R2/R3 (M1-M5) and the synthesis lag — are the amendments of this
revision.

All prototypes are worktree/scratch-isolated analysis instruments, never landed (precept 50).
parse-that briefs run on scratch COPIES under the non-clobber law. Each fits one 40-min agent.

### PB-1 — NEON shufti/truffle prefilter spike (parse-that DFA, scratch copy)
- **Question.** Does wiring `detect_accel`'s orphaned `Memchr*`/`NibbleLut` strategies (+ a
  truffle tier) into the DFA search inner loop yield ≥2× cold on skip-heavy inputs with full
  match-position parity — i.e., does a prefilter tier PAY on real grammar-derived patterns?
- **Method.** Copy `rust/regex/` to the scratchpad; consume `StateAccel` in `dfa.rs`'s find
  loop; add `AccelStrategy::Truffle`; run `regex/tests` for parity; paired-interleaved cold
  ns/byte on a `[^"\\]*`-dominated corpus AND a dense-match corpus vs the unmodified copy.
- **Success.** Parity green; ≥2× on skip-heavy; no regression on dense-match. A miss records
  the negative with the per-state strategy histogram.
- **Repo.** parse-that (scratch copy).

### PB-2 — The x86-purge build probe (bbnf worktree)
- **Question.** With `avx2.rs`/`avx512.rs`/`wasm.rs` deleted, the parity/compaction x86 arms
  stripped, the two emit sites' x86 arms dropped, and the stamps regenerated, does the whole
  workspace build+test+smoke-bench green aarch64-only with grep-zero x86 outside the §2.2
  allowlist?
- **Method.** Worktree off master; execute research/01 A.1+A.2 dispositions; `cargo xtask
  regen`; build + test; run the C.2 token grep as a script; one cold smoke bench per corpus.
- **Success.** Build green; regen clean (no x86 stamp survives); token grep zero outside
  allowlist; smoke bench within the load-accounted band. Any hidden x86 coupling surfaces as a
  named finding.
- **Repo.** bbnf-lang (worktree).

### PB-3 — The greenfield value-plane API sketch
- **Question.** Can ONE grammar-neutral surface — eager fused `Sink` + lazy `View` cursor +
  `Path` alphabet + serde direct-to-struct — be expressed as COMPILING Rust signatures the
  existing AoS tape satisfies, with no per-grammar enum vocabulary?
- **Method.** New scratch crate over skinny's `TapeRec` shape; author the trait surface
  (~300-500 LOC) + ONE worked JSON example that walks a twitter-fragment tape through all four
  lanes.
- **Success.** Compiles; the JSON example materializes byte-identical to `json/view.rs` on a
  fixture; the sheets header's "no shared structural vocabulary" disclaimer is demonstrably
  answered by the neutral vocabulary.
- **Repo.** bbnf-lang (scratch crate).

### PB-4 — The type-plane greenfield skeleton
- **Question.** Does a `TypeDesc` (Named/Aggregate/Scalar) checker over a REAL `->`
  type-expression AST compile as scaffolding in the sk-v21 §6 order, and does the
  arrow-count invariant hold on json.bbnf (every `->` reaches a typed projection; the frozen
  text-threaded projections a strict subset, never a loss)?
- **Method.** Scratch crate consuming the grammar parse; minimal type-expression parser; fold
  `TypeDesc` over json.bbnf + one second grammar; count arrows vs derived projections;
  diff against the frozen `RuntimeTypedProjection` set.
- **Success.** Compiles; arrow-count == projection-count on json.bbnf; zero frozen projections
  lost; the two open-ended surfaces named with their HM-core seam identified.
- **Repo.** bbnf-lang (scratch crate).

### PB-5 — V8 shape-normalization microbench (TS dispatch combinators, scratch copy)
- **Question.** Does the uniform 3-field `Parser.context` + `mapState` retirement keep every
  dispatch-combinator site monomorphic (`%HaveSameMap` true, no dictionary-mode objects) and
  hold-or-improve the json floor?
- **Method.** Copy `typescript/src/parse/` to the scratchpad; apply the normalization; `node
  --allow-natives-syntax` probes (`%HaveSameMap`, `%GetOptimizationStatus`) over constructed
  parsers; paired cold json bench both copies.
- **Success.** Same-map true across constructed parsers; zero dictionary-mode; bench delta ≥0.
  A regression records the negative before PT-T4.W3 ships the proposal.
- **Repo.** parse-that (scratch copy).

### PB-6 — SIMD mantissa accumulation probe (parse-that number path, scratch copy)
- **Question.** Does the Mula/Lemire in-vector digit accumulate (vsub '0' +
  positional-weight `vmull`/`vpadd`) plus the vshrn digit-count beat the SWAR
  `parse_eight_digits` path with EXACT mantissa parity on coordinate-heavy input — the
  claimed highest materialization-leverage win?
- **Method.** Copy `scan/number_simd.rs` + `number_f64.rs` to the scratchpad; implement the
  NEON accumulate; property-test mantissa parity across random digit runs (≤19 digits); paired
  cold cyc/B on a canada-class corpus vs the unmodified copy.
- **Success.** Exact parity on all runs; ≥10% cyc/B win on the numeric corpus; the
  nibble-domain movemask trap (tzcount >> 2) demonstrated handled.
- **Repo.** parse-that (scratch copy).

---

## APPENDIX A — THE COORDINATION LETTER (content frozen; placement per §4.0)

**Target path (parse-that):** `docs/tranches/A/BBNF-PT-F-2026-07-09.md` — the
`VALUEJS-PT-E-2026-07-05.md` precedent: flat in the latest (only) tranche dir, with the
re-home-me invitation (research/06:14-33). ID prefix `BBNF-PT-` is disjoint from `VALUEJS-PT-`
and `KF-TO-PARSETHAT-`; `F` is cosmetic continuity, not a shared counter (research/06:37-52).
Placed by the owner or a parse-that-side session; never written by a bbnf agent.

> # BBNF-PT-F-2026-07-09 — coordination letter, bbnf-lang → parse-that
>
> First letter of this lineage (`BBNF-PT-*`). Placement follows the constellation pattern
> (an incoming letter in the recipient's latest tranche dir); re-home it if you open a
> coordination inbox. **Nothing here gates any parse-that wave. This is a proposal-and-notice
> letter, folded on parse-that's own cadence, not a dispatch** — with ONE dated exception,
> flagged TIME-SENSITIVE below, that protects YOUR local build.
>
> ## Standing context
>
> bbnf-lang has zero committed edits in this repo and will make none. The coupling is
> presently one-directional and invisible to you: your gitignored
> `rust/.cargo/config.toml` patches three bbnf-lang crates (`crates/core`, `crates/ir`,
> `crates/ser`) plus `crates/egraph`/`crates/egraph-derive` directly into your local Rust
> build — the file's own comment records it already broke once when bbnf-lang B2 retired a
> crate path. bbnf-lang is now executing its graduation: the legacy `crates/` tree archives
> behind tags and the workspace re-founds; `egraph`/`egraph-derive` re-home to
> `crates-infra/egraph[-derive]` in the graduated tree.
>
> ## Asks and offers
>
> | Priority | Row |
> |---|---|
> | **HIGH — TIME-SENSITIVE** | **Patch-table repoint (PT-R.5).** Before bbnf-lang removes `crates/`, your `rust/.cargo/config.toml` entries `egraph`/`egraph-derive` must repoint from `../../bbnf-lang/crates/…` to `../../bbnf-lang/crates-infra/…`, and the dead `bbnf = { path = "../../bbnf-lang/crates/core" }` line (plus the `bbnf-ir`/`bbnf-ser` siblings) should delete. bbnf-lang's removal step is HARD-GATED on confirmation of this repoint — a mis-order bricks both trees' local builds. The exact diff rides this letter; applying it is a one-minute edit to a gitignored file. csp-solver keeps its csc411 coordinate. |
> | MED — process ask | **Crate-path stability notice, reciprocal.** Any bbnf-lang wave that renames/removes a path your patch table names will land a one-line old→new note in its close commit. We ask nothing of your code — only that future parse-that sessions know this letter is where the coupling is documented. |
> | OFFER — review at leisure | **The PT-R track (Rust), five proposal tranches.** PT-R1 pure-primitives cut + de-nightly-as-redesign (`std::simd` → `core::arch::aarch64`, smallbox → BumpSlab, the two literal x86 arms in `scan/quote_parity.rs`/`scan/number_simd.rs` purged, an x86/portable_simd anti-regrowth guard); PT-R2 regex-engine SOTA (is_accept fold, padded find_at, lazy-DFA degrade of the 4096 bail, the ORPHANED `detect_accel` table wired into the DFA search, a truffle tier, SIMD mantissa accumulate, a 1.0 `Regex` façade); PT-R3 the bidirectional fold-back (skinny's recognizer tier upstream; ONE `eisel_lemire` home; `Box::leak` → interner; regex.bbnf as a live differential gate). Sketches + evidence coordinates in bbnf-lang's `sk-v23/SPEC.md`; pull what earns its keep, on your letters. |
> | OFFER — review at leisure | **The PT-T track (TypeScript, → 1.1.0).** PT-T4/PT-T5 answer value.js's own live PT-E1/PT-E2 asks (scoped diagnostics via your PACKRAT_ARMED latch; inference tightening), plus V8 hidden-class normalization, a grammar trace mode, the legacy state-tier sweep, and the worker-safe result shape. Naming the PT-E overlap here saves you re-deriving it. Backbone discipline assumed throughout: value.js / latex-paper / keyframes suites against any candidate before publish. |
> | RECORD, not ask | **The fold-back seam.** bbnf-lang will consume your regex engine as a byte-identical vendor mirror (`crates-infra/bbnf-regex`) at a pinned commit and proposes folding its 322-LOC recognizer tier upstream (PT-R3). Recorded so a future session sees the seam; nothing moves in your tree until a parse-that session pulls it. |
> | RECORD, not ask | **x86 abrogation posture.** bbnf-lang is aarch64/Apple-M only, everywhere, with anti-regrowth enforcement. Your platform envelope is your own call; the PT-R1 guard proposal is scoped to match whatever you decide. |
>
> Nothing here gates any parse-that wave beyond the one dated repoint that protects your own
> build. — bbnf-lang SK-V23, 2026-07-09.

---

## CLOSING LEDGER — what this draft decides

1. Research/08's skeleton RATIFIED: six bbnf tranches, five parse-that tranches; GW0.b
   execute/closure split ratified; GW4 split ratified.
2. The x86 enforcement mechanism DECIDED (§2.2-2.3): structural token gate primary, transitional
   allowlist dated TB1→TB2.
3. The type plane sized GREENFIELD, the one SK-V22 override, carried at TB4.W1 with the B3
   width patch demoted to a wave-item.
4. PT-R2's 4096-bail degrade UPGRADED to lazy-DFA (supersedes the sk-v22 NFA-walk framing);
   Teddy/sheng explicitly gated.
5. The coordination letter frozen as Appendix A with ONE time-sensitive row; review-not-execute
   for all PT content.
6. The fan-out contradiction resolved: 3 default / 6 write ceiling / 7 read-only lanes.
7. The feature triage bound: nine bbnf folds + eight PT folds homed; the NOT-NOW nine ledgered
   with reasons; the shopwindow NAMED at TB6.W3.
8. Six prototype briefs dispatched against the riskiest claims (prefilter payoff, x86 purge
   buildability, value/type greenfield shapes, V8 shape stability, SIMD mantissa).

**Pass-1 amendments (2026-07-10), agglomerated from PB-1–PB-6 + the nine critiques:**

9. The fused-trigger accel gate made MANDATORY at PT-R2.W2; the ungated form REFUTED (dense
   regresses 1.1-1.9×); the skip-heavy win claim run-length-caveated; gate-cost re-measure
   certification-grade at close (PB-1 + honesty B1).
10. C.1 restated proven-for-legacy / gated-for-greenfield (regen 310→0 x86 tokens, zero
    insertions); the C.2 wasm32 token rule wave-owned at TB1.W3 with a two-way planted proof
    (PB-2 + its critique).
11. `NodeClass`/`Classify` ADOPTED as the TB4.W2 value-plane seam; the sheets retirement
    RE-SCOPED out of the unconditional −1,059, gated on its own differential oracle (structural
    events + 39-kind pass-through); the TB4 close gate extends parity to every retired member
    (PB-3 + its critique). *(Gate framing SUPERSEDED at item 18: PB-3b settles CARRY.)*
12. TB4.W1 re-pinned to the sk-v21 §6 order (steps 1-3 land, 4-6 demand-gated — PB-4's
    inversion rejected); the falsifier broadened to per-rule TypeDesc/registry equivalence;
    the HM surfaces declared scaffold-dormant (Y.13 honesty); the orphaned `@host`/closure
    surface re-homed to TB4.W1 land-or-delete; the arrow-bearing corpus migration sequenced at
    TB1.W1 so the gate is never vacuous (PB-4 + honesty B2 + architecture B1).
13. PT-T4.W3 close restated no-regression-beyond-noise on a quiet host + a debug/trace parity
    assertion (PB-5); PT-R2.W4 adopted with a COLD full-parse canada cyc/B close clause (PB-6).
14. The ordering law grew a defined CONFIRMATION artifact, opening-time delivery for the
    TIME-SENSITIVE row, and the bounded NEIGHBOR-GRACE fallback — the critical path no longer
    hangs on an un-SLA'd external act (charter critique).
15. TB3.W2 split unilateral (vendor mirror, fork delete) from contingent (recognizer delete),
    with a named CONTINGENCY row — the spine never gates on a non-compellable act
    (architecture B3).
16. TB1.W4 owns the DEFERRED-LEDGER continuity audit — no row orphaned under GW→TB
    re-lettering; the REDRESS rejected-route registry cleared per greenfield (architecture B2).
17. The CONTINGENT-DELIVERY clause added at §4.0 — OWNER-RATIFY, a named freeze precondition
    (charter critique).

**Pass-2 amendments (2026-07-10, the closing agglomeration), from PB-3b + research/09 +
research/10 + the two re-critiques:**

18. **SHEETS = CARRY, unconditional** — PB-3b's executed byte-parity (zero Enter/Leave events
    on the sheets tape; pure `Classify` byte-divergent from the `rule_kind` navigation;
    `NativeTag` rejected as re-imported flattening). The sheets differential oracle DELETED
    from the TB4.W2 close gate; the retirement headline recomputed −1,059 → **−780 JSON-only**
    (the ~16-LOC css declaration-values sink retires per its own oracle, uncounted in the
    headline — the two-way arithmetic reconcile pinned); item 11's gate framing superseded.
    The neutral surface's honest span: keyed/indexed value trees only; flat-tape
    rich-`rule_kind` grammars keep their native sweep.
19. TB1.W4's continuity audit re-pointed at research/09's pre-drafted 70-row table (verify,
    not discover; "~40" corrected to 70); the N5 oracle-hygiene sweep folded into TB1.W4; the
    Lock-5 TS/WASM + TS-path standing deferrals named "authority unchanged, re-verified"; the
    N20 revival door bound to TB5.W1's cost table (closing pass-1 L1 with it);
    walk-content-not-labels (P7's literal TW6 label → TB3.W1 by content).
20. The three owner adjudications folded as veto-able PROCEEDING positions per research/10:
    Item 1 contingent-vs-delivered framing (PT-R.5 dated ask at TB1 opening + bounded
    fallback; the full-veto hard-halt branch disclosed, never papered); Item 2 the
    NEIGHBOR-GRACE duplicate sanction; Item 3 release.yml aarch64-only + the named-exception
    clause. The default-proceed authority is stated as itself the first thing a veto may
    withdraw (honesty re-critique governance note).
21. The five pass-1 MODERATE interlock gaps closed: M1 `materialize_f64` seam owner (TB4.W2
    authors, TB5.W0 consumes; decode-ordering law restated); M2 PT-R3.W0 declared
    detached/time-clamped (the PT-R2 entry dep binds W1-W3 only); M3 the standing
    allowlist meta-invariant at every close; M4 C.2 scans the vendored mirrors + the
    clean-pin sync law; M5 the TB4/TB5 schedule risk declared.
22. Re-critique scores recorded: p2-architecture 94, p2-honesty-charter 97 — both with ZERO
    open design questions; every named determinate fold (R1/R2/R3 and the synthesis lag)
    lands in THIS revision. Pass-2 movement ledger in `CONVERGENCE-LEDGER.md`.
