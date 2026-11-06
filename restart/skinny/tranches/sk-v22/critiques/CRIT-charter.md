# CRIT-charter — SK-V22 SPEC-DRAFT adversarial critique (charter + precepts compliance)

Date: 2026-07-09. Agent: CRITIQUE (adversarial, conservative). Lens: does SPEC-DRAFT.md
discharge every binding charter correction, the re-issued original call's demands, and the
frozen DEFERRED-LEDGER without orphaning a dispositioned item — and does any residual carry a
named owning wave, or is it an open design question. Evidence is file:line against the tranche
corpus and the on-host trees the reports cite.

**Convergence: 83 / 100.** The archival, doc-home, leverage, and profiling legs are
executable-tomorrow and evidence-complete. The two parse-that seams (§3.2 one-regex-home; the
Eisel-Lemire fold) leave a genuine cross-repo design question open, not merely wave-owned
execution — that, plus a consumer-less-crate uplift the SPEC does not reconcile, is the whole
gap to 100.

---

## Part A — Charter corrections: every one OWNED

Walking the five binding corrections (PASS-ALPHA:26-58) and the re-issued call's demands:

1. **Playground archives outright** — OWNED. §2.1 tags `playground-spa-final` + `git rm`;
   §6 point 5 explicitly supersedes ADDENDUM §2's "rebuild the existing SPA"; "only the
   skinny-backed wasm prerequisite survives as a concept." Research 01 §3 confirms the dirty
   `vite.config.ts` is moot (dies with the tree). Clean.
2. **bbnf-buddy is a live sibling** — OWNED. §3.1 SEAM-BUDDY records zero bbnf dependency,
   archival touches nothing, transitive `buddy → value.js → parse-that` noted for PT semver only.
   Research 03 §2B is the evidence (its `bbnf-*` strings are its own file-format brands).
3. **parse-that TS backbone; regex consumed; NO-GO dated; PT-E1 mirrored** — OWNED. §3.1
   SEAM-NOGO (dated 2026-07-05), SEAM-PT-E1, the consumer map (§3.3 opening). Correct.
4. **parse-that IN scope, both TS and Rust, FULL SOTA audit** — OWNED with a reservation
   (Finding C). §3.2 PT-R.1-6, §3.3 PT-T.1-5, §3c the no-legacy manifest all land; the
   alloc/LLVM/V8 exploitation is concrete (BumpSlab arena, `core::arch` re-founding, hidden-class
   normalization, IC/megamorphism discipline). The reservation: research 05 §2's pivotal finding
   is not reconciled (Finding C).
5. **Profile under load, with accounting** — OWNED, exemplary. §5 makes the PMU/paired/cold/
   multi-window/loadavg-recorded method standing law; research/04 IS the load-accounted baseline,
   self-validated within 2-5% of A1/report-33 (04:29-33). One disclosure note (F).

The "no workarounds / no legacy into the graduated tree" demand is honored by §1.3's
REWRITE-SUPERSEDED verdict on the core hand seam (only vendored infra + `.bbnf` data port in)
and §3c's transposition-not-patch discipline. "Chronic items folded" is asserted by §6 and the
§1.2 re-map (see Part C).

---

## Part B — The three findings (design residuals, not pure execution)

### FINDING A (blocking to a 100): the one-regex-home creates an unmechanized cross-repo dependency in tension with the self-containment gate

§3.2 (B) rules that "the canonical `bbnf-regex` engine keeps the name and the home
(`parse-that/rust/regex`)" and §3.2 PT-R.4 states "skinny deletes `parse-that-regex` (3,022 LOC)
+ its `bbnf-regex` stub and depends on the canonical engine." §3.4 confirms GW2's leg gates on
"the canonical engine builds on the graduated stable toolchain against the graduated egraph home."

Consequences the SPEC does not resolve:

- **A cross-repo runtime+build dependency, mechanism unstated.** The graduated bbnf tree will,
  at GW2, depend on a crate living in a *separate repo* (parse-that). Is that a path-dep into
  `../parse-that/rust/regex`, a published crates.io pin, or a vendor? Research 03 §1A records the
  engine as `bbnf-regex 0.1.0` but never establishes it is *published*. If it is a path-dep, it
  is an escaping path-dep — which directly contradicts the GW0 close gate "zero path-deps
  escaping the workspace" (§1.5) and §1.4 point 4's "self-contained workspace." The whole
  archival premise is decoupling; this seam re-couples at GW2 with no named resolution.
- **A cross-repo dependency cycle.** parse-that's engine build-depends on egraph, whose only home
  after GW0.b is the *graduated tree* (SEAM-EGRAPH, 03:31-46); the graduated runtime then depends
  on parse-that's engine. So graduated-egraph ← parse-that-regex ← graduated-runtime — a two-repo
  cycle resolved only by gitignored patch tables, the exact fragility research 05 §5 and 07 R-7
  flag. The SPEC re-homes egraph to break one edge but silently creates the reverse.
- **`bbnf-simd`: relocate or depend-back, unstated.** §1.2 (TW4.1→GW3), §3.2 (C), and §3c R-1
  all treat `bbnf-simd` as resident in the graduated tree ("binds to bbnf-simd's live dispatch").
  But PT-R.4 says "the NEON scanners absorb" upstream into parse-that. Either parse-that gains a
  reverse dependency on the graduated `bbnf-simd` (another cross-repo edge), or the NEON kernels
  are duplicated. The SPEC names neither.

This is charter-sanctioned in *principle* (correction 3 names the engine as "consumed"), but the
charter naming a seam does not discharge the SPEC's duty to mechanize it. As written, "where the
unified engine lives, how the graduated tree resolves it without an escaping path-dep, and which
direction `bbnf-simd` flows" is an open design question owned by no wave — it sits between GW2 and
the PT-R track. This is the single largest bar to "executable tomorrow, zero open design
questions."

### FINDING B (medium): the one measured perf gap has split, cross-repo, cross-track ownership

Research/04 isolates exactly one honest deficiency: canada `fused/sonic` is 1.20× SLOWER, a
+1.48 cyc/B tax "localised to `materialize_f64` (the Eisel-Lemire/SWAR target)" (04:88, 149-159,
222-226). GW3's headline close gate is "canada eager cold beats sonic (the +1.48 cyc/B float tax
closed)" (§1.5). But PT-R.4 relocates the Eisel-Lemire number decode INTO parse-that at GW2
(research 05 §4.3: the byte-identical 660-LOC `eisel_lemire/table.rs` collapses to one module
upstream; §3.2 PT-R.4 "kill eisel_lemire duplication"). So by the time GW3 runs, the code its
gate must optimize lives in parse-that, while GW3 is a bbnf graduation wave binding to
`bbnf-simd` (§1.2 TW4.1→GW3). Who owns the SWAR fix to `materialize_f64` after the fold — GW3
(bbnf) or a PT-R wave (parse-that)? No cross-track hand-off is named. The tranche's only
decision-grade performance workload has ambiguous ownership across two repos.

### FINDING C (medium): a consumer-less Rust combinator crate is uplifted, not adjudicated for existence

Research 05 §2 is explicit and pivotal: "The Rust combinator crate has no consumer that survives
graduation" — every Rust consumer (core, ir, gorgeous, wasm, xtask) archives; the backbones
consume the *TS* package (05:108-126). Research 05 open-Q-A asks whether the graduated tree keeps
a general parser-combinator surface at all. §3.2 answers *half*: "the graduated bbnf tree does
NOT grow a combinator surface." It does not answer the other half — whether parse-that's own Rust
combinator crate, now consumer-less (its demo parsers deleted by PT-R.1, its bbnf consumers
archived), is worth the arena redesign (PT-R.2/RU-2) or is itself an archival candidate. The
charter mandates the combinator uplift, so executing PT-R.1/R.2 is defensible; but the SPEC
should record the tension (uplifting a crate nothing consumes) rather than route past research
05's own flagged open question. As written, PT-R.2's arena transposition polishes a surface whose
reason-to-exist the evidence disputes.

---

## Part C — DEFERRED-LEDGER + precepts orphan sweep: no hard orphan, one soft edge

I walked every DEFERRED-LEDGER row against the §1.2 re-map and §4/§5 durable-doc landings:

- §0 REDRESS 51/53/246/247 → GW0.a verbatim + NEGATIVE-RESULTS (§1.2 GW0.a row; §4). ✓
- §1 C1-C6 → GW1 (Lock-1 two-way), GW2+GW4 (C-fold), GW3+GW4 (C3/C4/C5), GW0.a (C6). ✓
- §3 N1-N18 → all re-homed (N1/N6/N25→GW2; N5/N9/N12→GW1/GW5; N11 W0.3-6→GW0.b-d/GW1). ✓
- §4 N19-N27 → GW1 (N19/N20/N24), all-waves (N21), GW0.b (N27), GW3 (N26). ✓
- §5 deferred-by-design (TS/WASM, bbnf-path-ts) → §1.3 KEEP-DORMANT/REWRITE-DEFERRED. ✓
- §6 pass-loop residues → P6→GW5, P7 walls→GW2 (moved *earlier* than the ledger's TW6 — a
  strengthening, gated at §1.5 GW2's WART-C-fold close), the nightly-vs-stable ratio (DL:122)→GW1
  old-pin/new-pin. The DL:122 mapping is DEFENSIBLE: skinny carries no own `rust-toolchain.toml`
  (02:112-118) and inherits the root nightly pin (03:55-59), so GW1's old(nightly)→new(stable-2024)
  capture is a real channel ratio, not a mere edition delta. Not an orphan.

The 23-directive precepts-walk (34) maps through the same rows (directive 9→N12, 21→C3/pluggable,
5/18→TW5). No directive is orphaned.

**Soft edge (Finding E, low).** A handful of sub-rows are owned only by the §1.2 blanket
assertion ("every row re-maps by this table; no row is orphaned") through their *parent* wave,
without an individually named landing gate: N22 (csp `node_budget` guard verification before
escalation), N23 (Y.5 connected-components CSP), the precept-21 `perf_cost` one-line production
fix (passes/src/lib.rs:590 per 34:137-147), and §5's profile-guided-specialization re-examination
trigger. All inherit GW4 (=TW4.2) ownership, so none is strictly orphaned, but this is weaker than
SK-V21's per-row-with-gate discipline; a synthesis pass should name their gates explicitly.

---

## Part D — Lower-severity notes

- **FINDING D (low):** §3.2 (C) imposes aarch64-only + scalar-fallback on parse-that's Rust crate
  (a separate published-library repo), rejecting research 05 §0's `wide`/`pulp` portable option,
  without establishing parse-that Rust's own platform/CI requirements (05:315-317 open-Q-C names
  this). Adjudicated, hence wave-owned, but the imposition of bbnf's Apple-M posture onto a
  sibling repo is under-examined.
- **NOTE F (disclosure):** the graduation baseline's finest layer — leaf-level self-time
  (04:161-182) — is TRANSFERRED from report 33, not freshly symbolized (the load band made
  xctrace unreliable). The charter fallback permits this and it is disclosed; PERFORMANCE-THESIS
  (§4) should carry the "inherited leaf ranking, fresh PMU floor" caveat so the provenance is not
  laundered.

---

## Verdict

The SPEC discharges all five charter corrections and the no-legacy/chronic-fold demands, and the
DEFERRED-LEDGER survives the graduation with no hard orphan (one soft blanket-assertion edge).
The archival mechanics (§2), leverage plan (§4), profiling law (§5), and PT-TS track (§3.3) are
executable-tomorrow. What blocks a top score is the parse-that Rust seam: Finding A
(the one-regex-home cross-repo dependency mechanism + self-containment reconciliation) is a live
design question, and Findings B/C are unreconciled interactions the synthesis flagged and only
half-closed. Resolve A explicitly (publish-or-vendor the engine, or grant GW0's gate a named
cross-repo exception, and fix the `bbnf-simd` flow direction), assign B's owner, and record C's
tension — and this reaches ≥95.
