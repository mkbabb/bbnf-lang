# LOCK-6 — THE ISOMORPHISM PLANE — ADVERSARIAL CRITIC 1 (within-lock), pass 1

*Critic 1 of 2. Weighting: internal soundness of each family spec + whether the two prototypes
actually tested the sharpest uncertainty. Grounded against the extant tree this session (paths
re-verified, not taken from prose). The synthesis (FAMILY-SPECS) is genuinely strong — it is
honest about the incompatibility map, it grounds seams in code, and it correctly draws 6D-floor /
6C-collapse. This critique attacks what it over-claims, not what it got right.*

---

## THE SYSTEMIC FINDING (binds every percentage below)

**The plane's central artifact — the §5 stratification table — is HALF-VERIFIED, and the half that
is missing is load-bearing.** The table crowns **6B to govern the declarative/tape/oracle surface**
and installs **6D as the floor beneath every cap**. Both crownings rest on numbers the two
prototypes did **not** measure:

- The stratification is explicitly gated (§5 last ¶, §6) on *two* reasonable-point numbers: 6B §2
  open-Q i (generated-LOC vs hand-LOC per face) and 6A §1 open-Q i (clean-map ratio). **Only 6A's
  number was measured** (PROTO-2: 47/61/39). **6B's decisive number was never measured — no 6B
  prototype ran at all.** The family crowned to govern the *largest* surface has the *least*
  evidence.
- §6 names the two sharpest uncertainties: **Q1** (is 6C independent?) and **Q2** (is 6D a
  mandatory floor?). It commissions **Brief 1** (Q1 → PROTO-1, RAN, resolved NEGATIVE — good) and
  **Brief 2** (Q2, described verbatim as *"Cheapest, fully in-tree, single `cargo test`"*).
  **Brief 2 was NOT run.** PROTO-2 ran a *different* experiment — research-A's prototype-test 3
  (the 6A surface ratio), a secondary number — and even mis-labels the missing probe: it closes
  "probe for that seam is PROTO-1/T3 territory," but PROTO-1 is the 6C NEON probe, not a T3 probe.
  **The cheapest decisive experiment for the second-sharpest uncertainty was skipped in favor of a
  more expensive secondary one.** Q2 — the thesis on which the entire "6D floor everywhere" reading
  rests — is therefore *anchored* (the LIVE `nope` case is real, verified at
  `isomorphic_path_error.rs:129-150`) but *not measured* (no linter exists to demonstrate T1/T2
  actually pass GREEN; "T1/T2 pass green here" is eyeballed from identical variant shapes, not
  produced).

**Constitution vii labeling violation.** 6B and 6D each received a running prototype allocation in
their research briefs (6B: four tests; 6D: four tests) and **neither ran this pass**. Their
dispositions read "ADVANCED" with the same typographic confidence as 6A (measured) and 6C
(measured). Per clause vii, 6B-ADVANCED and 6D-ADVANCED must carry the **SPEC_ONLY** label on every
claim not backed by PROVE-14 or `bbnf-path-ts`. The synthesis does not apply it. This is the single
most correctable pass-1 defect.

---

## Family 6A — MIRROR-TREE — convergence **62%**

Strongest-evidenced ADVANCED family: it has a running prototype (PROTO-2) that measured its decisive
number and confirmed its extractor toolchain runs (rustdoc-JSON, 0.25s, nightly already pinned → no
new blocker). The disposition (ADVANCED, strictly dependent on 6D) is internally sound. Open gaps:

- **(6A-i) The exemption-ratio budget (gate d) is a gate that CANNOT fail — no threshold is set.**
  PROTO-2 measured a **39% idiom allow-list** and correctly flagged the budget as "load-bearing,
  not decorative." But neither the synthesis nor the prototype fixes a numeric ceiling. A budget
  that "warns when the exempt fraction grows" with no defined RED line never goes RED. With 2 of
  every 5 `Parser` items exempt, 6A's linter is proving correspondence on ~60% of the surface and
  *budgeting* the other 40% — and the budget is currently vacuous. **Gap: set the numeric RED
  threshold and justify it, or the linter is vacuous on 40% of the combinator surface.**

- **(6A-ii) The grammar-anchoring "strongest form" self-defeats into the 6A⊥6B tautology it
  accuses 6B of.** research-A §2 makes the map grammar-derived on both sides ("derive generable
  names from the grammar so they cannot go stale") and admits "where the map's entries are derived
  rather than authored, 6A shades toward 6B." But §5's own incompatibility argument says a linter
  comparing two generator outputs is a tautology (consumer-less substrate, Y.13). **So 6A's linter
  is non-tautological ONLY on the hand-idiom residue — which is exactly the 39% allow-list.** The
  clean 61% is either hand-authored twice (real linter value, but then not grammar-anchored) or
  grammar-derived (grammar-anchored, but then tautological to check). 6A cannot have both. **Gap:
  the synthesis must state which — and whichever it picks, 6A's genuine load-bearing surface is the
  39% residue + arity-drift, not the 61% "clean spine" it advertises as its strength.**

- **(6A-iii) The mechanism itself (manifest + T1 linter) is UNBUILT — only the ratio differ ran.**
  research-A prototype-test 1 ("Name-map linter MVP on the one LIVE pair; delete the `schema.rs`
  prose") did not run. PROTO-2 ran a syntactic *ratio* extractor, not a manifest-driven linter, and
  did not touch the `schema.rs:1-15` prose it promises to delete. So 6A's core deliverable is
  SPEC_ONLY; only its *feasibility number* is measured. **Gap: T1 linter MVP + prose deletion
  unrun.**

- **(6A-iv) Brief 2's green-structural/red-behavioral PAIR was not produced.** 6A's whole
  disposition ("necessary-not-sufficient; contains 6D's T3 as a mandatory tier") rests on T1/T2
  passing GREEN on `$.statuses[0].nope` while behavior diverges. The divergence is LIVE-verified,
  but "T1/T2 pass green" is asserted about a non-existent linter. **Gap: run the cheapest experiment
  in the lock (Brief 2) — it converts 6A's central caution from assertion to measurement.**

- **(6A-v) Cross-repo CI ownership (F6) remains open** — per-repo manifest fragments + vendored
  conformance corpus is proposed, not prototyped. Unchanged from research.

---

## Family 6B — SPEC-GENERATED-SKELETON — convergence **48%**

The family crowned to govern the largest surface, with the **least** evidence. Its existing *pattern*
is real and verified (`bbnf-path`/`bbnf-path-ts` ship; `backend_ts.rs` + `backend_ts_typecheck.rs`
live — I confirmed all four). But every claim specific to the family's *actual new work* is
unbuilt and untested.

- **(6B-i) ZERO running prototype; the decisive number is UNMEASURED.** research-B §6 assigned four
  prototype tests (lockstep regen, oracle-cannot-self-stub, measure the boundary via `%HaveSameMap`,
  quantify "reasonable"). None ran. The reasonable-point number — 6B's *entire verdict* per §2
  open-Q i and the constitution's clause (iii) — is 0% measured. **Gap: 6B is SPEC_ONLY and must be
  labeled so.**

- **(6B-ii) All three "new faces" are 0% built — the "completion, not greenfield" framing is
  misleading.** I grepped the backend: `emit_tape_layout`, `emit_tape_decoder`, `emit_oracle` —
  **none exist**. Only Face II (type interfaces) is partly present (via `backend_ts.rs`). The
  load-bearing Face I (layout, "where drift actually lives," advertised "100% generated") and Face
  III (oracle, "highest-leverage") are entirely unwritten. "6B's real work is completion" understates
  the risk: the work that IS the family is greenfield.

- **(6B-iii) Face I's grammar-derivation anchor does not check out in-tree.** FAMILY-SPECS §2 and
  research-B §3a both cite `crates/ir/src/passes/materialization.rs` and name `STRING_NEEDS_DECODE`
  as the grammar-derived layout fact. I verified: **that file does not exist** (`MaterializationClass`
  lives elsewhere — `crates/ir/src/passes/mod.rs` + tests), and **`STRING_NEEDS_DECODE` appears
  nowhere in `crates/`**. The reachability claim ("the decode fact is reachable from
  `MaterializationClass`, already a CSP variable") is asserted over a mis-cited seam and an absent
  constant. **Gap: prove the grammar→layout-fact derivation actually exists before Face I can be
  called "100% generated."**

- **(6B-iv) THE ELEGANT-REDUCTION TRAP: the AoS↔SoA reconciliation is dressed "definitional" but is
  an unconverged hard core.** research-B §3a and FAMILY-SPECS §2 concede two *incompatible* tape
  dialects coexist: AoS `record.rs` **stores** a `TapeKind` discriminator in a nibble; the SoA
  offset-tape **stores no discriminator** (kind re-derived from `source[offset]`). 6B claims "one
  spec, N faces" makes the reconciliation "definitional." A single `LayoutSpec` that emits BOTH a
  stored-discriminator layout AND a discriminator-free layout is not obviously expressible — this is
  a "and then the hard part" step reading as trivial. **Gap: the one-spec-two-dialects expressibility
  is the load-bearing unproven core; it must be demonstrated, not asserted definitional.**

- **(6B-v) Cross-repo regen (F6) relocates, does not dissolve, the boundary** — correctly stated by
  the synthesis, but the "regenerated parse-that-side, shipped inside the letter" pipeline is a
  named, unprototyped seam.

*Note the synthesis's own honesty:* §2 already REFUTES 6B on the combinator type algebra (ts-rs/
typeshare flatten conditional types; PT-E2 `infer V` can't be derive-macro'd). That refutation is
sound and well-cited. It does not rescue the unbuilt/unmeasured state of the *declarative* faces
6B is crowned to own.

---

## Family 6C — WASM-CORE — convergence **60%**

The best-instrumented family: the sharpest-uncertainty probe (PROTO-1) RAN, cold-paired, with exact
parity checks, and resolved R-A **NEGATIVE decisively** (wasm SIMD128 at 0.34–0.47× native NEON).
I verified the substrate: `simd-scan/src/wasm.rs` is a genuine hand-written `core::arch::wasm32`
v128 twin (13 intrinsic sites), and the CLMUL→shift-XOR fallback is real (`parity.rs`). PROTO-1's
"already a second implementation" reading is sound. The disposition (BLOCKED-candidate as primary,
ADVANCED as crossing mechanism) is evidence-backed. Open gaps:

- **(6C-i) PROTO-1 measured the WRONG comparator to settle 6C-on-the-bbnf-side.** It compared wasm
  SIMD128 vs **native aarch64 NEON** — but native NEON is **not a browser deployment option**. The
  ecologically-valid question for "should the bbnf parser live in wasm" is **wasm-SIMD128 (~3 GB/s)
  vs a TS-native scalar scan** — both browser-deployable. That comparison was not run. If TS-native
  scan is < 3 GB/s, wasm-core still wins on the bbnf side *despite* NEON not crossing. PROTO-1
  correctly refutes "one wasm impl earns a perf claim vs native," but it does **not** decide
  "wasm-core is the right browser home for the bbnf parser." The synthesis treats 6C-on-bbnf-side as
  settled-viable; it is undecided. **Gap: the wasm-vs-TS-native-parse comparison — the actually
  decision-relevant one — was never made.**

- **(6C-ii) The "survives as crossing mechanism" role rests on PROVE-14, not on any pass-1 probe.**
  PROTO-1 measured the parse/scan kernel (source→tape), NOT the crossing (planes→typed arrays). The
  4.4×-stop-early / 0.9×-full-walk facts that justify 6C-as-crossing-mechanism come entirely from
  PROVE-14. That is legitimate prior evidence — but it means 6C's *surviving* role is inherited, not
  re-measured this pass. Fine, but state it: 6C-as-crossing = PROVE-14-anchored, not PROTO-1-anchored.

- **(6C-iii) R-B memory-grow view-invalidation gate is unbuilt** — flagged by research-C and the
  prior-art sweep as a hazard NOT in GN-4.md's R4, but no prototype exercised the two-increasing-
  parses-through-one-instance corruption case. It remains a named-but-unverified born-RED gate.

- **(6C-iv) R-E (sync ESM confinement) rests on an unre-verified cross-repo count.** "value.js has
  59 sync `all()` sites" is the load-bearing datum confining 6C to the bbnf side; it is a parse-that
  count (F6 read-only) not re-confirmed this session. The *argument* is strong; the *number* is
  taken on faith. Minor, but it is the hinge of incompatibility #3.

---

## Family 6D — ORACLE-ISOMORPHISM — convergence **55%**

The mandatory floor — compatible with all, required by each cap. Its substrate is genuinely rich and
LIVE-verified (`ts_node_execute.rs`, `json_canonical_parity.rs`, `sonic_rs_parity.rs` all present;
the consolidation thesis is sound). Its blind-spot analysis (§4.1, coverage-gap = silent drift) is
the most honest self-critique in the pack. But as the family the whole plane leans on, its evidence
is thin:

- **(6D-i) ZERO running prototype; BOTH decisive numbers UNMEASURED.** research-D §5 owes the lock
  two measured numbers — **drift-escape rate** (under corpus + coverage-guided fuzz) and **marginal
  maintenance cost per grammar** (add TOML, reach parity under 6D alone). Neither ran. 6D's claim
  "silent drift → ~0" is precisely the unmeasured proxy. **Gap: 6D-as-floor is SPEC_ONLY on its two
  verdict numbers and must be labeled so.**

- **(6D-ii) THE mandatory-floor thesis (Q2) — the reason 6D is installed everywhere — was never
  measured.** Brief 2 (the necessary-not-sufficient decider, cheapest in the lock) went unrun. 6D's
  installation "beneath every cap" is inferred from the LIVE `nope` case, but converting that into
  the demanded green-structural/red-behavioral PAIR — the experiment that would *prove* structural
  caps are insufficient alone — did not happen. The floor's foundation is anchored, not measured.

- **(6D-iii) The IEEE-754-bit digest — 6D's "single most important sharpening" — is untested.** The
  claim that folding the 8-byte big-endian f64 bit pattern makes the digest language-agnostic is
  elegant and probably correct, but "V8 `DataView.getFloat64(be)` folds identically to Rust
  `f64::to_be_bytes` over the corpus" is asserted, not run — and it is exactly the kind of
  cross-language number claim the family exists to be paranoid about. NaN-canonicalization and
  `-0.0→+0.0`-iff-grammar-says-so are named but unexercised. **Gap: the digest's cross-language
  bit-equality is unverified over even one corpus.**

- **(6D-iv) The three-way accept/reject partition and coverage-guided fuzzer are proposed, not
  seeded.** The JSONTestSuite-style must-accept/must-reject/may-either corpus is "missing and
  required" (research-D's own words); the coverage-guided grammar fuzzer with delta-debug shrink is
  the family's "teeth" and does not exist. 6D-without-teeth is just the scattered parity asserts it
  proposes to consolidate — which is real, but is the *floor of the floor*, not the offensive
  detector the disposition claims.

- **(6D-v) "pure 6D BLOCKED for the tape" is correctly identified** (layout-freedom negates the
  byte-identical SoA the zero-copy win requires). No gap here — this is the synthesis at its best.

---

## Cross-cutting: seam hygiene (within-lock)

- **Stale tape path in two research reports.** research-C and research-priorart cite
  `crates/runtime/src/tape/mod.rs`; I verified that path is **ABSENT** — the tree consolidated under
  `crates/core/src/runtime/tape/mod.rs` (which exists). research-A caught this; FAMILY-SPECS uses the
  correct path. Not a synthesis defect, but the research substrate carries a stale seam that any
  downstream reader will trip on.
- **x86 SIMD lives in the probed crate despite constitution iv ("x86 dead").** `simd-scan/src/
  avx2.rs` and `avx512.rs` are present. This is LOCK-9 surface, not LOCK-6's to fix, but PROTO-1
  built `simd-scan` and the refutation law says x86 is dead — the cross-lock inconsistency should be
  surfaced to GESTALT, not buried.

## The single highest-value pass-2 directive

**Run Brief 2.** It is the cheapest experiment in the lock (single `cargo test` over the already-LIVE
`isomorphic_path_error.rs`), it decides the second-sharpest uncertainty (Q2), and it underpins the
two least-evidenced rows of the stratification table (6B-governs and 6D-floor). Its omission — while
a more expensive 6A-ratio probe ran instead — is the clearest prototype-misallocation of pass 1. A
2-line CI harness converts "T1/T2 pass green, T3 goes red" from the plane's most-repeated assertion
into its first measurement.
