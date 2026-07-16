# LOCK-7 — THE GRAMMAR TOTALITY & BOOTSTRAP — CONVERGENCE REGISTRY

Agglomerated by the AGGLOMERATOR (Fable-tier judgment). One row per family, carried forward
across passes. Convergence % is conservative — the critics' floor unless a named, disk-verified
reason overrides it. `path:line` claims in this registry re-verified on disk at skinny HEAD
`edcf55f86` (the tree has moved past both critics' HEADs — CRIT-1 `d2b32f6e3`, CRIT-2/parse-that
`ef10d5b` — so the agglomerator re-ran the load-bearing checks itself).

---

## STANDING VERDICT (current, after pass 1)

| family | name | status | conv % | one-line disposition |
|---|---|---|---|---|
| 7A | STAGED-BOOTSTRAP | ADVANCED (narrowed) | 45% | fixpoint LAW survives as 7D's graft; posture + Rung-1 banked-as-subsumed |
| 7B | TOTAL-SELF-HOST | BANKED | 35% | posture measured-dominated by 7D; Gap B → LOCK-3; Gap C BLOCKED |
| 7C | HOST-FUNCTION-LATTICE | ADVANCED | 60% | the survivor — ships value now (T2/T4); orthogonal mechanism the winner consumes |
| 7D | META-CIRCULAR-ORACLE | ADVANCED | 60% | POSTURE winner (INC-2 resolved to it); lowest-risk; ORACLE-ONLY lawful terminal |

**Crowding redirect applied:** 7A/7B/7D are three postures on ONE ladder converging on 7D's
oracle-first mechanism. 7B (the promote-to-production posture) is marked **BANKED** — it is
measured-dominated by 7D on the one decidable edge, its Gap B is double-homed to LOCK-3, its Gap C
is a BLOCKED-candidate. Two underexplored formulations opened in its place (OPEN-1 emit-wall,
OPEN-2 fixpoint-over-oracle — § pass-1 directives).

---

## THE FIVE GOVERNING FACTS (re-verified on disk this pass, superseding FAMILY-SPECS §0)

FAMILY-SPECS §0 anchored the whole lock. Two of its four "governing facts" were wrong, and both
prototypes' relationship to the current tree had to be re-adjudicated. Corrected ground truth:

1. **Skinny does NOT self-host — CONFIRMED.** `skinny/xtask/src/main.rs:40-52` dispatches only
   json/sheets/css/real-typed/conformance/checkasm. No `regen-bbnf`/`regen-regex`/`fixpoint`.
   The only `.bbnf` under `skinny/` is `skinny/grammars/json.bbnf` (0 `->`). The working
   self-host organs (legacy `crates/core/src/grammar/generated/bbnf.rs`, 21557 LOC; the
   `bbnf_bootstrap_reproducibility.rs` fixpoint gate) live in the LEGACY `crates/` tree that GN-0
   archives. **This is XL-4, the largest cross-lock gap.**

2. **The self-grammars are authored, wired to nothing — CONFIRMED.** `grammar/bbnf/bbnf.bbnf`
   (3448 B) + `expressions.bbnf` (1378 B) + `types.bbnf` (301 B). `bbnf.bbnf` uses the full
   surface to describe itself (`@import` L4-5, `@{` L33/byte 1324, `@host` L73, `?w`/`->`
   pervasive, `|params|` closures, the recursive `term→…→rhs` tower).

3. **§0's base-case wall was MIS-MEASURED. CRIT-1 correct, CRIT-2 + PROTO-1 wrong.** The current
   hand parser (`skinny/crates/grammar/src/lib.rs`, 2142 LOC) **DOES** handle `->`
   (`parse_postfix:1409` `starts_with("->")` → `parse_projection:1481` → `ExprKind::Project`),
   `?w` (`parse_seq:1366` standalone + `parse_postfix:1449`), `<<`/`>>` (`:1414`), groups
   (`:1516`), refs (`:1522`). **byte 421 (line 9, `identifier = /…/ -> Span`) PARSES.** The real
   wall is **byte 662 (line 17, `big_comment = ( … ) ?w -> Span`)** — the first `?w` *immediately
   followed by* `->`: `parse_seq` consumes `?w` as a standalone element and `continue`s, then
   routes the trailing `->` to `parse_postfix`→`parse_atom`→catch-all (`:1529` "unexpected token
   `-`"). **PROTO-1's byte-421 / "no `->` token in the RD parser" does NOT reproduce** — it ran
   against a worktree that predated the SK-V18 `->`/`?w` parser extension (its cited line numbers
   `parse_atom:1416`, `parse_postfix:1365` are an older `lib.rs`). **CRIT-2's "CONFIRMED byte
   421" is spurious** — it cited the *current* line numbers (`parse_atom:1495`, catch-all
   `:1529`) that literally contain the `->` handler at `:1409`, without recompiling.

4. **The base case is THREE bounded parser fixes, NOT a rewrite / NOT a BLOCKED-candidate.**
   (verified against parser source):
   - **(i)** `?w` immediately followed by `->` — `parse_seq`'s `?w` branch (`:1366`) consumes and
     `continue`s, mis-routing the trailing `->`. One-branch attachment fix. (wall #1, byte 662.)
   - **(ii)** `@{ }` host-capture atom — genuinely unhandled; `parse_atom` (`:1495-1531`) has no
     `@` arm. (wall #2, byte 1324/line 33.) **This is the same fix 7C-T4 needs.**
   - **(iii)** `|params|` closures — `parse_seq` breaks on `|` (`:1381`, treats as alternation
     delimiter); closure param binding unhandled. (wall #3.)
   These are ordinary parser-extension work, failing the CONSTITUTION parsimony test for
   "primitive equal in difficulty to the original problem." **The parse layer of the base case is
   NOT BLOCKED.**

5. **§0 fact 4 FALSIFIED (regex producer already ships) — CONFIRMED by me.**
   `parse-that/rust/parse_that/src/regex/mod.rs:34` is
   `pub fn parse_generated(pattern) -> Result<Hir, ParseError>` — runs the generated parser THEN
   `host::lower` (`:57`). The public API is a **producer**. §0/research-D cited the *internal*
   `generated.rs:2288 -> bool` recognizer and mislabeled it "public." G-7D-3's
   recognizer→producer gap is **already closed**; that is exactly why PROTO-2 was runnable today.

**The re-scoped make-or-break (the one that survives).** The PARSE wall is small (3 fixes). The
**EMIT/lowering wall behind it is UNPROBED by construction** — PROTO-1 died at the mis-measured
parse wall and never reached `emit_from_source`. Whether the general-descent emitter LOWERS
bbnf's recursive-alternation tower + closures + `@import` to a *working generated parser* is the
genuine §0 uncertainty and it is **RED-unknown**. That is OPEN-1 (below): the underexplored
formulation every ladder family actually rests on and none has tested.

---

## PASS 1 — history, per family

### 7A — STAGED-BOOTSTRAP — ADVANCED (narrowed) — 45%

- **Convergence 45%** (critics' floor 45/45; not raised — see below).
- **What is sound.** Rung-1 staleness reuses `regen.rs:37 check_target` verbatim → trivially
  ships. The fixpoint mechanism is correctly specified (rustc `S₁==S₂` fixed point, not
  `S₂==S₀`). Provenance-ledger/blake3 design coherent (`regen.rs:153 validate_inputs` extends).
- **Why 45% and not higher despite the base-case de-risk.** The parse wall being 3 small fixes
  (fact 4) removes a *feasibility* fear, but it does not raise *convergence*: 7A's only
  distinguishing rung (Rung 2, the stage ladder) still gates on the **unprobed emit wall**
  (OPEN-1). Rung 1 is json's existing staleness check re-pointed — a **gate-that-cannot-fail**
  (it proves codegen-freshness, not self-hosting). So the shippable part is vacuous-adjacent and
  the distinguishing part is unproven.
- **Redundancy verdict (crowding).** 7A-default (frozen seed, R4-i) keeps the hand parser as the
  production path forever → **self-VERIFYING, not self-hosting**; INC-3 admits 7A-default ⊆ 7D +
  a fixpoint gate. The seed-disposition posture debate (frozen R4-i vs retire R4-ii) is **moot**
  under the 7D collapse and is **banked**. 7A's ONE non-redundant contribution is the **fixpoint
  LAW** (byte-identical `regen∘regen`, no-2-cycle) + provenance ledger — which 7D lacks a live
  version of (its "subsumes 7A on promotion" is vacuous because nothing promotes). 7A therefore
  ADVANCES *narrowed to its fixpoint law*, grafted onto 7D as OPEN-2.
- **Open gaps.** (a) emit wall unprobed (OPEN-1). (b) seed_LOC headline metric unmeasured — the
  trade the family exists to make is un-numbered. (c) Q2 twin-ladder (one `fixpoint` xtask over
  bbnf+regex, or two across repos) unresolved; double-counts the minimal-surface metric.
  (d) build-budget ~88s→~0.5s datum is a **LEGACY-tree transplant**, not a skinny ceiling (skinny
  xtask self-hosts nothing — a greenfield number does not yet exist). (e) Q3 regex
  promote-or-delete RESOLVED toward oracle-only by PROTO-2 (perf 1.8×) — record closed-toward-7D.
  (f) the GN-0/GN-1 rescue gate (XL-4) is not in 7A's born-RED gate list.

### 7B — TOTAL-SELF-HOST — BANKED — 35%

- **Convergence 35%** (critics' floor 35/35). **Marked BANKED per the crowding redirect.**
- **Why BANKED.** The three ladder families converge on 7D's oracle-first mechanism, and 7B is
  the redundant posture:
  - **Gap A (promote front-end to production) is measured-DOMINATED by 7D.** PROTO-2's regex
    analogue — the only edge with committed code both sides, same generated-RD-over-general-IR
    architecture — runs **1.77–1.81× slower**; neither promotion gate fires. INC-2 resolves
    against 7B on the one decidable edge. 7B's own R1 concession ("if diagnostics unmet, Gap A
    demotes to oracle") *is* 7D. Gap A also shares the base-case wall. → **BANK Gap A.**
  - **Gap B (`ir.bbnf` generating `ExprKind`/`BackendExpr`) is double-homed with LOCK-3 3C
    GRAMMAR-HOMOIOUSIA (XL-1)** — verbatim the same deliverable. Conditional on LOCK-3 picking 3C
    (LOCK-3 has 3A/3B/3C/3D); if LOCK-3 picks otherwise, `ir.bbnf` has **no consumer in the IR
    spine** (Y.13 consumer-less substrate). And its gate ("byte-identical to a frozen snapshot")
    means the grammar produces *nothing a drift-gate-over-the-enums doesn't already give* — the
    only consumer is a gate that needs no grammar. → **redirect Gap B to LOCK-3 adjudication.**
  - **Gap C (emitter-template grammar) is a BLOCKED-candidate category error (R3).** A template
    DSL emitting *correct specialized* Rust+NEON needs the type/borrow discipline it replaces —
    the very problem it removes. Survives only under an unproven strict pure-structure scoping
    with all typed logic behind kernel FFI. Elegant-reduction trap: "structure as grammar,
    kernels stay hand-Rust" reads clean but "control-flow/dispatch/tape-assembly expressible
    without a borrow checker" is the unconverged hard part. → **BLOCKED.**
- **Salvage.** 7B's honest residue is not a family but two questions homed elsewhere: the SCOPE
  axis (how far up the tower is feasible) → adjudicated in GESTALT against LOCK-3; the
  pure-structure-scoping of Gap C → a research question, not a route. If LOCK-3 does pick 3C, Gap
  B may be **un-banked into a joint LOCK-3⊗LOCK-7 family** — a named supersession is owed.
- **Open gaps.** (a)–(c) as above. (d) Q1 diagnostics parity (does a generated parser emit spans
  as richly as 2142 LOC of hand machinery?) unmeasured — rustc's deliberate hand-parser retention
  is standing evidence it won't. (e) Q3 regex-`->`-to-HIR is LOCK-5-dependent AND materially
  broken (host::lower rejects all bounded `{n,m}`). (f) four-plane fixpoint build ceiling
  unmeasured; bootstrap-of-bootstrap explosion (R2) unquantified.

### 7C — HOST-FUNCTION-LATTICE — ADVANCED — 60%

- **Convergence 60%** (critics agree 60/60). The best-substantiated in-tree family; the survivor.
- **What is sound (all disk-verified).** `passes/src/lib.rs:798` string heuristic
  (`name.contains("host") && (value.contains("decode")||value.contains("parse"))`) exists
  verbatim; `bbnf.bbnf:73` typed `@host … : type_name` directive exists and is **zero-usage
  dormant** (usage across all shipped grammars = 0, confirmed); `bbnf.bbnf:33` `@{}` exists;
  `grammar/src/lib.rs:222 BBNF-UNSUPPORTED-HOST-CAPTURE` exists; `json/host.rs` is **empty** (JSON
  proves the primitive floor — number/string decode are primitives, not host fns). The
  attribute-grammar framing (`&mut Ctx` = inherited attribute, return = synthesized) is exact.
  T2 (delete the heuristic → `check-json` bit-for-bit) and T4 (productionize `@{}`) are genuine
  in-tree wins needing **no generated parser and no other lock**.
- **The corrections that hold it at 60%.**
  - **NOT unconditionally orthogonal (INC-4 overstated).** T6 (`@host` survives `regen∘regen` on
    `bbnf.bbnf`) inherits the base-case wall — `bbnf.bbnf` does not parse yet — and the bbnf-
    subsystem totality% floor cannot be *counted* until it does. 7C is orthogonal for
    **T1/T2/T3/T5**, NOT for **T4/T6**.
  - **T4 is coupled to base-case fix (ii).** `@{}` is a **hard parse reject** (`parse_atom` has
    no `@` arm — fact 4-ii), not merely a runtime `BBNF-UNSUPPORTED` classification. T4 needs the
    RD front-end to *parse* `@{}` **and** lower `RuntimeConstructKind::HostCapture`. This is the
    same wall #2 the ladder needs — a shared fix, cross-pollinated.
  - **The typed-lattice half is LOCK-1-coupled (XL-3).** T1 (typed signature into inference) and
    T5 (tier off `&mut Ctx`, zero-cost re-verify) require LOCK-1's solver to admit a typed leaf.
    The "most independently viable / needs no other lock" verdict holds only for **T2 + T4**;
    split the claim.
  - **CSS floor presently UNMET.** `@{}` HostCapture is `BBNF-UNSUPPORTED` yet `color.bbnf:220` /
    `values.bbnf:69` depend on it — the CSS totality% is a paper number until T4 ships.
  - **Zero prototype evidence.** No probe exercised 7C. Its "most viable / one-file cure" claims
    are SPEC_ONLY — a gap for a family this cheap to probe (delete one heuristic, run check-json).
  - **CONTEXT-tier zero-cost (Q2) unverified** — no CONTEXT host fn exists (usage=0); the
    `&mut Ctx` parse-time-only invariant is asserted-per-subsystem, run for none. T3 (TOML
    `push_path`) is the first real test, unbuilt.
- **Open gaps.** (a) T6 + bbnf-floor share the base-case wall. (b) T4 needs `@{}` parsing (fix
  ii). (c) T1/T5 are LOCK-1 consumers. (d) no prototype for T1/T2/T5. (e) Q2 unverified. (f)
  totality% counter (leaves × branches) specified but unbuilt — all published per-subsystem
  numbers are projected, none measured. (g) T5 planted-breach falsification undone (a gate that
  has never tripped is not yet proven able to fail).

### 7D — META-CIRCULAR-ORACLE — ADVANCED — 60%

- **Convergence 60%** — critics' floor is CRIT-2's 58%; **raised to 60% with a named reason
  CRIT-2 erred low**: its 58% rested on byte-421 CONFIRMED (the bbnf edge walled at the *first*
  `->`). That is false (fact 3) — the bbnf edge is walled only at byte 662 behind 3 small fixes,
  so bbnf-edge feasibility is materially better than CRIT-2 scored. Held below CRIT-1's 62%
  because the emit wall (OPEN-1) is unprobed and the "subsumes 7A" relation is currently vacuous.
- **What is sound (the only family with a reproducing measured result).** PROTO-2 ran a real
  paired cold oracle over 51 patterns: structural parity **0.7647** (39/51; **0.80 / 40** under
  `Empty ≡ Concat([])` normalization), membership **0.8235** (42/51), perf GEN/HAND **1.77–1.81×
  slower**. **Neither promotion gate fires → ORACLE-ONLY** — resolving **INC-2 toward 7D** on the
  one edge where the contest is real. The 12-row divergence ledger is exactly the "checkable spec
  of the hand parser" the family promises, delivered independent of any cutover. ORACLE-ONLY as a
  lawful terminal is well-argued and Y.13-compliant (CI-consumed). Its own researcher caught the
  §0 fact-4 error (fact 5) — verified true. This is the **POSTURE winner** on the SCOPE×POSTURE
  plane.
- **The corrections that hold it at 60%.**
  - **bbnf edge behind the base-case wall.** ORACLE-ONLY for bbnf is reachable only after the 3
    parser fixes AND the unprobed emit wall (OPEN-1) — a weaker bar than promote-to-production but
    still the wall.
  - **The regex ledger row is RED and the parity headline is soft.** 9 of 12 divergences are
    **one fixable `host::lower` bug class** (bounded `{n,m}` reject) — parity 0.76 measures
    **LOCK-5's incomplete fold (XL-2)**, not an oracle verdict. The **stable** verdict is perf
    (1.8×). Report "regex ships now *as a red ledger row*," and the parity-moving fix lives
    **entirely inside LOCK-5's `host.rs`**.
  - **"7D subsumes 7A" is vacuous on current evidence** — nothing promotes, so the fixpoint
    corollary never activates. (Resolved by OPEN-2: decouple fixpoint from promotion.)
  - **≡ needs `Empty`-normalization codified** before parity% is quotable (from PROTO-2); the
    `Hand = parse-then-resolve_refs-then-validate` vs `Gen = pure parse` pivot must run shared
    semantic passes on both or neither (stated, not implemented).
  - **Q3 canonical regex `.bbnf` awaits LOCK-5** — three divergent regex grammars exist
    (`grammar/misc/regex.bbnf` 1006 B vs `parse-that/rust/bootstrap/regex.bbnf` vs an inline test
    copy); the oracle is undefined until LOCK-5 picks one. Named, not closed (XL-6).
- **Open gaps.** (a) bbnf edge on OPEN-1. (b) regex ledger red (parity 0.76/0.80, perf 0.55×);
  promotion may never fire (honestly named). (c) ≡ Empty-normalization uncodified. (d) canonical
  regex `.bbnf` LOCK-5-blocked. (e) grammar-driven corpus sampler unbuilt — parity is over the
  committed 51 only.

---

## THE INCOMPATIBILITY MAP — status after pass 1

- **INC-1 · 7A ⊗ 7B (SCOPE contradiction over `ir/src/lib.rs`)** — SOUND. Moot now that 7B is
  BANKED; if Gap B un-banks via LOCK-3, INC-1 re-activates as a LOCK-3⊗LOCK-7 scope question.
- **INC-2 · 7B ⊗ 7D (POSTURE polarity)** — **DECIDED BY MEASUREMENT toward 7D** (PROTO-2, the one
  decidable edge). Elevated from "convergence note" to measured resolution. Drives the 7B BANK.
- **INC-3 · 7A(retire-seed) ⊗ 7D** — SOUND and narrow. 7A-default ⊆ 7D + fixpoint gate; the
  retirement variant is banked with the rest of the seed-disposition posture.
- **INC-4 · 7C orthogonal to all three** — **CORRECTED: false for T4/T6.** 7C is orthogonal for
  T1/T2/T3/T5, coupled at T4 (shares wall #2) and T6 (base-case wall). 7C's stratum-2↔3 typed
  seam remains the kernel-carve-out boundary the surviving ladder posture (7D) consumes (XP-3).

**Lawful gestalt shape (unchanged, hardened):** **7C ships regardless** (the orthogonal
mechanism + the totality% metric every ladder reports against) **plus 7D as the single ladder
posture** (oracle-first, ORACLE-ONLY lawful terminal), **with 7A's fixpoint law grafted onto 7D**
(OPEN-2) and **7B banked** (its scope questions homed in LOCK-3).

---

## CROSS-LOCK LEDGER (carried, owed to GESTALT)

- **XL-1 · `ir.bbnf` double-homed 7B-Gap-B ⊗ LOCK-3 3C.** Conditional on a LOCK-3 verdict not
  named as a gate. Owed: a named supersession or a joint family.
- **XL-2 · regex oracle double-homed 7D ⊗ LOCK-5.** PROTO-2's 0.76 parity measures LOCK-5's
  incomplete `host::lower` fold (bounded `{n,m}`), surfaced through a LOCK-7 harness. The
  parity-moving fix is entirely inside LOCK-5. Adjudicate: bootstrap-verification deliverable
  (LOCK-7) vs functor-round-trip deliverable (LOCK-5)?
- **XL-3 · 7C typed leaf ⊗ LOCK-1.** T1/T5 are LOCK-1 consumers; only T2/T4 are lock-independent.
- **XL-4 · GN-0 archives the feasibility evidence (LARGEST GAP).** The working legacy self-host +
  the 21557-LOC generated `bbnf.rs` + the reproducibility fixpoint gate all live in the `crates/`
  tree GN-0 archives; SPEC §1's port-organ list omits them; skinny's ported xtask self-hosts
  nothing. **No family names a GN-0/GN-1 rescue-or-re-author gate as born-RED.** This is the
  supersession the whole lock depends on and it is currently a footnote. Owed born-RED.
- **XL-5 · LOCK-9 9C SCANNER-AS-DFA consumes LOCK-7's bootstrapped regex** — which PROTO-2
  measured 1.8× slower / 9-of-51 incomplete. A consumer edge that should be born-RED in the omega
  DAG.
- **XL-6 · 7A/7B/7D are net-new-vs-GN tranches** (the GN codex omits bootstrap entirely); lawful
  only if declared as named additions. 7C re-cuts GN-5 (well). Owed a declaration.

---

## PASS-2 DIRECTIVES

### What research must answer (per family)

- **7A.** (R-7A-1) Re-derive the seed_LOC headline number on the *skinny* tree (hand parser 2142
  + regex fold 524 + the codegen kernel S₀ touches) and state the trade vs a total self-host —
  the family's reason to exist is currently un-numbered. (R-7A-2) Resolve Q2 twin-ladder: one
  `fixpoint` xtask over bbnf(skinny/tape)+regex(parse-that/combinator), or two — and what that
  does to the minimal-surface metric. (R-7A-3) Cost the GN-0/GN-1 rescue (XL-4): is the legacy
  `bbnf.rs`/fixpoint gate a RESCUE into GN-1 or a re-author? Name the born-RED gate.
- **7B (BANKED — research only to test un-banking).** (R-7B-1) Resolve XL-1 with LOCK-3: does
  LOCK-3 pick 3C? If yes, draft the joint LOCK-3⊗LOCK-7 Gap-B family; if no, 7B stays banked.
  (R-7B-2) The Gap-C pure-structure-scoping question — can control-flow/dispatch/tape-assembly be
  expressed without a borrow checker, with a *named* failure signature for kernel-escape-into-
  control-flow? A research answer, not a route, unless it converges.
- **7C.** (R-7C-1) Split the independence claim in the spec: T2/T4 lock-free, T1/T5 LOCK-1
  consumers (XL-3). (R-7C-2) Specify the totality% counter (leaves × branches partition) and the
  floor-gate as a sibling of GN-5.W3 `check-name-gates`, with the planted-breach falsification.
  (R-7C-3) Confirm the CONTEXT-tier zero-cost invariant is testable via TOML `push_path` before
  T3 is scheduled.
- **7D.** (R-7D-1) Codify ≡ with `Empty ≡ Concat([])` normalization and the shared-semantic-pass
  pivot (`resolve_refs`/`validate` on both or neither) — pin the equivalence-relation-id before
  any parity% is quoted. (R-7D-2) Consume LOCK-5 for the canonical regex `.bbnf` (XL-2/XL-6);
  name the dependency edge. (R-7D-3) Draft the promotion ledger schema with a **totality% column
  from 7C's counter** (XP-1).

### What to prototype (pass 2), in priority order

1. **OPEN-1 — the EMIT/lowering wall (highest value, the genuine make-or-break).** Land the THREE
   bounded parser fixes (fact 4: `?w`→`->` attachment in `parse_seq:1366`; `@{}` atom in
   `parse_atom:1495`; `|params|` closure binding) — then attempt `emit_from_source("bbnf",
   bbnf.bbnf)` and report: does the general-descent emitter LOWER bbnf's recursive-alternation
   tower + closures + `@import` to a *working* generated parser, or does the wall move downstream
   into lowering — and at which construct? **This is the one thing no prototype has tested; every
   ladder family rests on it.** Binary outcome decides whether 7A/7D's bbnf edge reaches
   ORACLE-ONLY at all.
2. **OPEN-2 — fixpoint-over-the-oracle (the 7A→7D graft).** Prototype byte-identical `regen∘regen`
   as a property of the ORACLE-ONLY generated parser, decoupled from promotion (needs neither
   perf≥1.0 nor parity=1.0 — only that the generated artifact re-emits itself byte-identically).
   This keeps 7A's distinctive fixpoint LAW alive under the 7D collapse and gives 7D a live
   fixpoint gate its "subsumes 7A" claim currently lacks. Reuse `regen.rs:37 check_target`.
3. **7C-T2 (trivial, kills a SPEC_ONLY gap).** Delete `has_parse_time_host_decode`
   (`passes/src/lib.rs:794-798`) → typed-tier registry lookup; run `cargo xtask check-json`
   bit-for-bit. Makes 7C's "most viable / one-file cure" a RAN result instead of a claim.
4. **7C-T4 = base-case fix (ii), cross-pollinated.** Parse `@{}` in `parse_atom` + lower
   `RuntimeConstructKind::HostCapture`; CSS `colorFunction`/`urlFunction` stop emitting
   `BBNF-UNSUPPORTED`; CSS regen bit-for-bit + measured totality% delta. Discharges the CSS floor
   and delivers wall #2 for the ladder in the same stroke.
5. **7D regex ledger, corrected.** Re-run PROTO-2 with ≡ Empty-normalized (parity 0.80/40),
   record it as the first COMPLETE ledger row, and file the 3 substantive divergences (bounded
   `{n,m}`, `.` metachar, `(?s)`) as LOCK-5 `host.rs` obligations (XL-2), not LOCK-7 bugs.

### Cross-pollination (named, both families developed)

- **XP-1 · 7C totality% counter → 7D promotion ledger.** 7C (60%) supplies the totality% metric;
  add it as a column to 7D's (60%) ledger. Justified: INC-4 already names 7C's % as the metric
  every ladder reports against.
- **XP-2 · 7A fixpoint law → 7D oracle (= OPEN-2).** 7A (45%) + 7D (60%); the graft is "fixpoint
  over the oracle artifact." Keeps 7A's non-redundant core alive; gives 7D a live fixpoint gate.
- **XP-3 · 7C stratum-2↔3 typed seam = the surviving ladder's kernel-carve-out boundary.** 7B's
  "kernels behind a typed call interface" IS 7C's seam; 7D (the survivor) consumes it as its
  primitive line. Justified by INC-4.

### What stays WITHHELD from researchers (pass 2)

- **The REGISTRY convergence %s and family statuses** (standard protocol — no family allegiance).
- **The 7B-BANK verdict and the crowding redirect.** 7B-owned research must be dispatched as
  open scope-axis questions (R-7B-1/2), NOT told the family is banked — else it cannot honestly
  test the un-banking condition (LOCK-3 picking 3C).
- **The pre-committed answer to OPEN-1.** The emit-wall prototype must run genuinely RED-unknown;
  do not seed it with "the legacy 21557-LOC `bbnf.rs` proves it lowered" — that was the OLD
  emitter, a feasibility hint, not a verdict for the skinny general-descent path.
- **PROTO-1's byte-421 numbers.** Superseded and non-reproducing; pass 2 must carry only the
  corrected fact 3/4 (byte 662, three fixes), never re-quote 421.
