# LOCK-7 — GRAMMAR TOTALITY & BOOTSTRAP — pass-2 CRIT-1 (WITHIN-LOCK adversary)

Critic 1 of 2. Weight: **within-lock soundness** — internal cogency of each family spec, and
whether the pass-2 prototypes actually stressed the *sharpest* uncertainty rather than a softer
neighbour. CROSS-LOCK (XL-A…XL-H) consumed as binding fact; challenges routed to GESTALT, not
re-litigated here. Base law (XL-H) respected: every disk claim below was re-greped live on
skinny@HEAD (`c93fc4314`, `merge-base --is-ancestor ce51c36fc HEAD` = YES). This file supersedes the
stale 20:20 CRIT-1 (written against the 19:33 re-cut, before UF-2/UF-3 touched disk).

**Disk re-verification performed this pass (load-bearing facts the spec leans on):**
- `general_descent/{emit,mod,regex_first,view}.rs` = **1206 LOC** (452/93/230/431) — CONFIRMED.
- `has_parse_time_host_decode` at `passes/src/lib.rs:794` — CONFIRMED (a live recursive
  substring-style predicate, the latent name-gate 7C targets).
- **`ExprKind` (`ir/src/lib.rs:234`) = {Seq, Alt, Repeat, Optional, Literal, Regex, Ref, Annotation,
  Project, Discard} — NO `Host` variant.** The only `Host*` token in the crate is
  `cost.rs:255 HostFnParseTime` (a cost enum, not an arena node). This single fact is the hinge of my
  sharpest 7C finding (§C-3).

---

## C-0 — THE HEADLINE: A PROBE RAN 24 MIN AFTER SYNTHESIS FROZE AND REFUTED A LOAD-BEARING PREMISE IN FOUR PLACES

`FAMILY-SPECS.md` froze **22:30**. `PROTO-2-UF3.md` (the fixpoint-on-the-regex-artifact probe) is
timestamped **22:54** with verdict:

> **RAN_MEASURED — the regen path is SEVERED; the fixpoint gate CANNOT run on the regex artifact.**
> The documented regenerator does not reproduce `generated.rs`, it **truncates it to 0 bytes** …
> FAMILY-SPECS §0/§4's premise "the regex parser self-emits TODAY" (lines 166, 172, 329) is
> **empirically false against HEAD.**

The mechanism is named and hard: B2 (`6142387f1`) deleted the `bbnf_derive` proc-macro;
`bootstrap/src/lib.rs` is now a pure re-export; `cargo expand -p regex-bootstrap` carries no grammar,
so `bootstrap-regex.sh`'s anchor line never appears → the committed `generated.rs` is **truncated to
the empty-file hash**. The alternate (`cargo xtask regen`) has a 9-grammar manifest with **no regex
row** and emits none of the parser's symbols. **The committed `parse_that::regex::generated` is a
frozen fossil of a decommissioned pipeline — no path, live or dormant, from `regex.bbnf` to
`generated.rs` exists in the current tree.**

This is not a footnote. The FAMILY-SPECS carries the refuted premise **"runnable today on the regex
oracle artifact"** as a live, weight-bearing claim in FOUR places:

1. **§0.6** — "runnable TODAY on the regex oracle artifact (the regex parser self-emits — UF-3)."
2. **§1 gate (c)** — "**runnable TODAY on the regex oracle artifact** (the regex parser self-emits)."
3. **§4 promotion ledger** — "The `fixpoint` column is fillable via UF-3 without LOCK-5's engine — the
   regex row is one column short of complete."
4. **§7 sharpest-uncertainty brief 2** — "the regex parser self-emits today (`generated.rs`) … Fills
   both columns; discharges 7A's SPEC_ONLY reduction."

**All four are now false.** This is exactly the **BREACH-1 stale-synthesis** failure the pass-2
REGISTRY invoked against the 15:12 FAMILY-SPECS for narrating PROTO-3/7C-T2 as "open" after they ran
— except here it is *self-inflicted by the current re-cut*: §7 proposes UF-3 as a future brief while
§0/§1/§4 already assert its **outcome** as fact. A brief and its pre-committed answer cannot both be
honest; the re-cut asserted the reduction it was simultaneously proposing to test. **GESTALT must
fold UF-3 as it folded PROTO-3: synthesis moves current, not stale.** Downstream: 7A's one
non-redundant runnable asset is INERT (C-1), and 7D's "complete ledger row save one fillable column"
is now structurally incomplete (C-4).

---

## C-1 — 7A · STAGED-BOOTSTRAP — CONVERGENCE **30%** (was 40; −10, named reason: UF-3)

**What is genuinely sound (preserve into the graft).** The fixpoint LAW — byte-identical
`regen∘regen`, no 2-cycle, blake3 provenance — is a real non-redundant idea; killing the `88s→0.5s`
legacy transplant is exactly the constitution's perf-claim discipline (research-A §1.67 verifies it
non-resident on skinny); the non-vacuity protocol (a planted input-mutation that MUST move the
`derived` region, partitioned from `scaffold`) is a gate engineered to be able to fail. The design is
not wrong. Its **reachability is gone.**

**The elegant-reduction trap, now sprung.** The whole 40% rested on ONE survival-test answer. §1's
own closing line: **"The one fixpoint signal runnable WITHOUT LOCK-5's engine is UF-3."** UF-3 ran and
returned: **that signal is not ownable because it cannot be produced.** PROTO-2-UF3 states it directly
— "This **strengthens the case that 7A RETIRES**: the fixpoint law's only engine-free artifact is
inert." The FAMILY-SPECS' explicit survival test ("name ONE thing 7A owns post-graft that 7C+7D do
not, or 7A RETIRES next pass") now has an *empirical* answer: **nothing it owns can run.**

**The residual "fixpoint law is live for the bbnf-lang emitter" escape is consumer-less for 7A's
purpose.** PROTO-2-UF3 flags that `cargo xtask regen --check` over the 9 live bbnf-lang grammars IS a
real committed==regen fixpoint — but "that certifies the bbnf-lang xtask emitter, a *different
artifact* … and **cannot reproduce the committed regex bytes. It does not fill 7D's regex row.**" The
law has an artifact it can run on (the xtask emitter) and an artifact the ledger needs (the
regex/bbnf self-host), and **these are disjoint.** Sound law, empty ledger consumer (Y.13, at the
level of *which artifact the gate actually gates*).

**Exact open gaps (7A):**
1. **The regen edge is SEVERED (UF-3, RAN).** No path `regex.bbnf`→`generated.rs`; `bootstrap-regex.sh`
   truncates to 0 bytes; xtask has no regex row. 7A's SPEC_ONLY reduction (OPEN-2/XP-2) is **NOT
   discharged and NOT dischargeable on the current tree.**
2. **Survival test FAILED empirically** — after the graft, 7A owns nothing 7C+7D do not *and can run*;
   the single claimed exception (UF-3) is inert. → RETIRE candidate for pass-3, not a 40%-BANKED route.
3. **The building-seed inflation datum is UN-RUN** — argued from an in-repo *analogy* (parse-that
   regex `generated.rs` 2293 + `host.rs` 524), never measured on an actual bbnf building-seed; the
   spec concedes this. The 30% must not lean on it as measured.
4. **The bbnf self-emit route** remains BLOCKED cross-repo on LOCK-5's `match_at` (INC-5 seam i).
5. **Fixpoint SUBJECT decided but never exercised** — neither sub-gate (bytes / GrammarIr floor) has
   RUN on any bbnf artifact, and the one artifact it could run on (regex) is severed. SPEC_ONLY.

---

## C-2 — 7B · TOTAL-SELF-HOST — CONVERGENCE **32%** (hold; no in-lock advance this pass)

**What is sound.** The plane decomposition (A front-end / B IR / D emitter-templates) with distinct
verdicts is clean; the third-Futamura / TREE-META framing (research-priorart) is correct and rare;
the named kernel-escape signature (the speculative-cursor reborrow `let cursor = &mut _probe` inside
`(|| {…})()` + the progress-guard `if _probe == _before { break; }`) is a precise, disk-located
statement of what a grammar-plane emitter cannot emit. As a **research answer** it is strong.

**The twice-un-run ratio (the sharpest 7B uncertainty went untested AGAIN).** The maximal-pole verdict
rests entirely on the **DATA≈50% / WALK-FOLD≈20% / ESCAPE≈21%** split — a **static line-count read of
`emit.rs` (research-B §3), NEVER RUN.** §2 concedes every part: shares sum to **~91% (≈9% of `emit.rs`
unclassified — "name the residue or the ratio is soft")**; the E0499/E0503 borrow-fail is
**"*checkable* … but was asserted, not shown as a RAN compile-fail — the twice-un-run experiment."**
INC-3's running variant was deprioritized a second consecutive pass. The family's one quantitative
claim ("generator becomes mostly data" ⇒ ~70% liftable) is an *unmeasured estimate with a 9% hole*,
and the number that decides BLOCKED-candidate (21% ESCAPE) is its softest part. **Did the prototype
test the sharpest uncertainty? No — it read it.**

**Consumer-less unless LOCK-3 adopts 3C (correctly bounded).** Plane-B is "Conditional on LOCK-3
picking 3C; else consumer-less (Y.13)"; the lawful-today fallback (ir.bbnf as concrete syntax of a
serialized-IR text + a `check-ir` round-trip oracle) IS a genuine consumer and is the honest floor.
Handled well — but it means 7B has **no in-lock RUN advancing it this pass**: a scope question
deferred to LOCK-3 plus an un-run ratio.

**Exact open gaps (7B):**
1. **The 50/20/21 ratio is UN-RUN** (static line-count read); ~9% of `emit.rs` unclassified; the E0499
   escape asserted, never shown as a RAN compile-fail. → RUN the blob-conversion + compile-fail, or
   demote "MEASURED ~21% ESCAPE" to "static-read conjecture."
2. **Plane-A** BLOCKED cross-repo — same leaf primitive as 7A/7D-bbnf (INC-5 seam i); UF-1 bounds it
   (leaf-floor + front-end + 1 misfire) but does not unblock it.
3. **Plane-B** LOCK-3-conditional (XL-B); consumer-less (Y.13) if LOCK-3 declines 3C → 7B RETIRES.
4. **Plane-D maximal pole** is a BLOCKED-candidate on a missing primitive equal in difficulty ("it
   *is* authoring a VM"); the "escape relocates into LOCK-4's core, not vanishes" caveat (R6) is
   unverified — that LOCK-4's *existing* combinator core covers exactly probe/commit +
   many-with-progress-guard + mutable-cursor threading is asserted, not shown.

---

## C-3 — 7C · HOST-FUNCTION-LATTICE — CONVERGENCE **54%** (was 57; −3, named reason: the flagship is constant-1.0-on-real-corpus)

**What genuinely advanced (credit it).** UF-2 RAN (22:44) and independently reproduced the prior
PROTO-1 run **byte-for-byte** — json 15 / sheets 39 / bbnf-closure 53 rules, censuses sensible,
build-side only (no emitter, no leaf evaluator, no self-emit). The counter is NOT SPEC_ONLY; the
harness executes; the tier design (keyed on whether a signature threads `&mut Ctx`, a pluggable
decision point, NOT a name test) is the correct anti-name-gate architecture. Killing
`has_parse_time_host_decode` (disk-confirmed at `:794`, a live latent name-gate) is right. 7C remains
the survivor.

**The sharpest uncertainty was NOT the one UF-2 tested — and the one it dodged is load-bearing.** The
brief asked UF-2 to "demonstrate a NON-VACUOUS floor." UF-2's demonstration is a **SYNTHETIC Tier-2
injection** (`HOST_SENTINEL="host"` overwriting one plain leaf → json 1.0000 → 0.9825). On **every
real grammar the counter reads `totality% = 1.0000 EXACTLY`** (json, sheets, bbnf-closure). I
disk-verified *why this is structural, not incidental*: **`ExprKind` has no `Host` variant** (§top),
so Tier-2 is **unreachable on any grammar that can be built.** The metric is a **tautological constant
1.0 on the real corpus** — it cannot discriminate, cannot fall, cannot fail, until `ExprKind::Host`
exists. This is the *gate-that-cannot-fail* mode dressed as a passing gate:

- The number "**every ladder reports against**" (§3, feeding 7D's `totality_pct`, XP-1) is a constant
  `1.0` by construction of the type. A ledger column provably `1.0` for every current row carries
  **zero information** this pass — a placeholder that *looks* measured.
- The floor is "exercised" only by a node the IR cannot represent yet. UF-2's own note (patch line
  250) admits it: the real corpus reads 1.0; non-vacuity needs the synthetic injection. A metric whose
  discriminating input is *un-representable in the IR* is **consumer-less at the level that matters**
  (Y.13): its only real consumer — a non-1.0 reading — requires the un-built `ExprKind::Host` (7C-T4).

**Chicken-and-egg, named.** A meaningful (non-1.0) totality% requires `ExprKind::Host`; that node is
7C-T4, **UNBUILT**. "Runnable today" and "informative today" are different claims — UF-2 proved the
first, not the second.

**T2 (PROTO-2) proved a NO-OP and delivered a LATERAL move, not the construction (carried).**
`has_parse_time_host_decode` is a **dead** name-gate — `"ws".contains("host")` is always false, and
the *inverted* predicate is ALSO byte-identical, so eager-tape selection never flowed from it. Honest
hygiene, but the patch keyed on `value.is_some()` (a shape-sniff on an always-empty slot), **NOT** the
typed `ExprKind::Host` node. The *refutation* is discharged; the *construction* the lattice needs is
un-built. This caps "ships value now": what ships is a RUNNABLE metric that reads a constant.

**Exact open gaps (7C):**
1. **The flagship reads `1.0` EXACTLY on all real grammars — non-1.0 requires a SYNTHETIC node
   (`ExprKind::Host`) absent from the IR.** The metric cannot discriminate or fall on real input until
   7C-T4 lands the typed node. The survivor's central un-discharged item; the 57→54 mark rests on it.
2. **The real `ExprKind::Host` typed node + `parse_atom:1495` `@{` arm (T4) is UNBUILT** — T2 was a
   shape-sniff on an empty slot; the actual HOST discriminator does not exist, so the HOST column is
   empty by construction.
3. **CONTEXT-tier zero-cost invariant un-round-tripped** — `resolve_refs∘resolve_refs` no-op +
   `pretty()` unchanged + TOML `push_path` residue=0 proposed, none RUN (constitution clause v owed).
4. **CSS floor UNMET until T4** (`@{}` is `BBNF-UNSUPPORTED`; `color.bbnf:220`/`values.bbnf:69` depend
   on it) — the counter's CSS column is structurally incomplete now.
5. **Floor ∈ {0,1,2} is LOCK-1-pending** (XL-3); T1/T5 (typed `HostFnSig` into inference; tier-off-
   `&mut Ctx`) are LOCK-1 consumers, unbuilt.

---

## C-4 — 7D · META-CIRCULAR-ORACLE — CONVERGENCE **48%** (was 51; −3, named reasons: fixpoint column now unfillable + charitable-framing on 52/53)

**Genuinely the strongest measured evidence in the lock.** `ORACLE_REL_V1 = R1∘R2∘R3` is **pinned AND
RAN as code**; all four planted mutations trip — the oracle is *proven able to fail*, the one gate in
this lock demonstrated non-vacuous on a real artifact. The regex row is honestly partitioned (9×
`host.rs` bounded-`{n,m}` → LOCK-5; 3× `regex.bbnf` verdict → oracle bank). ORACLE-ONLY as a lawful
terminal (CPython pegen) is the right posture. Best-evidenced family; the negative is honest.

**The "complete row save one fillable column" claim is now FALSE (C-0 → 7D).** §4: the regex row is
"one column short of complete" and "the `fixpoint` column is **fillable** via UF-3 without LOCK-5's
engine." UF-3 refuted exactly this: the fixpoint column is **NOT fillable by the current tree** — the
regen edge is severed, so the column is **structurally unfillable** until a regen edge is rebuilt
(itself net-new work re-establishing the derive path B2 deleted). 7D's concrete deliverable headline —
"the first COMPLETE promotion-ledger row" — is **not one column from complete; it is permanently
incomplete** on the current tree: parity 0.7759 · perf 1.77× · **fixpoint = STRUCTURALLY N/A** ·
BLOCKED. A weaker deliverable than the spec presents.

**Framing smell: the 52/53 headline is the CHARITABLE reading over a HAND-NORMALIZED input.** UF-1
reports **STRICT 25/53** and **CHARITABLE 52/53**; the FAMILY-SPECS leads everywhere with "branch
skeleton SOUND (52/53)" and mentions 25/53 only parenthetically. Two suppressions: (a) 52/53 depends
on treating the lowering's `Annotation(?w)→Seq([])` rewrite as an ε-leaf — a *reading choice*; the
strict/charitable delta is *exactly* the 27 `?w`-bearing rules; (b) the oracle could not ingest the
raw closure — `parse_grammar` fails at **byte 2343** (`?w`-then-`->`) — so UF-1 **manually leaf-elided
2 sites** to get a parseable input. So **"bbnf IS structurally Tier-R (52/53)"** is: *the charitable
reading, over an input the production parser rejects and a probe hand-normalized.* This is the same
"report the highest of several sub-gate figures" pattern flagged on the regex parity row (0.7931
Empty-normed led over 0.7759 strict) — **same family, same pass, same smell.** Report both numbers
with equal weight + the normalization caveat, or "SOUND" is a framing artifact.

**The bbnf oracle twin is SPEC_ONLY on a convenient tautology.** Only the *regex* `ORACLE_REL_V1` ran.
The bbnf twin `OracleRel-bbnf-v1` has **not been run**, and its cited soundness ("`Empty ≡ Concat([])`
satisfied at zero cost by construction because there is no `ExprKind::Empty`") is a **tautology of the
current type**, disk-confirmed (no `Empty` variant), not a *tested* equivalence. The bbnf leg's
positive signal is UF-1's leaf-blind branch comparison, not this un-run relation.

**The 0.80 gate provenance is UNCITED (latent gate-tuning risk).** The threshold deciding
ORACLE-ONLY-vs-promote is unsourced; robust *here* only because all three parity metrics clear below
it, but an unsourced promotion threshold is a name-gate-adjacent decision point a future pass could
tune. Cite or derive it.

**Exact open gaps (7D):**
1. **`fixpoint` column STRUCTURALLY UNFILLABLE, not "one column short" (UF-3)** — the "complete ledger
   row" deliverable is permanently incomplete on the current tree.
2. **52/53 is the CHARITABLE reading (STRICT 25/53) over a hand-normalized non-parseable input** (2
   sites elided at byte 2343). Report both + caveat, or "branch skeleton SOUND" is a framing artifact.
3. **The bbnf oracle twin `OracleRel-bbnf-v1` is SPEC_ONLY** — never run; base-case soundness is a
   tautology of the missing `Empty` variant.
4. **bbnf production/leaf tiers BLOCKED cross-repo** on LOCK-5's `match_at` (INC-5 seam i).
5. **3 `regex.bbnf` verdicts LOCK-5-substrate-bound**; **0.80 gate provenance UNCITED**; grammar-driven
   corpus sampler UNBUILT (parity over the committed 51/58 only — generality unmeasured).
6. **L1 structural oracle proven at 52/53 but not closed to 53/53** (the `bool_value` materialization
   misfire — a single named in-lock bug) nor wired as a CI gate; the front-end `?w`-attach fix lives
   only in the PROTO-3/UF-2 worktrees — a worktree reconciliation is owed before any of this is landed.

---

## C-5 — CONSTITUTION & GESTALT-OWED FINDINGS (within-lock evidence)

1. **Constitution clause (i) vs HEAD — the flagship self-host artifact is INERT.** Clause (i): "the
   regex grammar IS bootstrapped and bbnf-defined." UF-3 measured the regex bootstrap **decommissioned
   in B2** — the committed `generated.rs` is "a frozen artifact of a decommissioned pipeline" with **no
   live regen edge in either repo.** The ONE existing self-host artifact the constitution points to is
   a fossil. GESTALT-Ω must carry: **"the regex grammar's committed parser exists but its bootstrap
   edge is severed (B2 `6142387f1`); re-establishing it is net-new work, not a rescue."**
2. **Zero-runtime-cost (clause v)** — the CONTEXT-tier invariant (7C gap 3) is asserted testable, never
   run. An undischarged invariant the type-plane law requires before the CONTEXT tier is scheduled.
3. **Consumer-less-substrate (Y.13) recurs three times, only one benign:** 7C's totality% metric
   (discriminating consumer = a non-1.0 reading = an un-built node); 7A's fixpoint law (ledger consumer
   = a self-emitting artifact that is severed); 7B Plane-B (consumer = LOCK-3 adopting 3C). Only 7B's is
   honestly bounded with a lawful fallback; the first two are dressed as delivered.
4. **The "runnable TODAY without LOCK-5's engine" motif is the lock's recurring over-claim.** Of the
   three UF probes billed as engine-free wins: **UF-1** ran but its headline is a charitable reading
   over a non-parseable input; **UF-2** ran but reads a constant 1.0 on real input; **UF-3** ran and
   found its artifact severed. Each returns a result softer or harder-negative than the framing. The %s
   should track the *result*, not the runnability.

---

## SUMMARY (CRIT-1, within-lock)

The lock's terminal finding — the bbnf leg cannot converge inside LOCK-7 alone (LOCK-5 `match_at`,
INC-5) — is **correct; I do not challenge it.** My objections are to the four families' *internal*
convergence claims, sharpened by three probes that ran AFTER the 22:30 re-cut:

- **7A 30%** (was 40): UF-3 refuted the ONE thing 7A owns post-graft — the regex artifact does not
  self-emit; the regen edge is severed. Survival test fails empirically. RETIRE candidate.
- **7B 32%** (hold): no in-lock advance; the 50/20/21 ratio is the twice-un-run static read with a 9%
  hole; consumer-less unless LOCK-3 adopts 3C.
- **7C 54%** (was 57): the flagship counter RAN but reads `totality%=1.0` EXACTLY on every real grammar
  — Tier-2 is structurally unreachable (`ExprKind` has no `Host` variant, disk-verified), so the metric
  cannot discriminate on real input until the un-built typed node lands. Runnable ≠ informative. Still
  the survivor.
- **7D 48%** (was 51): strongest measured evidence, but the "complete ledger row save one fillable
  column" is now structurally incomplete (UF-3), the 52/53 headline is the charitable reading over a
  hand-normalized non-parseable input (strict 25/53), the bbnf oracle twin is SPEC_ONLY on a
  tautological base case, and the 0.80 gate is uncited.

**One demand of the GESTALT:** fold UF-3 as the registry folded PROTO-3 — "the regex parser self-emits
today" is dead in four spots, and with it 7A's non-redundant asset and 7D's complete-row claim. The
regex bootstrap, the constitution's flagship instance of grammar totality, is **inert against HEAD.**
