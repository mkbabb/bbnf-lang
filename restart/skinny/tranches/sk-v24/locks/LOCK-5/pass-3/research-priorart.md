# LOCK-5 — THE REGEX CONTINUUM — pass-3 prior-art research

Researcher pass 3. No family allegiance; REGISTRY unread (withheld from research per the
portfolio protocol). Charter = the LOCK-5 task statement + THE CONSTITUTION only. All local
paths verified by `grep`/`ls`/`Read`; all external claims carry a URL. This is a prior-art
sweep: what exists, what won/lost elsewhere for THIS problem class, the constraints our
evidence base already imposes, the named traps.

The problem class: **a regex engine reborn grammar-first** — regex grammar bbnf-defined and
BOOTSTRAPPED; a regex HIR truly isomorphic to bbnf's grammar HIR/IR (one type, or a lawful
functor); NFA→DFA→minimization→prefilter recast as solver problems (CSP/e-graph) *where
measured to win*; PROVE-10's prefilter tier, the Teddy tier, the lazy DFA folded; the
1148-LOC `classify.rs` re-derived or re-homed; the existing engine's tests + consumers as the
parity floor.

---

## 0. The substrate as it actually stands (measured, not asserted)

**The engine of record is `parse-that/rust/regex` = `bbnf-regex`, 6742 src LOC** (R3), NOT the
skinny vendored copy. **VERIFIED trap:** `skinny/crates/parse-that-regex/` is a *partial* vendor
— `lib.rs` + `integration/` + `number/` + `unicode/` only; it carries `RegexError`,
`StringMode`, `NumberSpan`, the Eisel–Lemire number path — but **no `hir/`, no `automata/`, no
`classify.rs`, no `egraph/`**. The full engine (HIR parser, NFA, DFA+Hopcroft, classify, the
egraph) is vendored fresh by GN-1 (INDEX interlock GN-1.W3 ⇄ PT-1.W1: pins pre-PT-1, re-vendors
post-W1). LOCK-5 designs against the *parse-that* engine; the skinny crate is a number/string
seam, not the automata home.

**The two HIRs, side by side (the isomorphism question made concrete):**

| bbnf grammar `ExprKind` (`skinny/crates/ir/src/lib.rs:234`) | regex `Hir` (`parse-that/rust/regex/src/hir/mod.rs:216`) |
|---|---|
| `Seq(Vec<ExprId>)` | `Concat(Vec<Hir>)` |
| `Alt{branches,mode:AltMode}` (**mode = Dispatch \| ...**) | `Alternation(Vec<Hir>)` (**leftmost-first priority**) |
| `Repeat{body,min,max}` / `Optional` | `Repetition(Repetition)` |
| `Literal{bytes,case}` | `Literal(Vec<u8>)` |
| `Regex{pattern:String}` (**opaque escape hatch**) | — |
| `Ref{name,target}` / `Project` / `Discard` / `Annotation` | — |
| — | `Class(CharClass)` |
| — | `Look(Look)` (**zero-width assertions**) |
| — | `Group(Box<Hir>)` (transparent) |

The overlap is real (Concat/Seq, Repetition/Repeat, Literal/Literal) but the **deltas are the
whole design**: regex has `Class`+`Look`; bbnf has `Ref`+`Project`+`Discard`+`Annotation` and,
tellingly, `Regex{pattern:String}` — **today a grammar regex leaf is stored as an opaque string
and re-parsed by the standalone engine**, the exact seam LOCK-5 is chartered to dissolve
(5D "lower DIRECTLY into the grammar's own IR — no separate engine on the generator path").

**The e-graph already exists over regex HIR.** `parse-that/rust/regex/src/egraph/` = 1253 LOC:
`mod.rs`, `node.rs`, `translate.rs`, `cost.rs`, `saturation_cache.rs`, `rules/`
(repetition/union/flatten/superset/redundant). It does **HIR-level equality saturation**
(canonicalize/simplify the pattern), depending on the external `egraph`/`egraph-derive`/
`csp-solver` crates (the shared substrate the constitution points every lock at). **This is the
partial precedent for 5C** — candidate *generation* via saturation is BUILT at the HIR tier;
what is NOT built is saturation at the **automata** tier (NFA/DFA construction, minimization,
prefilter-tier selection). That gap is exactly where 5C is speculative and must be measured.

---

## 1. What WON elsewhere for this problem class (the SOTA the parity floor must respect)

**rust `regex-automata` is the reference SOTA** (Gallant / burntsushi). Its shape is a
**meta-engine that composes and never returns a wrong answer**: literal/prefilter → **lazy
(hybrid) DFA** → one-pass DFA → bounded backtracker → PikeVM; the meta engine picks per pattern
and falls back on a **quit byte / cache-exhaust bail** (`MatchError`, never a wrong answer).
[burntsushi regex-internals](https://burntsushi.net/regex-internals/);
[docs.rs regex_automata/hybrid](https://docs.rs/regex-automata/latest/regex_automata/hybrid/index.html)

- **Prefilter ladder that won:** memchr/Two-Way (single, rare-byte-ranked) → **Teddy** (multi,
  SIMD, 2–72 literals) → Aho-Corasick (tens of thousands). Prefilter runs on start-state entry;
  a hit triggers an *anchored* full-engine confirm at that offset only; literal extraction is
  exact (skip-engine) vs inexact (confirm), **order-preserving for leftmost-first**.
  This is PROVE-10's design verbatim, and it is the *productionized-elsewhere* pattern.
- **Lazy DFA that won:** builds ≤1 state per input byte (worst-case O(mn)), under a memory
  budget; on exhaustion it bails to a lower engine. **`LazyStateID` reserves tag bits**
  (unknown/dead/quit/start/match) — the compact-cache encoding to copy.
  [LazyStateID](https://shadow.github.io/docs/rust/regex_automata/hybrid/struct.LazyStateID.html)
- **Teddy / shufti / truffle lineage (Hyperscan, NSDI'19):** decompose regex into string + FA
  components; SIMD fast-path finds candidates, slow back-end confirms. shufti = low+high nibble
  `vqtbl`/`pshufb` LUT AND → byte-class membership in ≤4 ops; truffle extends to arbitrary
  256-char sets; Teddy = SIMD bucketed multi-literal.
  [Hyperscan NSDI'19](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf);
  [Teddy ICPP21](https://oaciss.uoregon.edu/icpp21/views/includes/files/pap205s4-file2.pdf)
- **NEON is the only vector plane on M-series** (R7 §0, settled law): no non-streaming
  SVE/SVE2, SME only in streaming mode with no byte-classification benefit. 16-B lanes, the
  `vshrn_n_u16 #4` movemask idiom. Lock the cost model to 128-bit NEON.
  [tzakharko m4-sme](https://github.com/tzakharko/m4-sme-exploration/blob/main/reports/01-sme-overview.md)

**Alternative automata construction — derivatives (relevant to 5A/5C, mixed verdict).**
Brzozowski derivatives build a **canonical, already-minimal DFA directly** from the regex, no
separate minimization pass; smart-constructors ARE the normal form (algebra-friendly, e-graph-
shaped). Antimirov *partial* derivatives give a smaller NFA without state-identification.
Owens–Reppy–Turon "*Regular-expression derivatives re-examined*" is the canonical practical
reference (derivatives handle **character classes and complement/intersection natively** — which
matters because regex `Hir::Class` + a grammar's `minus`/negation are hard for Thompson).
[Owens et al re-examined](https://www.khoury.northeastern.edu/home/turon/re-deriv.pdf);
[Antimirov (semantic-domain)](https://semantic-domain.blogspot.com/2013/11/antimirov-derivatives-for-regular.html);
["Yacc is dead" — derivatives for CFGs](https://arxiv.org/pdf/1010.5023)
**But:** production throughput engines (regex-automata) use Thompson NFA → lazy/subset, **not**
derivatives — derivatives are elegant and canonical yet lose on build/scan throughput at scale.
Verdict for us: derivatives are the *natural bridge for the unified-HIR + class/negation +
minimization-as-rewrite ambition*, but they are a **measured-or-negative-result axis**, never an
assumed win.

**e-graph / equality-saturation as the optimization substrate (5C's frame).** Saturation
represents *all* rewrite-equivalent forms non-destructively, then extracts a cost-minimal
representative; it is a proven paradigm for compiler/tensor/query optimization.
[eqsat dialect](https://arxiv.org/pdf/2505.09363);
[Tensat](https://arxiv.org/pdf/2101.01332). It is genuinely apt for **HIR canonicalization**
(already built here) and plausibly for **prefilter-tier *selection*** (extract the cheapest
legal accelerator). It is **unproven for DFA minimization** — see §3.

---

## 2. Constraints our own evidence base already imposes (binding, measured)

1. **PROVE-10 is the floor and it is CONFIRMED GO.** Start-state memchr prefilter + unanchored
   `Dfa::find`, **8.8–29.5× on prefilterable patterns, 0.97–1.01× controls, compile-neutral
   (O(256) derivation), 44/44 tests, 245-line additive, no @generated touched, no x86**. Scope
   cap (honest, PROVE-10 §5): **only 1/2/3-byte entry sets** are prefiltered (memchr tier);
   wider sets fall to `None`/parity — the Teddy/shufti tier is *named future work*, not done.
2. **The parity floor is behavioral + numeric:** 44/44 bbnf-regex tests; the DFA subtest oracle
   is currently **DARK** — `parse_that::regex::parser` E0432 (a pre-existing parse_that-side
   re-export drift; `hir::parser` not re-exported at `parse_that::regex`) blocks the whole
   `parse_that --test regex` bridge (49 dfa + 22 parser + nfa/utf8/…). Any LOCK-5 route that
   claims DFA correctness must **first restore this oracle** (PT-1.W4), not lean on a bench
   `assert_eq!`.
3. **Compile axis is a hard non-regression axis** (PROVE-10 §2b: 1.003× fastest). Any
   saturation/derivative/lazy machinery that inflates DFA-build time must pay for itself against
   this measured baseline, per pattern.
4. **No stable-port perf claims (SPEC §0.09, REFUTED).** `bbnf-regex` is **pure scalar +
   SmallVec — no `portable_simd`** (R3 §3). W1 speed comes from *memchr's own aarch64 NEON
   backend*; any wide tier speed must come from *real `vqtbl1q_u8` intrinsics*, measured paired,
   **never a shim asymmetry** (the two-arm-window self-deception is named law).
5. **No warm benches; cold paired same-binary N≥200; ratios certified, absolutes directional**
   (PROVE-10 measured under loadavg 11–17, so paired-and-same-window is mandatory).
6. **The type plane costs zero at runtime** (constitution v) — a grammar-derived regex HIR that
   carries types (for `->` projection over a regex leaf) must erase to nothing.
7. **Grammar totality + bootstrap (constitution i):** the regex grammar **must be bbnf-defined
   and bootstrapped** — yet **VERIFIED: no `regex.bbnf` exists anywhere** (only
   `skinny/grammars/json.bbnf`); the current HIR parser is hand-written recursive descent
   (`hir/parser/` — 7 files: mod/class/escape/group/repetition/atom/cursor). So the grammar-
   first regex parser is **entirely green-field** — a bootstrap to author, not a refactor.
8. **Consumer-in-wave (Y.13, constitution viii):** a prefilter/tier/functor with no caller does
   not land. PROVE-10's seam is named: wherever the generator emits an unanchored regex-leaf
   scan it calls `dfa.find` instead of a hand `find_at` loop (PT-1.W5 ⇄ GN-2.W1 — **exactly one
   lowering per site**, no double-emit against bbnf-simd's `find_member`).

---

## 3. The traps named in comparable systems (what to NOT relitigate as an assumed win)

- **"One giant union HIR" bloat (against 5A).** The concrete HIR table (§0) shows regex needs
  `Class`+`Look`+byte-`Literal`; bbnf needs `Ref`+`Project`+`Discard`+`Annotation`. burntsushi's
  own architecture keeps `regex-syntax::Hir` **separate** from the automata layer and from any
  host grammar — domain-fit representations won there. A forced single type risks a lattice
  where half the variants are `unreachable!()` on each side. The lawful-functor framing (5B)
  exists precisely to avoid this; 5A must **prove** the union pays for itself, not assert it.
- **Alt-mode conflation (a correctness trap, not a perf one).** bbnf `AltMode::Dispatch`
  (LL(1)/first-set) vs regex `Alternation` **leftmost-first priority with backtracking** are
  *different languages of choice*. Prefilter/literal extraction is only correct because it is
  **order-preserving for leftmost-first** (R7 §4). A unified HIR that erases the mode distinction
  can silently change match semantics — the exact "parity gap: a tier silently changing the match
  set" defect PT-1.W7 hunts.
- **DFA minimization as e-graph saturation is UNPROVEN for this class (5C's riskiest claim).**
  **No production regex engine ships automata minimization via equality saturation.**
  regex-automata uses classical constructions and **does not even minimize the lazy DFA** (the
  hot engine); the eager DFA uses Hopcroft (already in `dfa.rs:291`). The web sweep on
  "DFA minimization + e-graph" returned **no result tying the two** — it is a research frontier,
  not settled art. 5C's "Hopcroft as rewrite-to-normal-form" must be a **measured head-to-head
  with a named negative-result exit**, per the constitution's concrete-artifacts clause.
  (The HIR-tier egraph — already built, 1253 LOC — is the *safe* saturation home; the automata
  tier is the speculative one.)
- **Derivatives elegance-vs-throughput trap.** Canonical minimal DFA from derivatives is
  beautiful and unifies class/negation — but loses to Thompson+lazy on scan throughput in every
  shipping engine. Adopt derivatives (if at all) as the *class-algebra / minimization* mechanism
  behind a measured gate, never as the throughput path.
- **The lazy-DFA cache-thrash trap (PT-1.W3 risk, already named).** A high-entropy corpus over a
  large unicode class re-determinizes per sample; compile win but throughput regression. The
  lazy engine's gate must bind **both** axes (compile ≥5× AND throughput 0.98–1.02× of eager)
  and the **state-cap bail must return `Bail`, never a partial/wrong match** (the cap-boundary
  race is the sharp edge).
- **Bootstrap "rip out the hand parser" trap.** LOCK-7's 7D lesson applies transitively: the
  bbnf-defined regex grammar should serve as a **differential ORACLE against the hand parser
  first**, promotion to the production parse path gated on measured parity + throughput — not a
  blind cutover of a 7-file hand recursive-descent that currently passes the suite.
- **Widening beyond memchr3 can LOSE to memchr's own vectorization** (PT-1.W2 §8 risk): memchr
  internally vectorizes, so a naive shufti loop on a near-in-tier 4-byte set can lose to
  `memchr3`+1-byte-fallback. The tier boundary must be a **measured crossover, pluggable cost
  input**, not a fixed width (constitution vi / pluggable-components).
- **classify.rs (1148 LOC) is load-bearing downstream.** `RegexClass::classify_regex` drives
  `FnDescriptor` specialization on the bbnf side (R3 §3). "Re-derive or re-home" (task) cannot
  drop a consumer — whatever replaces it must keep the specialization signal the generator reads,
  or the generator regresses silently.

---

## 4. Route-relevant reuse map (what each family would build on, verified)

| Family axis | Concrete substrate that EXISTS | The measured gap |
|---|---|---|
| 5A ONE-HIR | `ir/src/lib.rs:234` ExprKind ≈ `regex/hir/mod.rs:216` Hir (60% overlap) | `Class`/`Look` vs `Ref`/`Project`; `Regex{String}` opaque leaf; Alt-mode semantics |
| 5B LAWFUL-FUNCTOR | both HIRs total + small; shared `egraph`/`csp-solver` crates | round-trip oracle unbuilt; two-way totality unproven |
| 5C AUTOMATA-AS-EXTRACTION | `regex/egraph/` 1253 LOC (HIR saturation, cost.rs, rules/) BUILT | automata-tier saturation UNBUILT + unproven anywhere; Hopcroft already classical in `dfa.rs:291` |
| 5D GRAMMAR-FRAGMENT | `ExprKind::Regex{pattern}` opaque today; PROVE-10 `dfa.find` seam | direct lowering into grammar IR nonexistent; PT-1.W5 is the first wire |
| prefilter/Teddy tier | PROVE-10 (memchr, GO) + `automata/accel.rs` Memchr1/2/3 + `sets/byteset.rs`, `info/literal_prefix.rs` | shufti/Teddy = PT-1.W2 born-RED; wide-entry unbuilt |
| lazy DFA | `info/dfa_size.rs:38` estimator; regex-automata `LazyStateID` blueprint | PT-1.W3 born-RED; unchosen half |
| bootstrap grammar | hand parser `hir/parser/` (7 files); `json.bbnf` is the only extant grammar | **no `regex.bbnf`** — green-field author |
| classify re-home | `classify.rs` 1148 LOC → drives `FnDescriptor` | consumer-preserving re-derivation unspecified |

---

## 5. Synthesis for the design fleet (findings, not a chosen route)

1. **The engine of record is parse-that's 6742-LOC bbnf-regex; the skinny vendor is a partial
   number/string stub.** Design against the former; GN-1 re-vendors post-PT-1.W1.
2. **PROVE-10 (memchr prefilter, GO, 8.8–29.5×, 245 LOC additive, compile-neutral) is the
   settled floor** — the wider tiers (Teddy/shufti PT-1.W2) and the lazy DFA (PT-1.W3) are
   born-RED extrapolations of the *elsewhere-proven* regex-automata meta-engine ladder.
3. **The isomorphism question is concrete and 60% pre-answered by the two HIRs' shared spine** —
   but the deltas (`Class`/`Look` vs `Ref`/`Project`, and the `Dispatch`-vs-leftmost-first
   Alt-mode) are where 5A (union) risks bloat + a *match-semantics* correctness trap, and where
   5B (functor) buys domain-fit safety. The **e-graph is already the shared substrate at the HIR
   tier** (built, 1253 LOC) — the safe home for saturation.
4. **5C's automata-tier saturation (minimization-as-rewrite, prefilter-selection-as-extraction)
   is a research frontier with NO shipping precedent** — the sweep found none. It is a
   measured-head-to-head-or-named-negative-result axis, exactly the concrete-artifacts discipline
   the constitution demands. Derivatives are its natural but throughput-losing algebra.
5. **Grammar totality for regex is green-field:** no `regex.bbnf` exists; the bootstrap is an
   authoring job with LOCK-7's differential-oracle-first discipline, not a refactor of the
   passing hand parser.
6. **Hard constraints that kill routes on contact:** compile-axis non-regression; NEON-16B-only
   (no SVE/SME); no shim perf claims (real intrinsics / memchr NEON only); no warm benches;
   consumer-in-wave (no orphan prefilter); exactly-one-lowering-per-site (no double-emit with
   bbnf-simd `find_member`); classify's downstream `FnDescriptor` signal must survive any
   re-home; the dark DFA-subtest oracle (E0432) must be restored before any DFA-correctness
   claim.

Sources: [burntsushi regex-internals](https://burntsushi.net/regex-internals/),
[regex_automata hybrid](https://docs.rs/regex-automata/latest/regex_automata/hybrid/index.html),
[LazyStateID](https://shadow.github.io/docs/rust/regex_automata/hybrid/struct.LazyStateID.html),
[Hyperscan NSDI'19](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf),
[Teddy ICPP21](https://oaciss.uoregon.edu/icpp21/views/includes/files/pap205s4-file2.pdf),
[Owens et al derivatives re-examined](https://www.khoury.northeastern.edu/home/turon/re-deriv.pdf),
[Antimirov partial derivatives](https://semantic-domain.blogspot.com/2013/11/antimirov-derivatives-for-regular.html),
[Yacc is dead (CFG derivatives)](https://arxiv.org/pdf/1010.5023),
[eqsat dialect](https://arxiv.org/pdf/2505.09363),
[Tensat equality saturation](https://arxiv.org/pdf/2101.01332),
[tzakharko M4 SME/SVE](https://github.com/tzakharko/m4-sme-exploration/blob/main/reports/01-sme-overview.md).
Local: `parse-that/rust/regex/src/{hir/mod.rs:216, automata/dfa.rs:291, egraph/, classify.rs}`,
`skinny/crates/ir/src/lib.rs:234`, `skinny/crates/parse-that-regex/src/lib.rs`,
`prototypes/PROVE-10/{REPORT,VERIFY}.md`, `docs/tranches/sk-v24/PT-1.md`, SPEC §0.
