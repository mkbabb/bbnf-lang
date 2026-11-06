# Consumer-plane adjudication and final design (lockstep, 2026-11-05)

I checked the critics' facts read-only against these trees:

- bbnf-lang: I fetched origin, which is c9b93adb4. Local master is af15f63e0.
- value.js.
- npm.

## A. Facts I re-verified

| Fact | Finding |
|---|---|
| bbnf-lang local master vs origin/master | Ahead 71 and behind 38. The merge base is d2b32f6e3. |
| Files changed on both sides since the merge base | Only `.gitignore`. |
| Local-side changes under `typescript/` | 0 files. |
| Main checkout | 243 porcelain entries. |
| `let _ = csp.propagate()` sites at origin/master | **6**, not 3 and not "about 8": `inline/plan.rs:167`, `egraph/csp_scheduler.rs:235`, `dispatch/eligibility.rs:67`, `sets/first_sets.rs:71`, `sets/follow.rs:70`, `passes/span.rs:61`. |
| What happens after `propagate()` | Every site reads the solved domain (`domain.solved`, `domain.decision`). Only the `Result<(), Unsatisfiable>` is thrown away. The AZ-IV audit flagged this already (AUDIT-C row "VALUE-DISCARD"). |
| `impl LatticeDomain` | There are 6 domains (Inline, Dirty, Bool, CharSet, Dispatch, Bitset). They are distinct lattices, not duplicates. |
| `InlineDomain::join` (`constraints.rs:85`) | Undecided→Decided only. `Decided(X) ⊔ Decided(Y)` keeps whichever came first and does not signal the conflict. So it is first-writer-wins and not commutative. It is not last-writer-wins. |
| Name `TypeDomain::join` | Does not exist. `TypeDomain` lives in `ir/passes/types/constraint/domain.rs`. |
| TS `@pretty` | `typescript/src/grammar.ts:409` parses `@pretty`. `emit.ts` has 0 references to it. |
| value.js `files` | `['dist', …]`. The `.bbnf` sources are not in the tarball. |
| value.js on npm | `latest` = 4.2.1. The 4.3.0 release commit `d5a8fbc0c` is local only. |
| `bench/records/W9lg1/{node,browser}` | Both empty. |

## B. Rulings

### Critic 1

- **O-1, ACCEPT.** The TS emitter has its own analysis code (`analysis/*.ts`, `optimize.ts`, `emit.ts`). "Same LoweringPlan" is a producer rewrite, not an interface that already exists.
  - I add unit **P-IR-TS**. It goes to the IR-plane seat, after wf_d837f96c closes.
  - The route I rule is **(a)**: the Rust front end emits a canonical `plan.json` with a `plan.hash`. The TS emitter reads it at generation time only, through a pinned `bbnf` CLI or wasm, so there is no runtime dependency.
  - I reject (b), a TS port of the IR, because it would be a second IR implementation. That breaks the gestalt law.
  - G-B1's second target and C3.2's ticket path are marked BLOCKED-ON-P-IR-TS.
- **O-2, ACCEPT, with a corrected count.** The defect is "Unsat swallowed at 6 sites". It is not "result dropped".
  - Falsifier: a grammar with contradictory constraints must give a pass error or `PassReport.saturated=false` at all 6 sites. Today it emits code.
  - `git grep -c 'let _ = csp.propagate'` must go from 6 to 0.
  - The join claim is restated as: `InlineDomain::join` is not commutative and silently drops Decided/Decided conflicts. The cure makes the conflict top (⊤), which raises Unsat.
- **O-3, ACCEPT.** G-P1 is narrowed to:
  - `formatCSSKeyframeString` (format.ts:201), emitted text→text;
  - value.js `serialize` round-trip, `parse∘print∘parse = parse`.

  The model serializers (`serializeSelector`, `serializeAdmittedValue`, `serializeEasing`) stay as actions. Each gets a ruling row saying "model→text, not a reprint". The "≥300 lines deleted" target is struck. I add producer unit **P-PP-TS**: the TS emitter emits a printer from `@pretty` as an inline Wadler renderer.
- **O-4, ACCEPT.**
  - value.js adds `src/css/grammar/*.bbnf` to `files`. That `package.json` edit is gated after wf_d837f96c closes.
  - bbnf-lang `grammar/corpus/value-js/` is pinned to a **released** value.js version, and its hashes are checked against that tarball.
  - The pin is bumped by a PR. bbnf-lang CI never reads value.js HEAD.
- **O-5, ACCEPT.** "Fetch and fast-forward" is struck from the seat LAW. P-2 is done in a clean worktree, as Critic 2 #1 describes. The only overlapping path is `.gitignore`, and the local side touches no `typescript/` files, so the merge does not collide with wf_d837f96c.
- **O-6, ACCEPT.** The G-S1 analysis is pre-registered:
  - non-inferiority on all 14 cells (CI upper bound < 1.03);
  - superiority only where the Holm-adjusted CI excludes 1.0;
  - ABBA interleaving, a fixed seed, and the corpus hash recorded;
  - an equal-work proof for base 4038dce5 (same output shape, same actions bound).
- **O-7, ACCEPT.** This is merged with Critic 2 #6/G-S2. Tiers are TOKENIZE, TREE and RESOLVED-COLOR, and each competitor is compared only at its own tier. The oracle is the shared WPT expected-serialization subset, with coverage denominators. The 0.70× target is struck.
- **O-8, ACCEPT.** x86 arms are claimed for correctness only: differential fuzz in CI, Miri on the scalar path, sanitizers. Perf claims are NEON on this Mac, in a quiet window. x86 perf is "UNCLAIMED" unless bare-metal hardware exists. One owner ruling covers P-0 and the hardware question.
- **O-9, ACCEPT.** The G-J1 timed region runs end to end: string → wasm copy → JS object graph that is structurally equal to `JSON.parse` output. A tape/lazy variant is timed separately and labelled as a different API. The expected outcome, RULED-closed, is written in advance.

### Critic 2

- **#1, ACCEPT.** It is the same finding as O-5 plus the dirty checkout, and both are verified.
  - Unit C-1: `git worktree add ~/Programming/bbnf-lang-lockstep origin/master`, branch `lockstep/merge-skv25`, ordinary merge of `master` (af15f63e0).
  - The 243 dirty entries get a read-only inventory with an owner per entry. Nobody touches them.
  - Falsifier: `git merge-base --is-ancestor af15f63e0 origin/master` is false now and must be true after.
  - I partly reject the claim that "the 38 commits would collide with wf_d837f96c files". The only overlap is `.gitignore`, so the merge can proceed now. Only a push touching `typescript/` would wait.
- **#2, ACCEPT IN PART.**
  - Accepted: there are several dropped sites (6, not 3), and the CSP sits inside `backend/rust/`. The inline plan moves into `crates/ir` (passes). Without that move, the TS emitter cannot consume the same plan (P-IR-TS).
  - Rejected: "one `fn join`, delete the second lattice". The domains are semantically distinct. One generic solver with several domains is correct design. The cure is a law test for **every** `LatticeDomain` impl: commutative, associative, idempotent, and monotone. It must be RED today on `InlineDomain`. Domain count stays as it is.
- **#3, ACCEPT.** `crates/core/src/generate/regex` is lifted to `crates/regex` and owns the regex HIR and `RegexSpec`. It has two consumers: the existing DFA emitter (Rust) and a JS RegExp printer (TS). parse-that `scan` is the ScanKernel runtime that `dfa/accel` calls, not a regex compiler. I-4 and I-5 are rewritten to match.
- **#4, ACCEPT.** Applications (kf demo, fourier web, atlas app, daraksha) pin exactly. Libraries (glass-ui, keyframes.js as published) use a caret peer range `^4.3.0` and pin exactly only in devDependencies. The gate is `npm ls @mkbabb/value.js` showing one resolved copy per app lockfile.
- **#5, ACCEPT.** C1.1 becomes an observation unit only. It waits for `npm view @mkbabb/value.js version` = 4.3.0 with provenance, published by wf_d837f96c. This lockstep never publishes value.js. The kf and daraksha pins at 4.2.0 are recorded in C0.2.
- **#6, ACCEPT ALL.**
  - G-S2: work classes, as in O-7.
  - G-S1: C0.1 sets the targets. Until then the only goal is "no regression" under O-6.
  - G-B1: a falsifier, not a target. Every byte delta must be explained by a `plan.hash` diff.
  - G-L1: an absolute budget, p95 parse of the 57 K-char path < 1 ms (NEON, quiet window), held under a fixed share of the frame. The ratio to the hand tokenizer is reported as information only.
  - G-J1: end to end, as in O-9.
- **#7, ACCEPT as far as I can see it** (the text is truncated). The machine-honesty part is folded into O-8. Anything past the truncation is unruled and goes back to Critic 2 for restatement.

## C. Final design

### 0. Preconditions and ownership

- **P-0 (owner ruling).** Runtime dispatch with both arms, x86 (SSE4.2/AVX2) and aarch64 NEON, each behind a scalar oracle. This is read as superseding O033. Under O-8, x86 is claimed for correctness only, and the owner rules on bare-metal x86 hardware in the same ruling.
- **P-1 (owner ruling).** value.js's six WPT-gated grammars (`color`, `css`, `math`, `stylesheet`, `tokens`, `value`) are the canonical CSS grammar.
  - bbnf-lang `grammar/css/l4` is retired in favour of `grammar/corpus/value-js/`, pinned to a **released** value.js tarball, with hashes checked against that tarball.
  - value.js ships `src/css/grammar/*.bbnf` in `files`.
  - Pins are bumped by PR. The dependency never points at HEAD.
- **P-2 = unit C-1 (history reconciliation, compiler seat).**
  - Clean worktree `~/Programming/bbnf-lang-lockstep` at origin/master, branch `lockstep/merge-skv25`, ordinary merge of local `master` (af15f63e0). The only expected conflict is `.gitignore`.
  - The untracked sk-v25 docs are committed by pathspec.
  - The main checkout is inventoried read-only and never mutated.
  - Falsifier: the is-ancestor check goes false → true.
  - No consumer unit depends on bbnf-lang Rust before C-1 lands on master after CI.
- **P-3 (concurrency).** wf_d837f96c owns bbnf-lang `typescript/` releases, parse-that releases, and value.js's BBNF pin, `gen-grammar.mjs`, the generated modules and the value.js publish. "After W10 close" means after that workflow closes. Consumer work is done on branches and handed over by commit.

### 1. Target architecture

The pipeline is unchanged, with three amendments:

- The CSP and inline plan live in `crates/ir`.
- `crates/regex` owns the regex HIR and `RegexSpec`.
- The TS emitter consumes `plan.json` at generation time (P-IR-TS).

Invariants:

1. One grammar per language.
2. Self-contained JS emission: `from@mkbabb=0`, `import=0`, a byte ceiling per module.
3. Every text→text formatter is emitted from `@pretty`. Model→text serializers stay as actions and carry ruling rows.
4. Pins follow #4: exact for apps, caret peer ranges for libraries, one resolved copy per app.
5. Every adoption carries a benefit table, with gates run twice.

### 2. Goals

| ID | Goal and measure |
|---|---|
| G-B1 | ≤ 15,200 / 6,100 gz (grammar.js / color.js) holds now. After P-IR-TS this becomes a falsifier: every byte delta is explained by a `plan.hash` diff. There is no pre-set lower target. |
| G-S1 | Pre-registered: non-inferiority with upper bound < 1.03 on all 14 cells; superiority only under Holm; ABBA order; seed and corpus hash recorded; equal-work proof for 4038dce5. The targets are set from C0.1. The W8 numbers stay NOISY. |
| G-S2 | Three tiers, each with its own competitors:<br>• TOKENIZE: tokens.bbnf vs `@csstools/css-tokenizer` and `postcss-value-parser`.<br>• COLOR→RGBA: `parseCssColor`+`toRgba8` vs `culori` vs `colorjs.io`.<br>• FULL-VALUE: reported unranked.<br>Shared WPT subset with coverage denominators. Report ratios with CIs. No pre-set target. |
| G-C1 | 66 V-C misses → 0 unruled. var()-origin is ruled under DR-c. |
| G-L1 | 100 % of the svg-path corpus; p95 < 1 ms on the 57 K-char path (NEON, quiet window); the ratio is information only; ≥ 150 hand lines deleted. |
| G-L2 | Unchanged: 0 AST mismatches, ≥ 1,300 lines deleted, ≤ 1 parse-that copy (ideally 0), build ms ≤ 1.0×. |
| G-P1 | `formatCSSKeyframeString` byte-identical via the emitted printer; value.js `serialize` round-trip holds on 100 %; a ruling row for each model serializer. |
| G-F1 | Unchanged: `parse_expr` count = 0, equation tests GREEN, adversarial classes rejected with a position. |
| G-R1 | Unchanged: 0 hand colour regexes; glass-ui `rgb(1 2 3 / 50%)` gives α 0.5. |
| G-J1 | End to end, including the wasm copy and object materialisation; lazy/tape timed separately; RULED-closed written in advance as the likely outcome. |
| G-X1 (new, producer-facing) | 6 → 0 swallowed-Unsat sites. Lattice law test GREEN on every `LatticeDomain`. |

### 3. Interfaces

- **I-1 (Grammar→IR).** Unchanged: `IrModule` canonical, `ir.hash` per grammar in the generated header.
- **I-2 (IR↔Passes).** Defect restated: Unsat is swallowed at 6 sites, and `InlineDomain::join` is not commutative. `PassReport.saturated` and `unsat[]` must be filled. The inline plan moves to `crates/ir`.
- **I-3 (Passes→LoweringPlan).** `plan.json` is produced by Rust and consumed by both emitters (P-IR-TS route (a)). Until then, I-3 does not exist for TS.
- **I-4 (Regex).** `RegexSpec` is owned by `crates/regex`, the lifted `generate/regex`. It has two printers: the Rust DFA, which exists, and a JS sticky RegExp. A differential accept-set fuzz test checks they agree. The subset is defined by `grammar/misc/regex.bbnf`.
- **I-5 (SIMD).** The ScanKernel is a parse-that runtime called by the DFA's `accel`. Arms: NEON, SSE4.2, AVX2, wasm-simd128. Each arm has a scalar oracle, a differential fuzz test and Miri on the scalar path. x86 is claimed for correctness only.
- **I-6 (Printer).** `DocPlan`. The Rust target is pprint `Doc`. The TS arm, a 2–4 KB inline renderer, is unit P-PP-TS. Text→text only.
- **I-7 (Action ABI, TS).** Unchanged.
- **I-8 (Python face).** Route (b) first: the browser parses and the API validates the AST against a closed node set.

### 4. Units

#### Producer units the consumers depend on

None of these has a consumer seat.

- **C-1** history reconciliation (compiler seat; can start now).
- **P-CSP**: Unsat surfaced at all 6 sites, lattice laws, inline plan moved to `crates/ir`. After C-1.
- **P-RX**: lift `crates/regex` and add the JS printer. After C-1.
- **P-IR-TS**: `plan.json` consumed by the TS emitter. After P-CSP and the wf_d837f96c close.
- **P-PP-TS**: TS printer emission from `@pretty`. After the wf_d837f96c close.

#### Wave C0: truth-taking

- **C0.1** Run W9lg1 (node and browser) under `guarded --cpu-cores 0`, after waiting for 1-minute load < 8, twice, using the O-6 protocol. The result directories are empty today. C0.1's numbers set the G-S1 targets.
- **C0.2** Baselines ×2 in `~/.dev-logs/bbnf-lockstep/baselines/`, including the pin drift (kf and daraksha at 4.2.0).
- **C0.3** Benefit-table template (unchanged).
- **C0.4** Tiered competitor harness: published releases, lockfile hash, identical hashed corpus, n ≥ 31, medians with IQR.

#### Wave C1: release and repin

- **C1.1** Observation only. Wait for npm 4.3.0 with provenance, published by wf_d837f96c.
- **C1.2** Repin each repo on its own branch, with the merge after CI, using the #4 policy and the `npm ls` single-copy gate. daraksha goes as a relay branch for daraksha-v3 to merge.

#### Wave C2: delete hand colour parsing

- **C2.1–C2.4** Unchanged: glass-ui (with the BH/BI relay), daraksha (relayed), atlas, and fourier hexChannels (decided by a micro-bench; if the cure is needed it is an emitted `hexColor` entry point).
- **C2.5** Unchanged: the kf library residue. `+=n` becomes a kf-owned `sequencePosition` `.bbnf` rule, with the falsifier "`a\,b, c` → 2".
- **C2.6** onward (kf demo and later). The source text is truncated. Every unit after C2.5 inherits these rulings: equal-work tiers, absolute per-frame budgets, the text→text-only scope for printers, and dependencies stated against P-*.

#### Later waves, gated on producer units

| Wave | Content | Waits for |
|---|---|---|
| C3 | value.js reads (G-S1, G-S2, G-B1) | P-IR-TS (for the G-B1 second read) |
| C4 | svg-path grammar plus fourier (G-L1) | P-RX |
| C5 | latex/bibtex grammars plus latex-paper (G-L2) | C-1 |
| C6 | printers (G-P1) | P-PP-TS |
| C7 | fourier equation route (b) (G-F1) | no producer dependency |
| C8 | JSON for TS consumers (G-J1); the simd-json/sonic-rs race stays with the rust-perf seat (gorgeous/CLI) | rust-perf seat |

### 5. Open owner rulings

1. **P-0** (x86 dispatch supersedes O033) and bare-metal x86 hardware for perf claims.
2. **P-1** (value.js grammars become canonical; `grammar/css/l4` is retired).
3. Adding `.bbnf` to the value.js `files`, done by wf_d837f96c or after it closes.