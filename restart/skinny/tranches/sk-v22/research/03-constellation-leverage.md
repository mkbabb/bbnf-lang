# SK-V22 R3 — Constellation seams + the leverage plan

Date: 2026-07-09. Scope: the three cross-tree seams the graduation must carry (parse-that
regex + TS backbone; xtask regen host + bbnf-buddy), and the distillation that turns the
SK-V21 evidence corpus into the graduated tree's durable documentation. All anchors are
file:line against on-host trees at HEAD.

---

## Leg 1 — parse-that: two seams, opposite directions

parse-that is **its own repository** (`/Users/mkbabb/Programming/parse-that`, remote
`github.com/mkbabb/parse-that`, git log HEAD `ef10d5b` VALUEJS-PT-E). It is NOT a bbnf
subtree. The archival touches **nothing** inside it. bbnf's relationship is two seams: it
*consumes* the Rust regex engine, and it *owes coordination* to the TypeScript package.

### 1A. The canonical Rust regex engine — exact seams the graduation carries

The engine lives at `parse-that/rust/regex`. Its manifest (`rust/regex/Cargo.toml:1-9`):
crate name **`bbnf-regex`**, lib `bbnf_regex`, version `0.1.0`, **edition 2024**,
"Bespoke regex engine: HIR, NFA→DFA compilation, structural classification." Modules
(`rust/regex/src/`): `algebra automata classify.rs egraph first.rs hir info path_lexer.rs
sets unicode.rs utf8.rs` — the full HIR-e-graph-riding engine, not a classifier stub.

**Dependency seam (`rust/regex/Cargo.toml:18-45`).** It build-depends on: `smallvec 1.13`,
`serde`(opt), `proc-macro2 1` (path_lexer emits `proc_macro2::Span` so bbnf-path's `path!`
proc-macro anchors diagnostics — AZ-IV.W2.3), `rustc-hash 2`, and critically **`egraph 0.1`,
`egraph-derive 0.1`, `csp-solver 0.1`** (Tranche H: the HIR e-graph rides the same
equality-saturation + CSP-scheduling substrate as bbnf's grammar tier). Dev-deps: `divan`,
`mimalloc`; four divan benches (`regex_parse/classify/hir_egraph/dfa_compile`).

**Path-patch seam (`parse-that/rust/.cargo/config.toml`, GITIGNORED local override).** The
three egraph/csp coordinates resolve locally:
- `egraph = { path = "../../bbnf-lang/crates/egraph" }`
- `egraph-derive = { path = "../../bbnf-lang/crates/egraph-derive" }`
- `csp-solver = { path = "../../csc411/CSC411_HW2_ProgrammingQuestion/csp-solver" }`

**This is the hard seam.** parse-that's regex engine build-depends on `egraph`/`egraph-derive`
whose only on-disk home today is `bbnf-lang/crates/egraph[-derive]` (confirmed present) — the
LEGACY tree that SK-V22 archives — and on `csp-solver` whose canonical home is the sibling
`csc411` repo (bbnf-lang/crates/csp-solver is a patched-in mirror; memory `csp-solver-crate`).
When legacy `bbnf-lang/crates/` archives, **egraph + egraph-derive must re-home into the
graduated tree** (they are general-infra crates — memory `general-infra-crates`), or the sibling
constellation's patch tables break. The graduation must publish egraph/egraph-derive at a stable
path and update the seam. csp-solver stays cross-repo (csc411); the graduated tree mirrors it
exactly as skinny already does (`skinny/Cargo.toml`: `csp-solver = { path = "../crates/csp-solver" }`).

**Reverse edge — parse-that does NOT hard-depend on legacy bbnf.** The config also lists
`bbnf/bbnf-ir/bbnf-ser` patch lines, but the bootstrap crate is now a pure re-export:
`rust/bootstrap/Cargo.toml` depends only on `parse_that = { path = "../parse_that" }` and its
comment records that the historical `bbnf_derive` registry edge was retired at bbnf-lang B2
(`6142387f`); `parse_that::regex::generated` is committed source. So archiving legacy bbnf does
not break parse-that's regex build — only the egraph/csp seam above must be preserved.

**Toolchain mirror.** `parse-that/rust-toolchain.toml` pins **`nightly-2026-04-11`** (resolves
`rustc 1.96.0-nightly 02c7f9bec`), nightly-only (parallel front-end, cranelift), for edition
2024. bbnf-lang root `rust-toolchain.toml` is the byte-identical mirror (both 2512 bytes). The
graduated tree must carry edition-2024 + this nightly pin for the regex crate to compile.

**Divergence to reconcile (name collision).** skinny already ships **its own** `bbnf-regex`
crate — but it is a slim **322-line** `src/lib.rs` (`RegexFacts`, `RegexHir`,
`RegexKind{Whitespace,QuotedString,Numeric,Unknown}`, `FirstSet`, `ByteSet256`), a *structural
classifier*, NOT the NFA/DFA engine. Same crate name as parse-that's canonical engine. Consumed
by skinny `codegen`, `ir`, `passes`, `bbnf-bench`. Separately, `skinny/crates/parse-that-regex`
(3022 LOC) is actually the **Eisel-Lemire fast-float/number parser** (`number/eisel_lemire/`,
`unicode/`, `integer.rs`) — a misnomer, no regex in it. So "parse-that as the one regex home"
(charter) requires an explicit decision in TW0: either the graduated tree **adopts the canonical
`bbnf-regex` engine** by path/patch (as it already patches egraph/csp) and retires the slim
classifier, or it renames the classifier to end the collision. The charter's intent is the
former; the slim fork is a divergence, not a second home.

### 1B. The TypeScript package — the constellation backbone

Confirmed consumers (each repo is its own live tree; `package.json` verified on-host):
| consumer | parse-that edge | other bbnf edge |
|---|---|---|
| `value.js` | **direct** `@mkbabb/parse-that ^1.0.0` | none |
| `latex-paper` | **direct** `@mkbabb/parse-that ^0.7.1` | **`@mkbabb/bbnf-lang ^0.1.1`** |
| `keyframes.js` | **transitive only** (via `@mkbabb/value.js ^3.1.0`); NO direct pin | none |

**Correction to the charter's consumer list (evidence-driven).** Only **value.js and latex-paper
directly depend on parse-that**. `keyframes.js/package.json` deps are `@mkbabb/value.js ^3.1.0`
*alone* — no `@mkbabb/parse-that` line anywhere (deps, devDeps, or peer). keyframes' parse-that
edge is transitive through value.js, and its source deliberately minimizes even that: its light
engines re-export from value.js's **`parse-that`-FREE math subpath** to carry "0 CSS-grammar /
0 parse-that / 0 engine modules" (`keyframes.js/src/animation/internal/leaves.ts:9-10`,
`src/animation/CLAUDE.md:47,395`). So the older "keyframes → parse-that ^0.8.1" reading is stale.
The **semver discipline** this implies is precise: a parse-that breaking change reaches keyframes
*only* through value.js's re-export surface, never a direct keyframes pin — value.js and
latex-paper are the two trees the uplift must not break at the parse-that seam; keyframes is
shielded behind value.js.

parse-that TS is at 1.0.0 (git HEAD `ef10d5b`, VALUEJS-PT-E). value.js tracks current (1.x);
latex-paper lags (0.7.1) — a version-skew note, not bbnf's to resolve. **latex-paper additionally
depends on `@mkbabb/bbnf-lang ^0.1.1`** — a published bbnf TS package is a real constellation
edge; the graduation's TS-publish story (if any survives the playground archival) must not
silently break latex-paper.

**bbnf's obligations TO parse-that (research/17 §3, 41 §8) — three, then nothing:**
1. **The NO-GO letter (W0.10).** bbnf's LR/packrat GO/NO-GO is **NO-GO, dated**
   (`research/41-feature-surface.md:116-121,148`). parse-that holds packrat/WDM open *for* a
   hypothetical bbnf LR consumer that is not coming; TW0 records the outbound NO-GO so
   parse-that is "RELEASED from the bbnf keep" (`41:116`). Outbound letter only, `§7.1`/`W0.10`.
2. **PT-E1 diagnostics mirror (opportunity, not obligation).** Scoped per-parse diagnostics —
   finish `ParseDiagnostic.expected` (`research/17-parse-that-refresh.md:198-212`,
   `41:119-123`). It is the design bbnf **mirrors WHEN** it grows an error surface, not a debt
   bbnf owes now (`17:209-212`: "No hard obligation, one opportunity").
3. **Toolchain mirror obligation (`17:275`)** — the nightly pin above is shared constellation-wide.

Nothing else. No fold-in, no retirement — the charter's Standing Correction 3 is confirmed by
the manifests: parse-that TS is a backbone bbnf consumes and coordinates with, not owns.

### 1C. Archival touches nothing in parse-that — confirmed

Separate repo, separate git, own version line (1.0.0), own toolchain. The only bbnf→parse-that
write is the dated NO-GO letter (a doc). The only parse-that→bbnf runtime need is the
egraph/egraph-derive path home (Leg 1A). No archival disposition applies inside parse-that.

---

## Leg 2 — xtask (the regen host) + bbnf-buddy (out of scope)

### 2A. Two xtask crates: legacy fork archives, skinny/xtask matures

**Legacy `xtask/` (316K)** — `src/` = `regen.rs regen_css.rs regen_simple_runtime.rs lib.rs
main.rs` + `runtime-projections/` + `tests/metadata_fail_closed.rs`. The **per-grammar regen
fork** (multiple `regen_*` entrypoints). Disposition: **archives WITH the legacy `crates/` tree**
— it regenerates the 247K-LOC legacy grammars and has no role in the graduated tree.

**skinny/xtask (5181 LOC: `src/*.rs` 4809 + `src/bin/*` 372)** — the right *pattern* (build-time codegen out of proc-macro = B2's
cure, `research/12-dig-b0-b2.md:46-55`; deps: `codegen`, `blake3`, `serde_json`, `toml`;
features `runtime-tools`/`conformance`; bins `capacity_probe`/`parity_oracle`/`profile_lazy`).
But it is **not yet the clean one-generator host** — gaps to close before it is fit:
- **Per-grammar fork residue.** Still splits `regen.rs` (187) + `regen_css.rs` (98) — a
  CSS-specific regen path, exactly the fork the graduation is meant to collapse to ONE generator.
- **God-module risk.** `main.rs` is **2780 LOC** — violates memory `no-god-modules`; must split
  into a directory module.
- **Tranche one-offs.** `skv15_w0.rs` (729) + `real_typed_schema.rs` (1014) are tranche-specific;
  fold into the generator or archive with SK-V15 history, do not carry as live xtask surface.
- **Missing gates (all from the B2 lesson, DEFERRED-LEDGER):** `regen --check` byte-equality
  (N1, `DEFERRED-LEDGER.md:61`); **`generated_loc_budget` enforced in `regen --check`** (N6,
  `:66` — `css_l4.rs` was **108,406 lines / 6.1 MB, 7× budget**, `research/12:130-135`; the B2
  lesson is "relocation is not reduction"); **N25 idempotent-on-disk** assertion (`:90`, B6.W0
  192× payoff). These are the graduated regen host's acceptance criteria.

### 2B. bbnf-buddy — zero touchpoints, sibling scope (correction confirmed)

`~/Programming/bbnf-buddy` is a live repo (own git, SPEC/DESIGN, playwright baselines, package
`@mkbabb/bbnf-buddy 0.1.0`). Its `package.json` deps: `csp-solver-wasm, glass-ui, keyframes.js,
morph, pencil-boil, value.js` — **no dependency on bbnf-lang, parse-that, or any bbnf grammar/
wasm.** The only `bbnf` strings in src are its **own** file-format brands (`.bbnf-pose.json`,
`.bbnf-form.json`, `.bbnf-bundle.json` — its pose/form serialization) and the one `.wasm` is
`csp-solver-wasm` (from csc411), not bbnf. **Disposition: sibling project, entirely out of
bbnf-lang scope; SK-V22 archival touches nothing in it.** (Transitive-only edge, informational:
bbnf-buddy → value.js → parse-that, two hops — so parse-that TS stability indirectly reaches it,
via the backbone, not via bbnf.)

---

## Leg 3 — the leverage plan: corpus → durable docs → evidence index

The SK-V21 corpus is cached intelligence, not waste: `research/01-41`, `prototypes/`
(PROTO-A1..A3, P1..P7), `critiques/` (CRIT-P1..P7, spec-architecture/honesty v2-v4,
freeze/blocker audits), ledgers (DEFERRED, CONVERGENCE, GESTALT-SYNTHESIS), `restart/locks/
LOCKS.md`, corpora, SK-V20 docs, memory. The whole tree **archives** under branch+tag; six
distilled documents + one index carry FORWARD into the graduated tree so no lesson is re-learned.

### 3A. The six durable documents and what each distills from

| Doc (graduated tree) | Distills FROM (report-level citations) |
|---|---|
| **ARCHITECTURE.md** | skinny `SUBSTRATE.md`, `COMPILER.md`, `WORKSPACE.md`; `research/20` (lacuna-architecture), `38` (seam disposition), SPEC.md. The surviving architecture: un-forked structural dispatch, grammar-derived projections, lazy cursor, fused sink, NEON kernels (PASS-ALPHA:11-14). |
| **PERFORMANCE-THESIS.md** | skinny `BENCH.md`; `research/22` (simdjson), `23` (json-rust), `24` (css), `25` (dav1d-neon), `33` (cold-truth). Live-lane ratios: **1.59/1.42/2.14× vs sonic-rs**, full structural CSS **2.0–3.4× vs lightningcss CSSOM** (`33:59,89`). |
| **NEGATIVE-RESULTS.md** | the do-not-relearn ledger — see 3B. |
| **BENCH-PROTOCOL.md** | `research/33` (cold-truth), `21` (lacuna-profiling), `prototypes/PROTO-A1` (bench-adjudication), `PROTO-A2` (kernel-pmu); PASS-ALPHA §4. Cold-only, PMU cyc/byte, paired per-sample interleaving, per-block loadavg, multi-window, load-robust ratio certification. Enforces memory `no-warm-benches`, `bench-single-run`, `no-polling-loops`. |
| **ORACLES.md** | `skinny/xtask/src/bin/parity_oracle.rs` (skinny JSON vs serde_json, structural_match, first-byte divergence, exit 1), `capacity_probe.rs`; `skinny/corpora/` (`json.bbnf`, `css-l4-sk-v14`); `test-fixtures` crate. The parity/conformance gate the graduated tree runs. |
| **EVIDENCE-INDEX.md** | the navigational map — see 3C. |

### 3B. NEGATIVE-RESULTS.md — the do-not-relearn ledger (cited)

Each entry: the lesson, then the archived anchor that proves it. This is the highest-value
distillate — it stops dead ends from re-running.

1. **SoA loses to AoS+sib_skip.** A 7-column SoA tape split was measured to lose to a single
   `Vec<TapeRec>` AoS + parallel `sib_skip` on cache-locality; reverted at AY-I.W1; "the most
   important architectural reversal of Era VI." `research/35-deferred-fold.md:83` (N19),
   `:15` (C1); underlying `era-VI-restart.md:59-64,76-81` via `14-dig-ba-bd`. **Any "AoS→SoA"
   proposal is blocked until it reconciles with this.**
2. **DTA / direct-threaded residue is un-inlinable dead weight.** `dta.rs` ~900 LOC dead reclaim;
   `dta`/`tape::visitor` ×14 residue "dies-at-cutover." `research/35:88` (N24),
   `38-tw1-seam-disposition.md:77`. Do not resurrect a direct-threaded interpreter path.
3. **The structural bitmap pre-scan is twice-buried, twice-exhumed.** Fully specified at AO
   Phase 0, deleted at AQ.5 as dead code because the derive path never called it and no
   end-to-end activation gate existed. Resurrection condition is explicit: "one production grammar
   shows `scan_structural` under `cargo expand`" must be an OPENING gate, not deferred activation.
   `research/35:84` (N20), `04-dig-am-ao.md:213-215`; the same gap rediscovered from the runtime
   side twenty tranches later (`21`).
4. **"Eager = a fold over lazy" must be measured, not asserted (fold-over-cursor risk).** simdjson
   On-Demand skips 3–4× more aggressively than a full parse (`22-sota-simdjson.md:77,106`); bbnf's
   direct-to-struct bridge must produce separate DOM-materialize / lazy-cursor / direct-to-struct
   numbers so the fold-over claim is falsifiable (`23-sota-json-rust.md:36-43,212-217`).
5. **Hand-asm is obviated by well-shaped Rust + `vshrn`.** Well-shaped Rust lands within a few
   percent of hand asm; durable wins are confined to idioms the compiler cannot synthesize (the
   `shrn`/`vshrn` movemask). `25-sota-dav1d-neon.md:14-18`; Lock-16 single-movemask co-gate via
   vshrn pack (`35:19`, C5). Do not hand-write asm kernels the autovectorizer already reaches.
6. **Warm benchmarks are fraud.** JSON was only ever measured WARM (the `profile_direct` tight
   loop amortising allocation); the real cold-vs-warm gap is **~14–20%, not the ~3× a warm loop
   implies** (`33-cold-truth-profiling.md:24,71-78`). "No JSON figure need ever be quoted warm
   again" (`33:165`). Also: a dead NEON pre-scanner cost 30–170% more cyc/byte to reclaim 3% —
   a net loss (`33:137,153`).

### 3C. EVIDENCE-INDEX.md — keeping the archive queryable, not dead

The archive is worthless if a future claim cannot be traced to its proof. EVIDENCE-INDEX is a
flat table in the graduated tree mapping **every load-bearing claim → the distilled doc section →
the archived report file:line → (where relevant) the deeper primary anchor.** Shape:

```
claim id | graduated claim              | durable doc §        | archived report:line          | primary anchor
NR-1     | AoS beats SoA                | NEGATIVE-RESULTS §1  | sk-v21/research/35:83 (N19)   | era-VI-restart.md:59-64
PT-1     | JSON 1.59× vs sonic-rs cold  | PERFORMANCE-THESIS   | sk-v21/research/33:59         | skinny/BENCH.md
SEAM-1   | regex egraph/csp path seam   | ARCHITECTURE §seams  | sk-v22/research/03 (this)      | parse-that .cargo/config.toml
XT-1     | css_l4 108,406 LOC / 7× budget | ARCHITECTURE §regen | sk-v21/research/12:130-135     | DEFERRED-LEDGER:66 (N6)
```

Because the SK-V21 tree archives under a **named branch+tag** (not deletion), every
`sk-v21/research/NN:line` coordinate stays resolvable via `git show <tag>:path`. EVIDENCE-INDEX
is the single durable pointer INTO the archive; it is what makes branch-archival (cheap, complete)
strictly better than distill-and-delete (lossy). New graduated-tree claims append a row; a claim
with no row is, by construction, unsupported.

---

## Disposition summary (this leg's inputs to the archival plan)

- **parse-that**: untouched separate repo. Carry the egraph/egraph-derive path home into the
  graduated tree (hard seam); mirror csp-solver from csc411; mirror edition-2024 + nightly-2026-04-11.
  Reconcile skinny's slim `bbnf-regex` classifier vs parse-that's canonical `bbnf-regex` engine
  (name collision) — adopt canonical as the one regex home. Emit the dated NO-GO letter (W0.10);
  hold PT-E1 as an opportunity. Note latex-paper's `@mkbabb/bbnf-lang ^0.1.1` TS edge.
- **xtask**: legacy `xtask/` archives with legacy `crates/`. skinny/xtask matures into the one
  regen host — collapse `regen_css.rs` into one generator, split the 2780-LOC `main.rs`, fold the
  tranche one-offs, add `regen --check` byte-equality + `generated_loc_budget` + N25 idempotency.
- **bbnf-buddy**: out of scope, zero touchpoints; archival changes nothing in it.
- **leverage**: six durable docs (ARCHITECTURE, PERFORMANCE-THESIS, NEGATIVE-RESULTS,
  BENCH-PROTOCOL, ORACLES) + EVIDENCE-INDEX; SK-V21 corpus archives under branch+tag so every
  cited file:line stays git-resolvable.
