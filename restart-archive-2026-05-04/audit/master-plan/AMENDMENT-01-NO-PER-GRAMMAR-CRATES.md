# Master Plan Amendment 01 — No Per-Grammar Crates

Date: 2026-05-03
Supersedes: portions of `restart/audit/master-plan/MASTER-PLAN.md` enumerating per-grammar declaration crates.
Retains: every other reconciliation, lock verdict, tranche allocation (modulo per-grammar-crate substance).

## Premise

The master plan committed at `a9a85f45` declares 33 workspace members, 9 of which are per-grammar declaration crates (`crates/json/`, `crates/css-l4/`, `crates/bbnf-meta/`, `crates/google-sheets/`, `crates/bnf/`, `crates/csv/`, `crates/ebnf/`, `crates/css-pretty/`, `crates/math/`). This is overfitting. Lock 14 names per-grammar declaration crates as an *optional* escape hatch, not a default. The master plan elevated the escape hatch to a mandatory 9-crate footprint without cause.

## Settled position

**Zero per-grammar crates** in the post-restart workspace. The greenfield is fully grammar-driven and fully agnostic. Every grammar plugs into the fleet via two declarative surfaces:

1. **Grammar source file** — `grammar/<name>/<name>.bbnf` (and any auxiliary `.bbnf` modules)
2. **Workspace metadata block** — `[workspace.metadata.bbnf.grammars.<name>]` in the root `Cargo.toml`

That is the entire onboarding surface. Adding a 10th grammar `yaml.bbnf`:

- Drop `grammar/yaml/yaml.bbnf` into the source tree
- Add a metadata block
- Run `cargo xtask regen`

No code change in any crate. No new crate. The future-grammar onboarding test (Lock 14 verification) collapses to a two-step ceremony.

## What replaces per-grammar declaration crates

### Generated parser + runtime modules

Live in `crates/bbnf-runtime/src/grammars/<name>/{generated.rs, runtime.rs}` — one subdirectory per grammar, but the subdirectories are **emitted from a single grammar-agnostic template** (`bbnf-runtime-template` at `crates/bbnf-runtime-template/`) consuming (grammar source + metadata). The per-grammar subdirectory is an *artefact* of regen, not a hand-maintained module. No `crates/<grammar>/` crate in the workspace.

Lock 13 footnote: the per-grammar subdirectory under `bbnf-runtime/src/grammars/` is permissible because every subdirectory is structurally identical (template-emitted), so the parent directory is cohesive — its concern is "houses generated grammar runtimes". This is **not** a god directory in the Lock 13 sense (which forbids hand-mixed concerns).

### Host-fn implementations

Live in `crates/bbnf-host-prims/src/` — a single grammar-agnostic primitive library. Primitives:

| Primitive | Signature | Used by |
|---|---|---|
| `parse_int_radix(s, radix)` | `&str → i64` | every grammar parsing integers |
| `parse_float(s)` | `&str → f64` | every grammar parsing floats |
| `parse_enum<T>(s, table)` | `&str → T` | dimension units, color spaces, cell-ref letters |
| `parse_hex_pair(s)` | `&str → u8` | CSS hex colors, byte-array literals |
| `slice_borrow(s, range)` | `&'i str → &'i str` | every leaf returning a slice |
| `cow_unescape(s)` | `&str → Cow<str>` | every grammar with escape sequences |
| `regex_captures(input, pattern)` | `&str → Vec<&str>` | leaf scanners |
| `validate_predicate(s, pred)` | `&str → bool` | host-fn validators |

Per-grammar logic (e.g., CSS hex-color parsing) is the *composition* of these primitives, expressed in:

- **Workspace metadata** (declarative composition): `parse_hex_color = compose("regex:#[0-9a-fA-F]{6}", "parse_hex_pair", "parse_hex_pair", "parse_hex_pair", "Color::Rgb")`
- **OR extended BBNF directives** (in-grammar composition): `@host parse_hex_color: regex("#[0-9a-fA-F]{6}") -> Color { Color::Rgb(parse_hex_pair($1[1..3]), parse_hex_pair($1[3..5]), parse_hex_pair($1[5..7])) }`

The host-fn composition is **grammar-driven**, not grammar-specific. The compositional vocabulary is the same across grammars; what varies is which primitives are composed in what order — and that variation lives in metadata or grammar source, not in Rust code.

### Tests + fixtures

Live in `crates/bbnf-test-fixtures/<name>/` — one subdirectory per grammar, but the subdirectories carry **fixture files** (`.json`, `.css`, `.bbnf`, etc.), not Rust code. A single grammar-agnostic test harness (`crates/bbnf-runtime/tests/grammar_parity.rs` or similar) iterates the workspace metadata, finds each grammar's fixtures, and runs the same test suite uniformly.

### Bench harnesses

Live in `crates/bbnf-bench/benches/` — one bench per grammar, but each bench is **template-emitted** (the bench harness consumes metadata + fixtures and produces uniform-shape benches). No per-grammar bench-author intervention.

## Corrected workspace shape

Of the prior 33-member workspace, the 9 per-grammar declaration crates retire. Replacements:

- `crates/bbnf-host-prims/` — generic primitive library (NEW; replaces 9× per-grammar `host.rs`)
- `crates/bbnf-runtime/src/grammars/<name>/` — template-emitted; per-name subdirs but no per-name crates
- `crates/bbnf-test-fixtures/<name>/` — fixture files only; no Rust per-grammar

**Final workspace member count: 24.**

| Member | Role | Notes |
|---|---|---|
| `bbnf` | aggregator + user-facing entry | re-exports across the fleet |
| `bbnf-error` | unified error type | Tranche B |
| `bbnf-pipeline` | phase-state pipeline coordinator | Tranche B |
| `bbnf-grammar` | grammar source loading + metadata reading | Tranche A/C |
| `bbnf-parse` | source + scanner + parse driver + lower | Tranche C |
| `bbnf-ir` | IR types only (no passes) | Tranche C |
| `bbnf-passes` | every transformation pass | Tranche C, F |
| `bbnf-vm` | bytecode VM substrate | Tranche C |
| `bbnf-codegen-ir` | typed IR (22 variants per BC.W0) | Tranche D |
| `bbnf-codegen` | per-backend lowerers (Rust/TS/WASM) | Tranche D, H |
| `bbnf-runtime` | runtime substrate + template-emitted grammar subdirs | Tranche E |
| `bbnf-runtime-template` | grammar-agnostic generator | Tranche E |
| `bbnf-host-prims` | host-fn primitive library (NEW; replaces 9 per-grammar host.rs) | Tranche E |
| `bbnf-host` | host-fn dispatch + registry mechanism (generic) | Tranche A/E |
| `bbnf-test-fixtures` | fixture files (no Rust per-grammar) | Tranche J |
| `bbnf-bench` | vitest-style bench harness with template-emitted benches | Tranche A/J |
| `bbnf-language-server` | merged analysis + lsp | Tranche I |
| `path-core` | path AST + lex/lower/validate | Tranche A |
| `path` | Rust proc-macro shell | Tranche A |
| `path-ts` | TS cdylib shell | Tranche A, H |
| `parse-that` | combinator library (permanent path-dep) | Tranche I |
| `bbnf-regex` | bespoke regex engine (path-dep until publish) | Tranche I |
| `egraph` + `egraph-derive` | e-graph optimisation (path-deps; publish at I) | Tranche F, I |
| `csp-solver` | CSP solver (path-dep; publish at I) | Tranche F, I |
| `simd-scan` | SIMD scanner kernels (workspace-internal) | Tranche F |

Net: **24 workspace members** (down from 33).

## Tranche-set impact

| Tranche | Old scope | New scope (under amendment) |
|---|---|---|
| A | 33-crate scaffold; 9 per-grammar skeletons | 24-crate scaffold; ZERO per-grammar skeletons |
| B | unchanged | unchanged |
| C | unchanged | unchanged |
| D | unchanged | unchanged |
| **E** (the convergent pivot) | per-grammar declaration crates + runtime template + direct-projection emit | **`bbnf-host-prims` + `bbnf-runtime-template` + template-emitted per-grammar runtime subdirs + direct-projection emit + Lock 14 metadata-driven host-fn composition** |
| F | unchanged | unchanged |
| G | unchanged | unchanged |
| H | unchanged | unchanged |
| I | unchanged | unchanged |
| J | per-grammar declaration crates carry per-grammar benches + fixtures | benches template-emitted from `bbnf-bench`; fixtures live under `bbnf-test-fixtures/<name>/`; no per-grammar Rust |

## Tranche E — the corrected convergent pivot

Under the amendment, Tranche E remains the architectural keystone but its substrate identity sharpens:

> Lock 1 (tape + columnar dead) + Lock 13 (no god directories) + Lock 14 (full grammar generalisation; **zero per-grammar crates**) retire AS ONE through:
>
> 1. `bbnf-runtime-template` — grammar-agnostic generator emitting per-grammar runtime subdirs from (grammar source + metadata)
> 2. `bbnf-host-prims` — generic primitive library; per-grammar host-fn logic expressed as metadata-declared composition or extended-BBNF directives
> 3. Direct-projection emit completing OpenFrame retiral workspace-wide
> 4. Reshaped Emitter trait consuming the 22-variant typed IR uniformly across Rust + TS + WASM

The future-grammar onboarding test (E.W?) verifies: adding `yaml.bbnf` requires source file + metadata block. Nothing else.

## Master-plan sections superseded

Lines in `MASTER-PLAN.md` that name per-grammar declaration crates are superseded by this amendment:

- §13-17 (executive summary): "per-grammar declaration crates" → strike; replace with "metadata-driven grammar onboarding"
- §38 (Pass B summary): "9 per-grammar runtime directories that retire for template emission" → retain; the wording is correct (template-emitted subdirectories under `bbnf-runtime/src/grammars/`, not per-grammar crates)
- §79-89 (table of new crates): rows for `crates/bbnf-meta/`, `crates/json/`, `crates/css-l4/`, `crates/google-sheets/`, `crates/bnf/`, `crates/csv/`, `crates/ebnf/`, `crates/css-pretty/`, `crates/math/` → strike; replace with `crates/bbnf-host-prims/` row + `crates/bbnf-test-fixtures/` clarification
- §93, §99, §111 (reconciliation footnotes): strike per-grammar-crate naming-convention discussion
- §157 (Cargo.toml `[members]`): strike 9 per-grammar member entries; add `bbnf-host-prims` entry
- §719, §722, §766, §785 (per-grammar references): re-anchor to `bbnf-runtime/src/grammars/<name>/` (template-emitted) + `bbnf-host-prims`
- §876, §887, §898, §909, §920, §931, §942, §953, §964 (per-grammar metadata `output_dir`): re-anchor to `crates/bbnf-runtime/src/grammars/<name>/`
- §973 (per-grammar Cargo.toml uniform shape): strike (no per-grammar Cargo.toml exists)
- §1215 (calendar): tranche E unchanged in calendar but identity sharpens
- §1288, §1296, §1325, §1371, §1377: re-anchor every "per-grammar declaration crate" to its corresponding generic-substrate replacement

A reconciled MASTER-PLAN.md will be re-issued post-amendment if the user requests; for now this amendment is the authoritative supersedence and tranche-drafting agents read both documents.

## Tranche-drafting discipline under amendment

Every tranche-drafting agent dispatched after this amendment:

1. Reads `restart/audit/master-plan/MASTER-PLAN.md` for context
2. Reads `restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md` (this document) for **authoritative** workspace shape
3. Where the master plan and the amendment disagree, **the amendment wins**
4. The amendment's 24-member workspace + zero-per-grammar-crate stance is the design contract
5. Lock 14's two-surface onboarding (source file + metadata) is the verifiable invariant; the future-grammar test (yaml.bbnf) gates the tranche-set's Lock 14 closure

## Closing posture

Hereupon the per-grammar-crate misstep is retracted. The workspace is genuinely greenfield — fully grammar-driven, fully agnostic, ratifiable by the future-grammar test in two surfaces. The convergent pivot at Tranche E sharpens; `bbnf-host-prims` joins the new-facility ledger; the 9 per-grammar crates dissolve into template-emitted subdirectories + metadata-declared compositions. Lock 14's intent — *zero overfitting* — is honoured by construction, not by the safety valve it grants.
