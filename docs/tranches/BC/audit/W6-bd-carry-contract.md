# BC.W6 — BD Carry Contract

Date: 2026-05-03
Status: settled. Closes surgery 33 (`audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:73`) and BC02-4 (`audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:62`).

## §1 Mandate

The BD tranche is drafted by the sister Phase-4 BD agent (per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:312` `BD: 0 (drafted from scratch) → 5-7 (W0-W4 or W0-W6) → ~1500-2500 lines`). This document specifies what BD must accept from BC. The BD agent reads this and drafts BD's W0/W1 to receive each carry with concrete close-gates.

This document does NOT touch BD files. It specifies the contract that the BD agent's draft must honour.

## §2 Carry table

| Carry tag | Producer wave (BC) | Receiver wave (BD) | Blocker (concrete dependency) | Receiving gate (cargo / shell command) |
|---|---|---|---|---|
| BC→BD.C1 | BC.W2 (TS + WASM scaffolds) and BC.W4 (visitor surface emit) | BD.W1 (TS/WASM activation) | TS/WASM ABI choice + host-fn resolution table design (host fn names per backend; argument marshalling rules; error propagation idiom) | `cargo nextest run -p bbnf-codegen-ts -p bbnf-codegen-wasm --test ts_e2e_json --test wasm_e2e_json` 100% pass; both backends parse twitter.json end-to-end with matching output |
| BC→BD.C2 | BC.W5a (sister crate API freeze) and BC.W5b (bbnf-regex endpoint reconciliation) | BD.W2 (sister crate publication) | crates.io publication tooling (`cargo publish` smoke run; semver-checks; docs.rs verification); workspace member metadata complete (license, description, keywords, categories) | `cargo publish -p egraph -p egraph-derive -p csp-solver -p bbnf-regex` succeeds (or in dry-run mode `cargo publish --dry-run` succeeds with semver-checks integration verifying minor-version bump rules) |
| BC→BD.C3 | BC.W5d (worktree fixture closure) | BD.W0 (parallel-agent worktree infrastructure) | `xtask worktree-init` materialises every grammar's data + rewrites; per-worktree symlink discipline; sibling repo path discovery | `xtask worktree-init` runs cleanly across the BD parallel-agent dispatch matrix; sample worktree boots with materialised `data/{json,css,bbnf,sheets}` + `grammar/<name>/rewrites/*.ron` + sibling sym-links resolved |

## §3 Per-carry detail

### BC→BD.C1 — TS/WASM activation

**What BC produces** (BC.W2 + BC.W4):
- `crates/bbnf-codegen/src/ts/` and `crates/bbnf-codegen/src/wasm/` with `Emitter` trait impls compiling against typed IR
- Trivial-grammar smoke test for CSV (one of BNF/CSV cohort) producing non-empty syntactically-valid output
- JSON `object` rule reference emit per `audit/RESTART-SKETCH-2026-05-03.md:559-577`
- Host-fn graceful failure (TS: `throw new Error(...)`; WASM: `unreachable`)
- Cross-backend visitor surface (`visit_<Name>` per record kind; method-name set isomorphic across Rust / TS / WASM modulo idiom)

**What BD must produce at receiving gate**:
- Host-fn resolution table per backend: TS uses `runtime: { parseHexColor: (s: string) => CssColor, ... }` injection at `bbnf.parse(input, runtime)`; WASM uses indexed `(import "host" "parse_hex_color" (func $extern_0 ...))` with the host wiring its function table
- Production TS scaffold runtime: `import { JsonParser } from 'bbnf-ts'; const v = JsonParser.parse(twitter)`. Output matches Rust output for the same input (typed-enum equivalence)
- Production WASM scaffold runtime: WASM module exports `parse_<grammar>(input_ptr, input_len) -> output_ptr`; runtime wrapper marshals strings; output matches Rust
- End-to-end test: `cargo nextest run -p bbnf-codegen-ts --test ts_e2e_json` parses twitter.json in TS and asserts deep equality with Rust output

**Blocker resolution**: the ABI choice is BD scope (Bun + Deno + Node.js native modules; wasm-bindgen vs wit-bindgen; NAPI for Node integration). BD's research mandate covers this per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:280`.

### BC→BD.C2 — Sister crate publication

**What BC produces** (BC.W5a + BC.W5b):
- `egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex` API frozen
- `cargo doc -p <crate>` clean for each
- `cargo publish --dry-run -p <crate>` clean for each
- Per-crate publication readiness audit at `docs/tranches/BC/audit/W5-publication-readiness.md`
- Endpoint reconciliation lands `bbnf-regex` at `parse-that/rust/bbnf-regex/`

**What BD must produce at receiving gate**:
- `cargo publish -p egraph -p egraph-derive -p csp-solver -p bbnf-regex` to crates.io (in publication order: egraph-derive (proc-macro) before egraph; csp-solver and bbnf-regex independent)
- `cargo install` from crates.io for each crate succeeds in a fresh container
- docs.rs build succeeds for each crate; the published doc URL is reachable
- semver-checks integration: every minor version bump preserves the public API; major version bump requires changelog entry

**Blocker resolution**: crates.io publication credentials, semver-checks tooling integration with `cargo workspaces` or similar; per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:280` the BD research mandate covers `cargo-release / semver-checks / docs.rs publication ergonomics`.

### BC→BD.C3 — Worktree fixture infrastructure

**What BC produces** (BC.W5d):
- `xtask worktree-init` materialises:
  - `data/{json,css,bbnf,sheets}/` (test datasets per grammar)
  - `grammar/<name>/rewrites/*.ron` per grammar (rewrite rule fixtures)
  - sibling sym-links: `parse-that/`, `pprint/`, `csc411/`, `bbnf-buddy/`, `ffuzzy/`
- Sample worktree boot test: a fresh worktree clone runs `xtask worktree-init && cargo check --workspace` clean

**What BD must produce at receiving gate**:
- Parallel-agent dispatch infrastructure consuming the worktree contract: BD's drafting model is N parallel agents each in its own worktree; each agent's worktree has the materialised fixtures
- BD's W0 closer-gate names the worktree-init command and asserts a multi-worktree boot succeeds in parallel

**Blocker resolution**: the parallel-agent dispatch is BD's own substrate; BC.W5d hands BD a working `xtask worktree-init` and BD wires the dispatch around it.

## §4 Receiving-wave naming convention

| Carry tag | BD receiving wave |
|---|---|
| BC→BD.C1 | **BD.W1** (TS/WASM activation) |
| BC→BD.C2 | **BD.W2** (sister crate publication) |
| BC→BD.C3 | **BD.W0** (parallel-agent worktree infrastructure; foundational for the rest of BD's waves) |

The BD agent's draft must place these gates at exactly these waves. If BD's planner needs to reorganise (e.g., merge W1 and W2), the BD agent must amend the carry-tag receiving wave names accordingly and notify the synthesis pass.

## §5 Closing posture

The BD agent reads this contract, drafts BD's W0/W1/W2 to receive each carry with the named close-gates, and verifies via in-document references back to this artefact. The synthesis pass at `audit/PHASE-4-SYNTHESIS-2026-05-03.md` (drafted post all four tranches) checks that BD's drafted waves cite this artefact's carry-tag rows and that each cited row's receiving gate is a verbatim cargo / shell command.

This artefact is settled. BC closes with the BD carries explicitly named per receiver wave + blocker + receiving gate, satisfying Operational Rule 2.
