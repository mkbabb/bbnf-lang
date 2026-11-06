# LOCK-5 · THE REGEX CONTINUUM — PROTO-1

**Probe 1 — ONE-HIR match-arm tax + structural-nullability parity oracle.**
Family 5A↔5B (Axis-1: type-count decision). Pass 1.

---

## QUESTION

Does merging regex atoms into the grammar HIR (`ir::ExprKind`) tax the grammar
passes **TRIVIALLY** — every new arm a structural one-liner reusing an existing
grammar construct → **5A/5D dominate, the regex functor is unneeded** — or does
it **LEAK** regex semantics into those passes — any arm needs regex-specific
dispatch (`analyze()`, a scalar-byte type unlike any grammar leaf) → **5B's
functor is justified**? This collapses Axis 1, which gates every downstream lock.

## METHOD

Worktree of bbnf-lang @ `d2b32f6e3` — the commit the brief's line numbers were
cut against (`ExprKind` has 8 variants; no `Project`/`Discard`; validator still
hard-rejects nullable repeats). `skinny/` is its own 12-member cargo workspace.
No x86; native build; single cargo per target dir (worktree-local `target/`).

**(A) Tax census.** Promoted `ByteSet256` out of `bbnf-regex` (added `serde`
derive; re-exported from `ir`) and added two `ExprKind` variants —
`Class { set: ByteSet256, negated: bool }` and `Look { kind: LookKind }`
(`LookKind` mirrors parse-that's read-only `regex::hir::Look`: 6 zero-width
anchors). Then iterated `cargo build --workspace`, grepping `E0004
non-exhaustive`, fixing each broken match with the smallest total arm, rebuilding
to surface the next crate (cargo halts at the first crate with errors, so
`ir`→`passes`→`codegen` surface in dependency order). Each new arm graded
**TRIVIAL-TOTAL** (structural one-liner reusing a grammar construct) vs
**REGEX-LEAK** (needs regex-specific dispatch).

**(B) Parity oracle.** New integration test
`crates/ir/tests/lock5_nullability_parity.rs` hand-builds the three json.bbnf
leaf patterns (`[ \t\n\r]*`, the quoted-string, the number) as
`Class`/`Seq`/`Alt`/`Repeat`/`Optional` subtrees. The translation mirrors the
real HIR at `parse-that/rust/regex/src/hir/mod.rs` — `Hir::{Class, Concat,
Alternation, Repetition, Literal}`, `CharClass::Bytes { ranges, negated }`,
collapsing ranges → `ByteSet256`. Runs the EXTENDED `ir::nullability` and asserts
`nullable[ws]==true`, `nullable[string]==false`, `nullable[number]==false`,
mirroring the extant string-regex oracle in `ir/src/lib.rs`.

## WHAT RAN

- `cargo build --workspace` — iterative census, converged to **GREEN (0 errors,
  all 12 members compile)** with the extended enum.
- `cargo test -p ir` — **4/4 pass.** The extant string-regex oracle
  `regex_nullability_uses_quantified_atom_shape` stays green under the extended
  enum; the new `folded_regex_atoms_preserve_json_leaf_nullability` passes.

## THE NUMBERS

### Grammar-plane `ExprKind` tax — every broken match, graded

| # | crate | site (fn) | new-arm behaviour | grade |
|---|-------|-----------|-------------------|-------|
| 1 | ir | `pretty_expr` | `Class`→`[…]`, `Look`→`^`/`$` (cosmetic) | TRIVIAL-TOTAL |
| 2 | ir | `nullability` | `Class`→`false` (like non-empty `Literal`), `Look`→`true` (like `Optional`) | TRIVIAL-TOTAL |
| 3 | passes | `infer_expr` (**type plane**) | `Class`→`Builtin(Bytes)` — the SAME leaf type `Literal` yields; `Look`→`Builtin(Unit)` — like `Annotation` | TRIVIAL-TOTAL |
| 4 | passes | `has_recovery_annotation` | leaf → `false` | TRIVIAL-TOTAL |
| 5 | passes | `has_parse_time_host_decode` | leaf → `false` | TRIVIAL-TOTAL |
| 6 | passes | `has_dispatch_overlap` | leaf → `false` | TRIVIAL-TOTAL |
| 7 | passes | `first_bytes` | `Class`→ its OWN byte-set (complement if negated), no `analyze()`; `Look`→ transparent-nullable like `Annotation` | TRIVIAL-TOTAL (simpler than `Regex`) |
| 8 | passes | `mark_transitive` (hot walk) | leaf → `{}` | TRIVIAL-TOTAL |
| 9 | passes | `lower_expr` (grammar→backend **boundary**) | `Class`→`BackendExpr::ByteClass{set,negated}` (1:1 forward); `Look`→ empty `Seq` like `Annotation` | TRIVIAL-TOTAL arm* |
| 10 | passes | `expr_has_regex_kind` | `Class`/`Look` are not `Regex` → `false` | TRIVIAL-TOTAL |
| 11 | passes | `expr_matches_literal` | leaf → `false` | TRIVIAL-TOTAL |
| 12 | passes | ref-dependency walk | leaf → `{}` | TRIVIAL-TOTAL |
| 13 | passes | `collect_literals` | leaf → `{}` | TRIVIAL-TOTAL |

`ir` 2 + `passes` 11 = **13 grammar-plane `ExprKind` arms. codegen: 0** — it
consumes `BackendExpr`, downstream of lowering, and never matches `ExprKind`.
The `derive_recognizers` site already carried a `_ => {}` wildcard, so it
absorbed `Class`/`Look` with **no edit at all**.

> ### ► Grammar-plane arms graded REGEX-LEAK: **0**

### Decisive evidence for 0 leaks

- **No new arm calls `analyze()`, `classify()`, `span_kind()`, `regex_type()`
  or touches `RegexKind`.** The folded atoms carry their semantics structurally
  (the `ByteSet256` is right there), so every pass reads them directly. The
  regex-classifier machinery stays bound to the still-present `Regex` variant.
- **The type plane — the brief's nominated leak site — is refuted.** `BuiltinTy`
  already carries `Bytes` and `U8`; `Class` reuses the EXACT type a `Literal`
  leaf produces (`Bytes`). No scalar-byte type "unlike any grammar leaf" is
  required.
- **`first_bytes` gets strictly simpler for `Class`** — the first-set IS the
  byte-set, whereas the `Regex` arm needs the heuristic `regex_first_bytes` /
  `analyze()` round-trip.

### * Backend-plane tax (plane-2, orthogonal to Axis 1)

Site 9 (`lower_expr`) is the grammar→backend boundary, not a grammar-semantic
pass. Its arm is a 1:1 structural forward, but it induces ONE backend primitive
`BackendExpr::ByteClass`, which cascades **mechanically** through each backend IR
plane — **6 further arms, every one TRIVIAL-TOTAL, none regex-specific**:
`passes` ×3 (`contains_direct_build`, `contains_tape_kind`, `alt_branch_count` —
leaf-classification), `codegen` ×3 (`tape_plan::render_expr` string-op,
`sink_only::lower_expr`, `sink_only::direct_shape`; `SinkOnlyExpr` grew a
`ByteClass{negated}` leaf). A byte-class emitter is needed by **any** design with
character classes, so **5A and 5B incur this identically** — it does not
discriminate the type-count axis.

### (B) Parity oracle

| pattern | tree | `nullable` | expected | result |
|---------|------|-----------|----------|--------|
| `[ \t\n\r]*` | `Repeat{Class,min0}` | true | true | ✓ |
| quoted-string | `Seq[Lit("), Repeat{…}, Lit(")]` | false | false | ✓ |
| number | `Seq[Opt(-), Alt[Lit(0), Seq[Class[1-9],…]], …]` | false | false | ✓ |

**3/3 parity. The extant string-regex oracle also stays green — 5A breaks no
green oracle (no refutation).**

## VERDICT

**(A) Grammar-plane REGEX-LEAK count = 0 ⇒ 5A/5D DOMINATE; the regex functor
(5B) is UNNEEDED to keep the grammar passes clean.** Folding `Class`/`Look`
directly into `ExprKind` taxes the 13 grammar-plane match sites TRIVIALLY — every
arm is a structural one-liner reusing an existing grammar construct, and not one
arm needs regex-specific dispatch. The type plane, the pass the brief flagged as
the likely leak, reuses `Literal`'s own `Bytes` type. **No leaking pass to name.**

**(B) 3/3 nullability parity; extant oracle unbroken ⇒ no refutation of 5A.**

The sole non-grammar cost is one backend primitive (`BackendExpr::ByteClass`,
+6 mechanical arms) that 5A and 5B incur identically — orthogonal to the
type-count axis. The full workspace builds green.

**VERDICT LINE: 5A/5D dominate — functor unneeded (0 grammar-plane leaks); oracle 3/3 (RAN).**

---

*No perf claims made, so no timing measured. The reported numbers are static
census counts (13 grammar-plane arms, 0 leaks; 6 backend-plane arms) and test
outcomes (4/4), all empirical from `cargo build`/`cargo test`.*
