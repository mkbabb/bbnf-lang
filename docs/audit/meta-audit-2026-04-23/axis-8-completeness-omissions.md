# Axis 8 — Completeness and Omissions

Current read-of-record for this audit: `5a260f94`.

Scope audited:

- `docs/tranches/meta-audit/*`
- `docs/tranches/B1/*`
- `docs/tranches/AZ-I/*`
- `docs/tranches/AZ-II/*`
- `docs/tranches/BA/*`
- `docs/tranches/BB/*`
- `docs/GESTALT.md`
- `docs/instructions/*`
- live repo surfaces under `scripts/`, `grammar/`, and `crates/ir/src/vm/`

This axis asks the simple question the others do not: what did the
audit still fail to cover, or cover only partially, even if its
conclusions were broadly useful?

## Verified

### 1. The abrogation catalog did cover the live script surface

The working tree currently has 19 top-level files under `scripts/`.
A direct name check shows all 19 appear somewhere in
`docs/tranches/meta-audit/08-abrogation-catalog.md`.

So this is not a case where the audit missed obvious shell surfaces.
The catalog is complete at the script-name level.

### 2. The prior Era V residue the plan talks about is still where the plan says it is

The live tree still contains the VM residue:

- `crates/ir/src/vm/bytecode.rs`
- `crates/ir/src/vm/compiler/`
- `crates/ir/src/vm/interpreter/`

And the hand-written rule lineage is still visible:

- `inline_acyclic` is live at
  `crates/ir/src/passes/transform/inline.rs`
- factoring passes remain live under
  `crates/ir/src/passes/prefix.rs` and
  `crates/ir/src/passes/transform/fuse_token/factor.rs`
- `merge_regex_alts` survives only as a deleted historical reference in
  `crates/ir/src/egraph/rules/regex.rs` and `crates/ir/src/passes/mod.rs`

That means the audit was not hallucinating the surviving Era V/Tranche H
substrate. The residue is real.

### 3. The tranche-instruction subdocs were not silently dropped

The streamlining pass did **not** rewrite the whole instruction stack,
but the tranche subdocs are present and wired:

- `docs/instructions/tranche/START.md`
- `docs/instructions/tranche/README.md`
- `docs/instructions/tranche/RESEARCH.md`
- `docs/instructions/tranche/SPEC.md`
- `docs/instructions/tranche/WAVE_SPEC.md`
- `docs/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md`

`docs/instructions/CHANGELOG.md` is explicit that the 2026-04-23
streamlining target was `README.md` and `PROFILING.md`. So the audit
did not silently lose tranche protocol material. It simply did not fold
that layer into the same pass.

## Refined

### 1. The grammar universe is broader than the plan surface usually admits

The production plan centers four grammars:

- JSON
- CSS L4
- Google Sheets
- BBNF

But the working tree's `grammar/` surface is broader:

- `grammar/bnf/bnf.bbnf`
- `grammar/ebnf/ebnf.bbnf`
- `grammar/misc/{csv,emoji,g4,json-commented,math,math-ambiguous,regex}.bbnf`

That does not make the plan wrong. It does mean the audit should have
named the scope boundary explicitly:

- which grammars are tranche-owned production targets
- which grammars are fixtures, experiments, or deferred consumers

Right now that distinction is implicit.

### 2. The appurtenant-repo assay mixes live modernization work with historical residue

The broad fleet view is useful, but some rows are already historical:

- `../parse-that` exists and is audit-relevant
- `../pprint` exists and is audit-relevant
- `../csc411/.../csp-solver` exists and is audit-relevant
- `../gorgeous` does **not** exist on the current host

So the audit's broader modernization logic is still helpful, but
"gorgeous sibling retirement" is no longer a planning problem in the
same way the other siblings are. That row should have been explicitly
labeled historical residue or already-removed surface.

### 3. The brief's "nine questions" framing is stale relative to the live capstone

The commissioning brief asks whether "all nine questions are resolved."
The live capstone says:

- `docs/GESTALT.md:1144` —
  `## 10. Decision record — the ten questions that gated the plan`

That discrepancy is minor, but it is another example of why this
meta-audit cannot inherit the brief's numbers without verification.

## Flawed

### 1. The audit never wrote the grammar-scope appendix the plan now needs

Given the live `grammar/` tree, the audit should have produced one
explicit table naming:

- in-scope production grammars for AZ-I / AZ-II
- deferred grammars
- experimental grammars
- which grammars are expected to fail `StructRegistry` closure on first
  pass and why

Without that table, "grammar-derived everything" reads stronger than the
actual tranche ownership surface supports.

### 2. The audit did not actually answer the user's "are our tests stale?" question

It answered adjacent questions:

- the runner surface should move to nextest
- CI should stop using plain `cargo test --workspace`
- parity harnesses exist and are valuable

But it did **not** perform a test-freshness audit in the literal sense:

- no fixture-drift review
- no stale-assertion pass
- no "tests that still pass but no longer constrain the intended
  architecture" inventory

That omission matters because the user's dev-loop complaint was not just
speed. It was speed **and correctness**.

### 3. Several "resolved" decisions in GESTALT are still missing their live owning artefacts

Three examples are enough to show the pattern:

1. `docs/GESTALT.md:1191-1194` says cross-worktree pin drift is
   CI-guarded by a shared workflow. `docs/tranches/B1/B1.md` only
   requires mirrored `rust-toolchain.toml` files; the workflow itself is
   not present in the plan surface.
2. `docs/GESTALT.md:1210-1212` cites
   `docs/tranches/AZ-I/CLASSIFIER-UNIFICATION.md`, which is missing.
3. `docs/GESTALT.md:1210-1212` and `:1222` cite
   `docs/tranches/AZ-II/BOOTSTRAP-CUTOVER.md` and old `AZ` docs, which
   are also missing.

This is a completeness problem because the capstone presents these as
available decision anchors.

### 4. The instructions streamlining story did not fully explain its own scope boundary

`docs/instructions/CHANGELOG.md` is internally clear. The broader audit
story is less clear. A reader could easily infer that "the instructions
were streamlined" means the whole instruction surface was normalized.

In reality:

- top-level operational docs were streamlined
- tranche-law docs remain separately maintained

That is a valid choice, but the audit should have named it as a
deliberate boundary instead of leaving the reader to infer it.

### 5. The audit did not produce the current-state toolchain probe it needed before hard-freezing B1

This is the omission underneath several Axis 3 flaws.

Before freezing:

- `nightly-2026-04-11`
- Cranelift enablement
- hard-coded linker guidance

the audit needed a small probe artefact answering:

- does the pinned nightly clear the current ICE reproducer?
- is `rustc-codegen-cranelift-preview` available on that exact pin and
  host?
- what linker path is actually valid on the audit host?

Instead, the docs partly guessed and left Wave 1 to carry the
correction later.

## Open

### 1. A production-vs-experimental grammar table is still missing

This is the single biggest documentation omission left by the audit.
Without it, future orchestration will keep re-reading the grammar tree
from scratch or silently assuming the four-grammar production set covers
everything the repo means to support.

### 2. A literal test-freshness pass is still outstanding

There should be a follow-on audit that answers:

- which tests still constrain current architecture
- which tests are legacy shims
- which parity harnesses or fixture corpora are stale against current
  competitor versions or grammar semantics

That work was not done here.

### 3. The capstone needs its missing cited artefacts either created or clearly marked as future outputs

At minimum:

- `AZ-I/CLASSIFIER-UNIFICATION.md`
- `AZ-II/BOOTSTRAP-CUTOVER.md`
- dead `AZ/` and `BC/` citations

must be normalized. A capstone cannot be a reliable navigation index
while pointing at missing files as if they are present.

## Axis 8 Verdict

The audit was broad, and it did not miss the obvious script, repo, or
Era V residue surfaces. Its omissions are subtler and more operational:

- it did not bound the grammar universe explicitly
- it did not actually audit stale tests
- it did not ensure that every "resolved" capstone decision had a live,
  present owning artefact

So the right judgment is: **the audit was substantially complete as a
planning synthesis, but incomplete as an execution inventory. Before the
plan becomes canon, it needs one more layer of concrete scoping:
grammar scope, test freshness, and live-vs-future artefact labeling.**
