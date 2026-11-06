MODEL/RUNTIME IDENTITY: Codex subagent; no independently recorded model or
reasoning-effort attestation was supplied to this lane.  This declaration is
metadata only and earns no MODEL-TRUTH credit.

# BB-C.W0 R7 post-fix fleet — hostile pre-run stop receipt

- Date: 2026-07-22.
- Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
- Result: **RED before build or regeneration**.  Two independent hostile
  reviews rejected the proposed generic Pratt-discriminant subject before the
  one-shot fleet command began.  Root ordered a fail-closed stop.  No fresh
  xtask was built; `regen`, `regen --check`, the four runtime regenerators,
  runtime checks, and semantic/product tests were all **UNRUN**.
- Prior fleet receipt:
  `../W0-AMBIENT-R7-ATOMIC-FLEET/AUTHOR.md`, SHA-256
  `01d235d62b0acc2b8158a40decfc2b8c24c2ff05da7507043b31d8413535ac0b`.
- Audited source:
  `crates/ir/src/passes/recognizers/grammar_facts.rs`, SHA-256
  `b89686298f2710b0170e5e0f7a70c7b153447972aca3bbda5a28ed16fb28f5e2`.
- Audited external test:
  `crates/core/tests/pratt_const_fold.rs`, SHA-256
  `43454beeb49f5b5da88a17259894b4d72cefe0dd57e7ef1ce70ed44d8da5bd85`.

## Exact terminal custody

Root held the governed source/build-input tree read-only while this lane
prepared its isolated campaign.  The hostile stop arrived before the first
snapshot/build phase.  An Apple-arm64 read-only recensus then sealed the
unchanged live operand and released the freeze:

| Datum | Identity |
|---|---|
| members | 843 |
| bytes | 15,309,199 |
| path-set SHA-256 | `9cd484942b9ee3f676ae150871f664bce5cc5dc9d8ef6b5dca791958be92c2ba` |
| ledger SHA-256 | `4e9a8f4b5db5e7e5fa46b44301abbdc55e58e38a81f5433470327b8480076a7c` |
| HEAD | `af15f63e0d2d3d719938c13b906a50acbb92ea3b` |
| porcelain-v1-z | 116,438 bytes / `6cf294d6d53800ce998a163cfbc062c1cf623f026564adfcecd10345d1a44d7c` |

The exact ledger is `BUILD-INPUTS-ABORT.tsv`, 844 lines / 105,390 bytes,
SHA-256
`4e9a8f4b5db5e7e5fa46b44301abbdc55e58e38a81f5433470327b8480076a7c`.
`ABORT-STATE.json`, SHA-256
`b332e688251b2da66f50cf8fde6fccb3f07589601c68053317694d85e0933929`,
records `build_started=false`, `regen_started=false`,
`runtime_started=false`, and terminal disposition
`RED_HOSTILE_PRE_RUN_STOP`.

The recensus command was:

```text
PYTHONDONTWRITEBYTECODE=1 /usr/bin/arch -arm64 python3 \
  restart/skinny/tranches/sk-v25/waves/BB-C/evidence/\
W0-AMBIENT-R7-POST-FIX-FLEET/RUNNER.py abort-recensus
```

No x86 command or artifact was used by this lane.  The purpose-built runner is
retained as formation evidence at SHA-256
`44df120aacfce9c3d186a81303df31d8386cf5d6d036831363ffebe86f24fdc0`;
only its `abort-recensus` phase ran.

## Why the proposed subject was rejected before product proof

The narrow Sheets intent is sound: an explicit typed `-> Nu8` constant is a
better semantic authority than transformed traversal order.  The submitted
implementation did not yet make that rule generic, transformation-stable, or
representation-safe.

### 1. `Next` / `Skip` normalization erases mapped authority

`match_operator_chain_rule` normalizes the repeat inner through
`strip_transparent_owned` at `grammar_facts.rs:1063`.  That helper recursively
turns `Next` and `Skip` into `Seq` while stripping child `Map` nodes at
`grammar_facts.rs:1329-1338`.  Consequently a topology such as
`Next(Map(Literal("+"), 7u8), operand)` reaches operator extraction as a bare
literal and receives the ordinal fallback instead of `7`.  Equivalent grammar
semantics can therefore acquire different Pratt tags depending on whether a
map remains behind a `Ref`/`Seq` or is normalized through `Next`/`Skip`.

### 2. Operator spelling silently truncates after two bytes

`precedence_entries` reads the first byte and optional second byte at
`grammar_facts.rs:1368-1373`, but it admits any non-empty literal.  The Pratt
metadata and emitter represent only those one or two bytes.  A three-byte or
multi-byte UTF-8 operator can therefore be admitted with a truncated spelling.
The representation must either generalize or fail closed outside its exact
supported byte width; this subject does neither.

### 3. Pratt and direct emitters disagree on constant authority

`mapped_u8_discriminant` clones and constant-folds any typed-U8 expression at
`grammar_facts.rs:1385-1397`.  The keyword and flat struct-direct emitters use
raw `MapExpr::IntLit` arms as their tag-bearing surface.  A foldable unary or
binary expression can therefore yield a Pratt semantic tag while its direct
consumer follows a different/no-tag route.  One normalized extractor must own
both surfaces, or Pratt must accept only the same exact raw shape.

### 4. `u8` semantic value and branch-index transport are conflated

Pratt emits its `op_discriminant` through `StructBuilder::push_branch_tag`.
That method is documented as an Alt branch index, while the generic tape
record reserves a six-bit branch field (`runtime/tape/record.rs:91,187`).
Sheets' current `0..=5` constants fit, but the proposed generalized U8
authority admits `0..=255`.  Until the transport contract is separated or
bounded and proved across every builder, the generalized claim is unsafe.

## Additional hostile debt carried into the repair

- Bare literals still derive observable tags from post-transformation ordinal
  (`grammar_facts.rs:1364-1379`), not source provenance.
- Associativity is lexeme-hardcoded: every `"^"` is right-associative at
  `grammar_facts.rs:1497-1503`, regardless of the declaring grammar.
- `Map` extraction now rejects every mapped operator not expressible as an
  explicit constant typed U8.  A Pratt-classified rule must demonstrably fall
  back to the generic parser rather than remain selected with an empty or
  incomplete operator table.
- The new test lives in a core aggregate rather than the source-isomorphic IR
  test tree, and does not cover direct/inlined Map, `Next`, `Skip`, nested map,
  supported byte-width boundaries, bare-source provenance, or exact generated
  Sheets direct/Pratt/runtime parity.  Its out-of-range assertion proves only
  that the named Pratt fact is absent, not why it was rejected.
- The source comment at `grammar_facts.rs:1411-1414` calls Sheets comparison a
  seven-literal chain; it has six alternatives.

These are generic engine defects, not requests for Sheets-specific branches,
generated-text edits, shims, or grammar renumbering.

## Product and worktree disposition

The previous fresh fleet remains the last executed product receipt and remains
RED at Pratt mapping.  This aborted lane grants no fleet, fixed-point,
semantic, runtime, test, profile, mutant, lock, wave, tranche, or movement
credit.  There was no candidate copy, generated output, runtime output,
package, manifest, lockfile, test, stage, commit, parse-that, Value, Glass,
SCI, Atlas, or other-repository mutation.  The only durable writes are this
append-only evidence directory.

The next lawful subject must preserve map provenance through every chain
normalization, enforce an exact operator-spelling representation, share one
constant-discriminant contract across direct and Pratt consumers, and prove
the tag transport bound.  Only after two fresh hostile passes accept that
source may a new one-shot Apple-ARM fleet campaign begin.

**R7 POST-FIX FLEET: RED BEFORE BUILD/REGEN. GENERATED 0. RUNTIME 0. TESTS 0.
GOVERNED FREEZE RELEASED AFTER EXACT 843-MEMBER RECENSUS.**
