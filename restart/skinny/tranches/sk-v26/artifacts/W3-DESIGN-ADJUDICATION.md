# W3 pre-freeze design adjudication

Task: `/root/skv26_w3_design_adjudicator`.

Disposition: `ACCEPTED_DESIGN`.

Evidence basis:

- BBNF W2 admission `686b7d51582c7817067e89286d52fc0d7327f548`,
  tree `780d1af636d691aeba1ee93344903595e2c53416`;
- parse-that `282ccb6f6da15ef1e5b355b5588a23cc6a76ae7f`,
  tree `4e813a353251f86906baee44a9c4d28e743fe27b`;
- all five sealed pool grammars, inspected without calculating or naming the
  held-out selection.

No P0, P1, or P2 design gap remains.

## Choosy reduction

The accepted design keeps:

- clean-break explicit grammar instances and entry selection;
- one parse-that transactional difference primitive at version `0.5.0` and
  ABI 3;
- the historical action-free syntax union;
- data-driven manifest-attached Pratt;
- one generic external public/selected witness harness;
- all candidate manifests and cases presealed but unexecuted;
- hostile pre-freeze review;
- git-object freeze and mutation-tested post-freeze zero-diff verification.

It rejects:

- normalization or automatic recovery metadata;
- imports, legacy `@ws`, `@recover`, actions, host arrows, callbacks, native
  code, dependencies, features, plugins, grammar functions, unary negation,
  backend or format identity;
- a duplicate `@pratt` source directive;
- separate projection opcodes;
- trailing-only trivia;
- a duplicate Pratt-operator event;
- empty plugin or dispatch registries;
- compatibility `ModuleInput` or `compile_module` paths.

Plugin and format dispatch are frozen as verified absence, not represented by
empty production registries.

## Witness authority correction

W3 corrects but does not rewrite W0 with:

- `artifacts/W0-PUBLIC-WITNESS-ADDENDUM.tsv`;
- `artifacts/W3-PUBLIC-WITNESS-CASES.tsv`.

Stable witness IDs:

- `PUB-JSON-EVENT-V1`;
- `PUB-CSS-EVENT-V1`;
- `PUB-SHEETS-PRATT-EVENT-V1`.

The addendum header is:

```text
witness_id	grammar_path	grammar_sha256	instance_path	instance_sha256	semantic_plane	feature_id	case_ledger_path	case_ledger_sha256	recovery_policy	disposition
```

The case header is:

```text
case_id	witness_id	class	input_path	input_sha256	expected_path	expected_sha256	disposition
```

Case class is `POSITIVE`, `NEGATIVE`, `RECOVERY_POLICY`, or
`FULL_CONSUMPTION`. Exact grammar, manifest, case, and expected bytes are
hashed before freeze.

JSON covers recursive sums, escapes, its exact declared number language,
trivia, rollback, and full consumption. CSS covers nested regions, comments,
both quote forms, grammar error-token continuation, rollback, and full
consumption. Sheets covers prefix/postfix/infix, precedence, both
associativities, parentheses, trivia, rollback, and full consumption.

JSON and Sheets use evidence policy `ATOMIC_REJECT`. CSS uses
`GRAMMAR_ERROR_TOKEN_CONTINUE`, expressed by an ordinary action-free inert
transform such as `@error(child)`. Runtime recovery remains `Disabled`; the
compiler never switches on the transform name.

All five W0 candidate manifests and stable positive, negative,
recovery-policy, and trailing-input cases are also sealed before freeze.
They are not compiled or executed before selection.

## Strict instance boundary

The clean-break API is:

```rust
pub struct GrammarInstanceInput<'a> {
    manifest_bytes: &'a [u8],
    source_bytes: &'a [u8],
}

pub fn compile_instance(
    input: GrammarInstanceInput<'_>,
    options: CompileOptions,
) -> StageResult<CompiledModule>;
```

The manifest requires exactly:

```json
{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "grammars/example.bbnf",
  "entry": "document",
  "trivia_regex": "[ \\t\\r\\n]*",
  "pratt_tables": []
}
```

`trivia_regex` may be null. Pratt tables contain `site_rule`, `atom_rule`,
and ordered operator rows containing `matcher_rule`, `fixity`,
`left_binding_power`, and `right_binding_power`.

Every object rejects additional properties. Validation rejects duplicate or
missing JSON keys, size/budget excess, invalid paths/names, absent entry,
invalid or non-nullable trivia, `?w` without trivia, unused non-null trivia,
duplicate or unresolved Pratt rows, non-consuming atom/matcher languages, a
site root that is not exactly its atom reference, invalid fixity/power shapes,
and every forbidden metadata dimension.

Binding-power shapes:

- prefix: null left, right `1..=65535`;
- postfix: left `1..=65535`, null right;
- infix: both present; right equals left for right associativity or exactly
  `left + 1` for left associativity.

Manifest order assigns compact contiguous revision-bound table/operator IDs
and deterministic overlap priority.

New compile budgets cover instance bytes, Pratt tables, Pratt operators, and
presentation hints. New diagnostics are `InvalidGrammarInstance`,
`UnknownEntry`, and `InvalidPrattTable`.

## Canonical revision

Revision uses domain `bbnf.w3.revision.v1\0` and does not hash raw JSON.

It hashes fixed-order typed fields: schema, logical path, entry, trivia tag and
pattern, table count, each site/atom/operator row in manifest order, one-byte
fixity and optional little-endian u16 powers, then source length and exact
source bytes. String/byte lengths are little-endian u64; collection counts are
little-endian u32.

JSON key order and insignificant whitespace do not change Revision. Every
semantic field or source change does. Logical path exists only in the
manifest. Complete replay stores the typed instance plus source and rebuilds
from canonical data.

## Frontend and semantic operations

Expression precedence is:

1. primary, group, brace, bracket;
2. postfix `?w`, `?`, `*`, `+`, recognizing `?w` first;
3. one left-associative binary tier containing `<<`, `>>`, and `-`;
4. comma concatenation;
5. choice.

Grammar-source `//` and non-nested block comments are transactional lexical
trivia outside literals and regexes. They are never pre-stripped or reparsed.
Braces lower to zero-or-more; brackets lower to zero-or-one.

`@pretty target hints...;` is structured source metadata with a finite
vocabulary: `group`, `block`, `indent`, `dedent`, `softline`, `hardline`,
`blankline`, `compact`, and `sep(<literal>)`. Targets and hints validate.
Presentation rows occur in source replay only.

The generic source and plan operations are:

```text
Projection { left, right, keep }
Difference { include, exclude }
Trim { child }
Pratt { atom, table }
```

Projection recognizes both sides and preserves all scalar events, but its type
is the kept side. Difference type is include. Trim type is child. Pratt type is
atom. Every discarded or excluded child remains a public dependency,
derivation, fact, overflow, and work participant.

Analysis laws:

- Projection nullable is conjunction; minimum is checked sum.
- Trim inherits child facts because trivia is nullable.
- Difference is nullable only when include is nullable and exclude is not;
  nullable exclusion makes the language empty; otherwise minimum
  conservatively inherits include.
- Pratt inherits atom facts, followed by nonempty atom/matcher validation.
- No FIRST carrier or unsupported exact language-difference claim is added.

Lowering stores one optional compiled trivia DFA and compact Pratt
tables/operators. It introduces no runtime grammar graph, callback, trait
object, or format switch.

## parse-that difference

The sole new public primitive is:

```rust
pub fn difference<'i, S, T, U, P, E>(
    run: &mut Run<'i, S>,
    primary: P,
    excluded: E,
) -> Step<'i, T>
```

It checkpoints, probes exclusion first, validates every foreign outcome, and
always rolls the exclusion cursor/events back. Exclusion match returns an
entry-position mismatch; exclusion mismatch runs primary from entry;
exclusion fault propagates without invoking primary. Primary mismatch/fault
rolls back the whole operation; primary match commits. Exclusion diagnostics
never contaminate primary diagnostics.

Cursor and sink are transactional; generated work accounting remains
monotone.

Advance parse-that to `0.5.0`, ABI 3, exact BBNF pin/assertion, and the runtime
bridge together. bbnf-regex remains `0.1.0`.

Born-RED laws cover both exclusion outcomes and faults, foreign provenance,
cursor restoration, speculative-event rollback, retaining/NoSink parity,
zero allocation, and proof that primary is never invoked after exclusion
match or fault.

## Genuine Pratt

Pratt tables and operators use compact private ordinals plus full-revision
public capabilities and nameable views. Tables store site, atom, and operator
range. Operators store table, matcher rule, fixity, and binding powers.

Generated direct control executes:

```text
parse_expr(min_bp):
    transact the invocation
    charge work and nesting
    choose the first matching eligible prefix row in manifest order
        match => parse_expr(right_bp)
        none => parse atom
    loop:
        choose first matching eligible postfix/infix row
            eligibility: left_bp >= min_bp
            failed probes roll back and try the next row
            no row => finish
        postfix => reduce
        infix => parse_expr(right_bp)
                 missing RHS rejects the whole expression
        emit PrattReduce only after commit
```

Required laws cover all fixities, precedence, both associativities,
parentheses through recursive atom reference, manifest overlap priority,
consuming atom/matchers, transactional probes, complete-expression rollback
on missing RHS, exact-cap/one-under work/nesting/events, and retaining/NoSink
parity.

Add only `EventKind::Trivia` and `EventKind::PrattReduce`.

Trivia is emitted only for nonempty leading or trailing consumption, with Trim
node identity, exact span, and value 0 or 1. PrattReduce carries Pratt node,
exact operator ID, complete reduced span, and value 0. Generated
`ScalarEvent` gains `operator: Option<u32>`, absent elsewhere.

Generated `PRATT_TABLES` and `PRATT_OPERATORS` descriptors expose only stable
IDs, rule identities, ranges, fixity, and binding powers.

## Version matrix

| Surface | W3 identity |
|---|---|
| BBNF | `0.4.0` |
| parse-that | `0.5.0`, ABI 3 |
| grammar instance | `bbnf.grammar-instance.v1` |
| Revision domain | `bbnf.w3.revision.v1` |
| source map | `bbnf.source-map.v4` |
| program | `bbnf.program.v3` |
| node remap | `bbnf.node-remap.v1` if byte/meaning-identical |
| dependency | `bbnf.dependency-solution.v2` |
| type solution | `bbnf.type-solution.v3` |
| analysis facts | `bbnf.analysis-facts.v4` |
| lowering | `bbnf.lowering-plan.v3` |
| scalar events | `bbnf.scalar-events.v5` |
| generated Rust | `bbnf.generated-rust.v6` |
| generated replay | `bbnf.generated-module.v5` |
| complete replay | `bbnf.compiled-module.v6` |

## Generic harness and falsifiers

One external contract lexically enumerates manifests, invokes only
`compile_instance`, emits ordinal module names and an automatically generated
function table, compiles against exact parse-that, runs retaining/NoSink,
ordered-event, replay, positive, negative, recovery-policy, and
full-consumption gates, and generates byte-identically into two empty roots.
It contains no format switch, feature, callback, handwritten parser, or
per-format runtime directory. Its selected-instance directory is empty before
freeze.

The package gate constructs a temporary local Cargo registry from freshly
packaged exact parse-that, bbnf-regex, BBNF, and required cached archives. The
external consumer uses registry versions only: no path, patch, or workspace
inheritance.

Pre-freeze falsifiers include explicit-entry-vs-root, missing-entry rejection,
manifest reformat identity, semantic-field revision separation, full
rule/path renaming, rule-order permutation, misleading labels, delimiter
twins, a neutral full-feature grammar, forbidden-field rejection, nullable
difference exclusion, speculative-event absence, operator-order and
binding-power mutation, exhaustive generic-root/output scans, one harness,
and candidate preseal without execution.

Freeze verified plugin/dispatch absence in
`artifacts/W3-FORBIDDEN-SURFACE-ABSENCE.tsv`.

## Freeze and post-freeze proof

After public witnesses pass two fresh hostile reviews and adjudication, commit
a freeze basis. `W3-FREEZE.tsv` records repository, commit, tree, path, mode,
git object, SHA-256, and disposition for both repository trees, compiler and
runtime source, manifests, locks, tests, schemas, harness, package verifier,
zero-diff verifier, witness/candidate packets, toolchain, and generated
outputs. It omits itself to avoid circularity.

Then commit `artifacts/W2-FROZEN-RECEIPT` alone. It fixes BBNF/parse-that/
bbnf-regex versions, trees, package bytes and hashes, freeze and witness
digests, instance schema, candidate preseal, forbidden-surface absence,
toolchain, and pre-freeze adjudication. It contains neither its own digest nor
its own commit.

Only after that receipt is git-durable may the sealed raw-byte selection rule
be evaluated.

The post-freeze verifier compares git objects with rename detection disabled,
permits only the one byte-identical presealed selected manifest as product
input, confines other additions to exact evidence roots, verifies selected
source against W0, requires clean tracked/untracked state and dual-root
generation identity, and replays every selected case plus a clean registry
consumer.

Mutation self-tests separately reject content, executable-bit, add, delete,
rename, symlink substitution, dependency/feature, new generic source, second
selected manifest, and parse-that-tree changes.

Only after selection populate `W3-HELDOUT-SELECTION.md`,
`W3-GENERALITY-DIFF.json`, `W3-REGEN.json`, and `W3-REPLAY.md`.

## Commit order

1. Seal the corrective public/candidate witness packet.
2. Commit parse-that ABI-3 born-RED difference laws.
3. Implement difference, advance package/ABI/bridge, and seal package gates.
4. Commit BBNF born-RED instance/syntax/semantic/Pratt/replay laws.
5. Replace implicit module entry with canonical grammar instances and migrate
   every caller without a shim.
6. Admit the action-free syntax union.
7. Carry projection, difference, Trim, and Pratt through semantic stages,
   events, replay, and direct emission.
8. Close the generic harness, package crossing, no-overfit, regeneration, and
   zero-diff mutation controls.
9. Obtain two fresh hostile implementation reviews and adjudication.
10. Commit the freeze basis.
11. Commit the frozen receipt alone.
12. Only then execute selection and the post-freeze proof.

Any P0/P1/P2 repair invalidates prior pre-freeze reviews and requires fresh
review.
