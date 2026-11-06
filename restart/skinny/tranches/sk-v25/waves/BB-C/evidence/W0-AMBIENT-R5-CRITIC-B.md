# BB-C.W0 ambient R5 — hostile critic B

## Receipt

- Role: fresh hostile critic B.
- Declared model: `gpt-5.6-sol`, reasoning effort `ultra`.
- Posture: assume the candidate, its evidence, and its wave placement are wrong until independently proved.
- Scope: SK-v25 `BB-C.W0`, R5 only.
- Production policy: read-only. No source, test, manifest, lockfile, generated-output, staging, or commit mutation was made.
- Permitted write: this receipt only.
- Machine policy: ARM only. No x86 execution was requested or performed.
- Value boundary: no Value source, evidence, or worktree was read or mutated.
- Final content identity: reported with the handoff after this file is sealed.

## Materials independently assayed

The following controlling receipts were read in full and checked against the
underlying source, patches, logs, or filesystem artifacts where applicable:

| Artifact | SHA-256 | Lines / bytes |
| --- | --- | ---: |
| R4 adjudication | `ecbeba2589a2df69e7207185200ec9967bf3b5303a7bb058cb6af67d125377db` | 712 / 52,638 |
| R5 born-RED author | `733bdf2cf8a6b93a4e0bf1f5ce44b4345326bb98f673607d52632b1ebd644834` | 197 / 12,210 |
| R5 born-RED native | `fc409a9383c401c110b4f40bdfe383c89f7b9154b6018bbcf0ca35485ed08b55` | 120 / 5,630 |
| R5 author | `7dc4eb343db5ca7d80c8094cd39d2a6377084e447f7befc0caf760fb42f6b6a1` | 217 / 13,043 |
| R5 native | `83c5616badafe488feb975f6c0c80f1650e734effce430405e67176bdff17774` | 116 / 8,445 |
| Mutation manifest | `e3fa48b4c659fda0823807de243d788043872526b4f66428e78dad8909b9b512` | 60 / 7,018 |
| W0 wave | `00bb9916a55765639b52804d4f15fb8d6bcd588f27343bc6f6ad30391c2f6df2` | read in full |

The seven GREEN logs and nine mutant logs were inspected directly. Their
digests and sizes agree with the R5 native receipt:

| Gate | SHA-256 | Lines / bytes | Observed result |
| --- | --- | ---: | --- |
| G1 | `ccea132265f5824026454726baff767d081464ef67226ecb007cc478089a8732` | 29 / 2,347 | 21 passed |
| G2 | `cfa8b1f32564a216688c59bf4de81654cbcbac6fb85c9690e865182203b1cf5f` | 20 / 1,238 | 12 passed |
| G3 | `e2ee4f51e86d3685c6d21d41935ae353b454555c4351fc25d47eaed18611cbf9` | 1,128 / 42,950 | 8 passed |
| G4 | `ad9222b3015242de44bbde93f67cba2f735abf5423f9f8506077169000f0ba0e` | 1,120 / 42,262 | 3 passed |
| G5 | `a5521c791a8d7fa2ef0a5ed5d942b4dbb3cd01c461bc8401913ee9150661d85d` | 13 / 784 | 5 passed |
| G6 | `11547c0b7275468617dc965fa695025c7de05bb8c7d8f57e33be20496f8c7088` | 16 / 936 | 9 passed |
| G7 | `08d5ae60ed81567a422b021aa92472d93ab99550dfabff82005e61b29436f999` | 18 / 1,179 | 11 passed |
| M01 | `639205eb98eae8c302a7557bb4741f80f17c3047414d05aa7f41d1e48dd43288` | 1,131 / 42,687 | killed |
| M02 | `f60c77edab9dc69b18538b06f4bda03fa20ddc692c27390c214fbd7880ca0914` | 21 / 926 | killed |
| M03 | `6eefca97d77ef69b3a003c785af3919401cd43a8360505948fa4be20cc29f1aa` | 24 / 1,382 | two tests fail |
| M04 | `6912dd4e95ce4f53b7c34f0f478c46dd8801c6c78f981748f2f80d059f9d666` | 28 / 1,309 | killed |
| M05 | `8e60ff3e52ae99cdfec654e95e225110ed750cff657396663140a0e863479d8b` | 30 / 2,041 | killed |
| M06 | `c77cd34492f76ac03c1e46f471d767fc15ca9dca5e381696539891ea827e3ed9` | 28 / 1,405 | killed |
| M07 | `6a76c816866e66de5de62e02fd2e8bf9c89ed8660c8a88cb7550b15609078c73` | 28 / 1,412 | killed |
| M08 | `0b2dcb97836c42810595ece3231a076081e8144332a40e4d0b51795ae767ab8` | 21 / 1,004 | killed |
| M09 | `ab9f556491f4abe05e1fc8939137713545d3fc40efb33d1cd4e1f965f45e0caa` | 21 / 983 | killed |

The reported GREEN total of 69/69, nine mutant commands, and ten mutant test
failures agrees with the raw logs. The 183-warning summaries in G3 and G4 are
also real and pre-existing; the R5 IR candidate gates themselves are clean of
new warnings.

## Candidate identity and restoration

All source identities declared by the author receipt were recomputed and
matched:

| Subject | SHA-256 | Lines / bytes |
| --- | --- | ---: |
| Pratt emitter | `bfc2dff61df7722404b001c45c24bdf84c796818c3bdc62932c7bd3e14e688d2` | 371 / 17,666 |
| Generated BBNF parser | `c8f5e69e320f1c09408eed83c6031264e39fa7424f4b141618847f0120fcbe2` | 21,211 / 1,062,265 |
| Generated registry | `3e7494a247b68fbc968376bd377363d3ab63da55edda210b4f868fe335b8021` | 2,232 / 49,354 |
| Expression lowerer | `3cb889217326833648a4154d17bd4d5122c64ab1f0030b70364262206139a16a` | 495 / 22,077 |
| Value-expression wrapper | `10fccf5386141107e8cc5e0cd78726bc38202ea3365024a4d55195517b8ea39c` | 491 / 18,126 |
| Value-expression module | `c21c02dba33e35b9e489eae08f96f3eab8e088ec8c2d40b1fa1ffca0e63b26` | 177 / 8,193 |
| CSP module | `9f987988c793a3778954e1ba9c8c17ed74916175660e6330d68bac28c516dca` | 204 / 8,404 |
| Grammar solver | `22c90aad83b968413f9c11f5aa09ce57f1ea7f64a6f71884e92606a945aea0c7` | 728 / 26,664 |
| Site lowerer | `5b7bb7dd9bea5a08309952084415e0026028a513e439d92ad03a11f6f1347b9` | 473 / 16,085 |
| IR tests | `8c3f0541a02d9797337106ebcb9357cf4b44ba07d20215ec5f85e32d56550436` | 679 / 22,695 |
| Lowerer tests | `6b1197af233c6bbab1a2a2fed90f74310cd27e5936113b01e42bd7a86eee8a48` | 212 / 6,749 |
| Cross-rule tests | `ac5cc4349ac08904ebf54694135264389729b1beed2cd85de7cbdb1386a24f52` | 793 / 27,493 |
| `Cargo.lock` | `480ce09825285416a73b7b16f106ae380d002cd212446a68d5783ba9481200e9` | 4,650 / 113,885 |

All nine patch identities match the mutation manifest. All nine currently pass
`git apply --check`. The patch ledger, source-restoration ledger, apply log,
and native compatibility patch also match the recorded identities:

- patch ledger: `fb8590cf2d614f48c0817eaac20115c10fd22b622db502185a6ba047b74312e8`;
- source restoration: `c5c05b4bc87ef408beef67b99a7af3340f661931491349f8457c37cd37c1fa27`;
- apply log: `d26392a77969a80c7bd769239d5ab2a5e88c80307082c8779fdb9c7cc8c63309`;
- compatibility patch: `5602edab31b7b4b8940767820236dad580153439e1eac7ec8dc05f3397ad0eda`.

The isolated `xtask` artifact exists, is arm64, and has SHA-256
`a92cd96aa92866adbe66dce14ce190156104904162f62dabd60e28f371069ac8b`.
The restored `apple.rs` has SHA-256
`ce3ac84b60afe11f51cc81fe5d6f8e46591f5ad3f83a82c9921b93cf37fffc5f`.
The temporary generated BBNF parser and registry are byte-identical to the
checked candidate.

That identity proof is necessary, but it does not establish the generator
fixed point discussed below.

## Fact adjudication

### Correct

1. **The R4 whole-span scanner was removed.** The candidate obtains the
   structural extent from parser-owned compound bounds. The removed
   `DirectArrow`/`find_direct_arrow`/`direct_map_rhs` route is not retained as a
   legacy alias or fallback.

2. **Structural Map is the sole valid arrow owner in the assayed path.** The
   lowerer no longer guesses arrow ownership from source text.

3. **Compound numeric negatives are constrained to literal nodes.** Suffix
   interpretation is limited to integer and float literals rather than being
   applied to arbitrary compound source spans.

4. **The public solver has one component-transaction path.**
   `solve_grammar_components` creates one private grammar transaction, enters
   and commits each component, and publishes only at final completion. The
   factorized and constrained routes converge at the same commit boundary.

5. **The transaction's substantive ordering is sound on the bounded dense
   IR.** There are cancellation checks at entry, after decode and before merge,
   before advancing to the next component, and before final publication.
   Merge failure clears the objective. Uncommitted output remains private.

6. **No public twin solve path or test hook was introduced.** The test-facing
   helpers are crate-private and exercise the production transaction state.

7. **Decoder rejection is defense in depth.** The M08 and M09 tests compute
   both compared results before asserting, so they do not accidentally skip
   one route through short-circuit evaluation.

8. **The R5 lowerer is materially simpler than the R4 candidate.** It removes
   a large scanner/fallback sector instead of adding a parallel interpretation
   path. This is directionally KISS-forward.

9. **The reported source restoration is real.** The three native compatibility
   files checked by the author/native receipts are restored after the evidence
   campaign.

### Incorrect or overstated

1. **“Nine production-distinguishing counterfactuals” is false.** M06 kills a
   helper truth-table state that the production solver cannot reach.
   `CancelToken` is monotone (`false -> true` only), and CSP sets
   `stats.cancelled = true` only after observing that same token. Therefore
   `stats.cancelled == true && current token == false`, the M06 differentiator,
   is impossible on the production route. The statistics leg of
   `cancellation_observed` is redundant, and the mutant proves the helper's
   spelling rather than product behavior.

2. **M07 does not prove its actual production boundary.** A final-leaf token
   cancellation with `stats.cancelled == false` is reachable and important,
   but the selected M07 oracle invokes only the boolean helper. The existing
   production-route test
   `cancellation_on_final_singleton_leaf_discards_incumbent` was not run as the
   M07 killer. The syntactic mutant is killed; the claimed production-distinct
   proof is not supplied.

3. **M01 does not execute its declared control.** The manifest/born-RED packet
   names the valid block-comment case as the ownership control alongside the
   division killer, but the native M01 command runs only the division test.
   Static inspection suggests the block-comment case remains valid, yet that
   is not the required dynamic paired control.

4. **The generic Pratt-emitter change was treated as though it were BBNF-only.**
   The emitter is shared. The repository currently has seven BBNF Pratt
   definitions, seven Google Sheets definitions, and two CSS L4 definitions.
   Only BBNF generated output was refreshed. Google Sheets and CSS generated
   parsers still visibly lack the new compound-bound recording at their Pratt
   functions. Merely compiling those stale products does not validate the new
   generic emitted behavior.

5. **“External killing oracle” is too broad a label for M01 and M03-M09.**
   Their files are physically displaced into source-isomorphic test trees, but
   they are included with `#[cfg(test)]` and `#[path = ...]` into crate-internal
   `--lib` test binaries. M02 is the true public integration oracle. This does
   not make the private tests worthless; it changes what they prove.

### Unproven

1. **Generator self-hosting fixed point.** The isolated `xtask` was built
   before the newly generated BBNF parser was applied, then used to generate
   that parser. It embeds the first-epoch parser. The campaign did not rebuild
   `xtask` against the regenerated parser, regenerate BBNF a second time, and
   require byte equality. Consequently the checked output is first-epoch
   output, not a proved self-hosting fixed point.

2. **Fleet-wide regeneration closure.** The generic emitter's Google Sheets,
   CSS L4, and other manifest-grammar consequences were not regenerated in a
   fresh temporary tree and compared or tested. Repository-wide
   `cargo xtask regen --check` closure is therefore unproved.

3. **Valid-grammar behavior under a rebuilt generator.** As a read-only probe,
   I ran exactly once on ARM:

   ```text
   /usr/bin/arch -arm64 /tmp/bbcw0-r5-xtask-target.VhwlDR/aarch64-apple-darwin/ax-iter/xtask regen --check --grammar google_sheets
   ```

   It exited 101 after generating/prettifying BBNF and then panicked while
   entering JSON lowering:

   ```text
   crates/core/src/lower/expression/wrap.rs:66:28:
   mapped_factor: value expression has no structural source bounds
   ```

   This is not proof that a newly rebuilt current candidate necessarily
   panics: the probed executable is precisely the stale first-epoch binary.
   It is proof that the frozen artifact cannot establish fleet closure, and it
   turns current valid-grammar behavior into an explicit RED question that a
   rebuilt fixed-point run must answer.

4. **Semantic and performance neutrality for all shared-emitter consumers.**
   CSS uses recording-related builder overrides, and Sheets has its own Pratt
   family. No affected-consumer semantic suites or profiles were presented
   against freshly regenerated products.

5. **One-shot/no-retry/native-shell claims.** The raw logs contain aarch64
   artifact paths and are consistent with ARM execution, but they do not
   contain the invoking shell command, architecture preflight, environment, or
   retry ledger. Those properties remain attestations rather than independently
   reproducible facts from the logs alone.

6. **Malformed RuleId totality.** This remains routed to W0 item 2 and is not a
   valid reason to reject the bounded R5 transaction mechanics. It still bars
   W0 completion and W1 entry.

## Mutation adequacy

The mutation bank is useful but not terminally adequate.

| Mutant | Hostile disposition |
| --- | --- |
| M01 | Killed, but missing its declared valid-comment paired control. |
| M02 | Clean public integration proof for objective clearing. |
| M03 | Useful private transaction-entry proof; it uses production state, not a twin implementation. |
| M04 | Useful private post-decode/pre-merge proof. |
| M05 | Useful private before-next-component proof. |
| M06 | Vacuous production state; delete the redundant stats leg/test instead of celebrating the kill. |
| M07 | Reachable policy, wrong oracle level; rerun against the actual final-singleton-leaf product route. |
| M08 | Useful private decoder defense-in-depth proof, not public malformed-input reachability. |
| M09 | Useful private decoder defense-in-depth proof, not public malformed-input reachability. |

The result is not “zero evidence.” It is also not the requested nine orthogonal,
production-distinguishing counterfactuals. Credit must remain bounded to the
facts each oracle actually establishes.

## Structure and parsimony assay

The principal lowerer repair is good architecture: parser-owned bounds replace
source rescanning, and a single structural owner replaces contextual guessing.
The solver transaction is similarly cohesive and avoids publishing partial
state.

Two blemishes remain:

1. `cancellation_observed(stats, current)` encodes a redundant, unreachable
   `stats && !current` distinction. The KISS solution is to consult the current
   monotone token at the publication boundary and remove M06's contrived state,
   unless a real independently sourced cancellation state is first introduced
   and proved reachable.

2. `find_inner_expression` repeats `BinaryFactor` in its ordered-kind search
   while the canonical dispatcher separately owns `BinaryFactor`. This is a
   small ownership duplication worth cleaning when the fixed-point repair
   revisits the lowerer, but it is not independently a demonstrated R5 semantic
   failure.

The internal `split_numeric_suffix` helper still exists for literal parsing;
only its broader export/use was removed. Any claim that the symbol itself was
deleted would be inaccurate.

## Wave and tranche fit

R5 was correctly aimed at the R4 blockers: parser-owned bounds, compound
numeric negatives, late objective clearing, deterministic transaction
boundaries, and cancellation publication. The lowerer and transaction work
largely satisfy that shape.

The native discovery that Pratt compounds needed explicit bounds was also a
legitimate dependency. The error was wave closure: changing the generic Pratt
emitter expanded the affected graph beyond BBNF, but the campaign continued to
prove only the BBNF generated product. A generic generator edit cannot be
accepted on a single-consumer, pre-fixed-point receipt.

This is not scope creep into unrelated future features. Generator/source/output
isomorphism and every directly affected grammar are part of the exact R5
dependency cone. Conversely, malformed RuleId totality remains W0 item 2 and
must not be smuggled into R5 implementation.

Even a repaired R5 cannot complete W0. Items 1, 2, 4, and 5 remain open under
the controlling wave specification, so W1 remains barred independently of this
verdict.

## Smallest technically honest next action

Do one bounded R5 repair formation, not a new architecture:

1. Remove the redundant cancellation-stats publication leg and retire M06, or
   first demonstrate a real production state in which stats is cancelled while
   the monotone token is not. Prefer deletion.
2. Bind M07 to the existing actual final-singleton-leaf cancellation route.
3. Add the declared valid block-comment control to M01's native command.
4. Rebuild the ARM `xtask` against the newly generated BBNF parser using the
   exact temporary compatibility procedure and restoration ledger.
5. Regenerate BBNF a second time and require byte-for-byte equality with the
   checked candidate. Any drift is born-RED and must be explained or fixed.
6. In a temporary tree, regenerate every manifest grammar affected by the
   shared Pratt emitter. Treat any valid-grammar panic or output drift as RED.
7. If Google Sheets or CSS output changes, apply the complete mechanical set
   together and run their directly affected public semantic suites and bounded
   profiles. Do not land partial generated output and do not add compatibility
   shims.
8. Repeat two hostile critics and an adjudicator on the repaired, content-
   addressed evidence set.

No redesign of the sound transaction core is justified by this receipt.

## Terminal verdict

**R5 is RED. Critic B is not clean. Zero terminal R5 credit. W1 remains
barred.**

The candidate contains a substantially better lowerer and a mostly sound
transaction core, and its reported 69 GREEN tests plus syntactic 9/9 mutant
kills are real. They do not close the wave. The shared Pratt-emitter edit lacks
a rebuilt self-hosting fixed point and fleet-wide generated-output proof; the
stale first-epoch generator actually fails on a valid manifest grammar during
the only independent probe. M06 is not production-reachable, M07 is proved at
the wrong level, and M01 omits its declared control. The clean recommendation
is a small dependency-closure and mutation-adequacy repair, followed by fresh
hostile adjudication—not acceptance, not rollback of the sound core, and not a
new workaround path.
