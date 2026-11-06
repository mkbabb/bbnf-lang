# CSS L4 W0 input assay — epoch 5

Epoch 5 is the append-only successor authorized by epoch 4 Gestalt 2
adjudication SHA-256
`728b5929543bc754b742be151761a8b5097d0274e37587b248ebd107836759fd`.
It is an executable input assay, not a CSS implementation or a Value Tape
candidate.

```text
retained_byte_replay = VALID
local_semantics      = BLOCKED
wpt_test_membership  = BLOCKED
downstream           = RED_OPEN
candidate_product    = ABSENT_RED
package              = RED_UNBUILT
production           = RED_UNCHANGED
```

No CSS, CSS-L4, VT, lock, control, candidate, package, product, production, WPT
test, WPT outcome, or campaign credit is carried by this directory.

## Eight owning mechanisms

1. `src/assay.py::validate_document()` closes the seven claim-plane enums,
   resolves every evidence identity to exact bytes, requires evidence for a
   positive state, checks compatible counts, and rejects local-state laundering.
2. Genesis reconstructs the complete 3,608-entry CSSWG Git tree, including Git's
   directory-name ordering rule, to root
   `75bf19c016ed98126381508073de6893c9f756f5`. All 96 selected sources are
   verified as exact blob objects. Worktree, staged, untracked, ignored, and
   submodule state cannot affect this commit-tree derivation and is not called
   clean.
3. `src/parsers.py` removes only recognized markup, retains source-byte fragments
   for every accepted unit, preserves `<<...>>` and `<...>` CSS dialect syntax,
   and either consumes the supported CSS VDS, Yacc, Lex, and WebIDL subset or
   emits a typed unsupported carrier.
4. `data/denominator.json` contains 2,565 failure-total unit records. Every unit
   has exact source fragments, a source digest, one closed disposition, and
   independently recomputed inherited-obligation reverse links. Its unit digest
   is `868e551f37de3a095d884b41a33596c9edd8d2cfc0563674c20954c74cd6a2e0`.
   Generation compares it with a fresh isolated child rederivation through the
   same sealed extractor. This is a repeated source rederivation, not an
   algorithmically independent oracle.
5. `data/wpt-prefix-blobs.json` names the retained relation honestly: all 1,905
   declared direct blocks plus the one `wpt-rest` prefix resolve to 16,147 unique
   pinned path/blob-OID rows. Every row retains exact directive source fragments
   and their digest, and is conservatively classified as metadata, reference,
   support/resource, or `UNCLASSIFIED_TEST_MANIFEST_REQUIRED`. WPT test count and
   actual outcome count are both zero; execution is `UNRUN`.
6. `data/candidate-absence.json` checks the closed `candidate` and `candidates`
   root set. Both are absent, candidate and complete-candidate counts are zero,
   and no unused future candidate inspector or candidate-authored graph authority
   exists.
7. `fixtures/counterexamples.json` supplies all A-01…A-10 and B-F01…B-F10 as
   external inputs, plus dropped, duplicated, and unknown WPT-route adversaries.
   Each is executed through its owning integrated gate; `data/mutation-results.json`
   derives 23 executions and 23 expected typed rejections from observed results.
8. `tests/proof.py` runs two complete two-root pairs—four distinct empty output
   roots. Every one rederives the input ledger, runs all counterexamples, and
   reproduces all eight generated files byte-for-byte. `src/manifest.py` then
   seals every ordinary owned byte, excluding only its self-reference, the final
   receipt, and future `GESTALT-*.md` reviews.

## Input and runtime boundary

Epoch 5 hash-binds epoch 4's manifest, policy, governing documents, controls,
adjudication, and 109-object immutable store rather than duplicating 43,013,202
bytes. Its only new content-addressed input is the 108,745-byte compressed CSSWG
commit-tree catalog at
`58875994e81cfdbdbd8418d4dc8016c83fb50ffc97e967c6f7db7028985d891f`.

Before assay import, `src/launcher.py` verifies the exact CPython binary,
isolated/no-bytecode/non-optimized startup flags, a closed child environment,
2,495 stdlib/runtime files (57,077,557 bytes), and all seven non-system dynamic
closure nodes (13,461,728 bytes). Alternate optimization and preload modes are
rejected.

## Replay

From this directory, with the frozen host runtime:

```sh
env -i PATH=/opt/homebrew/bin:/usr/bin:/bin LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC \
  /opt/homebrew/bin/python3 -I -B -X utf8 -X warn_default_encoding \
  src/launcher.py test

proof_root="$(mktemp -d /tmp/css-epoch5-proof.XXXXXX)"
env -i PATH=/opt/homebrew/bin:/usr/bin:/bin LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC \
  /opt/homebrew/bin/python3 -I -B -X utf8 -X warn_default_encoding \
  src/launcher.py proof --run-root "$proof_root/runs" \
  --output "$proof_root/replay.json"

env -i PATH=/opt/homebrew/bin:/usr/bin:/bin LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=UTC \
  /opt/homebrew/bin/python3 -I -B -X utf8 -X warn_default_encoding \
  src/launcher.py manifest --verify-receipt
```

See `AUTHOR.md` for exact hashes, counts, resource receipts, and routed RED
remainder.
