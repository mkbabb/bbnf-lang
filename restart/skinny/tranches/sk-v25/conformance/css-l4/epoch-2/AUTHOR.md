# CSS L4 W0 epoch-2 author receipt

Date: 2026-07-18  
Declared and task-context-verified model / effort: `gpt-5.6-sol` / `xhigh`  
Delegation for this implementation: none  
CSSWG pin: `c7573530343759ace8e46438a1fa2c44515b5554`  
WPT pin: `37cd7ff74eb974fd41600bf7bc01d37576abf8b5`  

## Terminal status

**W0 successor contract implementation: complete and executable. W0 evidence:
BLOCKED. Hostile-pass request: NO.**

This is a born-RED external oracle input only. It is not CSS support, a parser
implementation claim, a candidate, a semantic/type sidecar, or permission to move
production or tranche state. It remains external to the current four-of-four full
orthogonal VT candidate portfolio. Candidate schema/type consumption is
mechanically rejected; BBNF paths in the matrix are expectations only.

## Authoritative inputs and one-shot acquisition

The frozen epoch-1 bank is read-only and verified against eight exact file hashes,
including adjudication SHA-256
`e5a5d85f73dee62c09962b7a7f5a980dea9075c10e8425d0064d61a314e3abf8`.
No epoch-1, production, lock, or pinned CSSWG file was edited.

The CSSWG checkout was already present and clean at the required pin. WPT was
fetched exactly once from `https://github.com/web-platform-tests/wpt.git` with
`--depth=1 --filter=blob:none` at the exact revision, then exposed as a metadata-only
sparse checkout. Its tree is
`12fa64c9afe86e199ee0e737755b6507f363ceff`: 160,788 blob paths, with canonical
`path\0oid\0` manifest SHA-256
`cf3aedd08cbb161aeebd3c9e19c10f855f2be0be23fa5ecbbd2035d2e31db699`.
The complete sorted path/blob manifest is embedded in `data/scope.json`.

The dated-publication fetch used one request per URL, a 0.75-second interval, the
declared project user agent, no retry, and stop-on-first-failure. It pinned 61 new
dated receipts; together with the 23 banked receipts, 84 of 96 current specs are
pinned. After 123 completed requests it stopped on the first typed `HTTPError` in
the `css3-mediaqueries` retrieval cycle, recorded exactly as
`HTTP Error 404: Not Found`. Eleven later publications remain `MISSING`; the failed
one remains `BLOCKED_INPUT`. The fetch was not rerun.

Regeneration and verification force `GIT_NO_LAZY_FETCH=1` and
`GIT_TERMINAL_PROMPT=0`. The ignored 27 MiB `vendor/wpt/.git` verification cache,
all `__pycache__` bytes, and the WPT sparse worktree are not part of this subject.

## Four-stage evidence receipt

| Stage artifact | Bytes | Principal rows | Open blockers | SHA-256 |
|---|---:|---:|---:|---|
| `data/scope.json` | 37,285,537 | 96 specs; 174 dependencies; 160,788 WPT files; 29,229 routes | 644 | `a99597159f540fa76c132a875fac48eb36bff427cc12477e6236e73cd35ed037` |
| `data/carriers.json` | 3,649,956 | 1,468 exact-span carriers | 0 | `22b8bf619d2d47dfdbc87112c7c25a6d5440641c0534a9774622aa79e8009c95` |
| `data/grammar.json` | 4,021,193 | 793 definitions; 82 expressions; 1,012 entities; 580 profiles; 1,168 closure edges | 229 | `ca6e361de15d55f144606abeeaf05b7272575d9635181ebfd566c77831540109` |
| `data/matrix.json` | 5,203,904 | 2,102 external observation rows | 2,102 | `46fedfdce6b8314a4d66cc3d62f737a5cfec1bbf009cb58e660e71a130bb1873` |

The four generated artifacts total 50,160,590 bytes. The fetched publication
receipt is 19,993 bytes, SHA-256
`9946f814b75561988ef62fc2ea1512816e6d2d1112edf96bebfcafedaf4c319d`.
The independent 2,905-byte fixture is SHA-256
`23a300af73a38f99fed5cb97dcb989441f75f66f50cbbc6592a992830295e936`.

Stage 1 binds 96 unique canonical source paths totaling 11,676,246 bytes. Its 644
blockers are 624 unmapped source-declared WPT routes, 11 missing publications, one
typed publication-fetch block, and eight unproven external parsing authorities.
Of 29,229 routed tests, 28,605 resolve to exact WPT blob OIDs.

Stage 2 emits 1,395 normative, 48 example, 16 historical, and nine non-normative
carriers. It covers 660 property rows, 112 descriptor rows, 303 value-production
blocks, 49 railroads, 114 legacy grammars, 13 legacy rows, 137 IDL blocks, and 80
algorithms. The two source members with no normative carrier have explicit terminal
scope/obsolete-profile decisions; no Stage-2 carrier-decision blocker remains.

Stage 3 parses 769 of 793 definitions, 46 normative railroad trees, 34 other
grammar expressions, and all 1,012 entities. It keeps 26 parse failures, 18 profile
conflicts, and 185 unmapped references visible. Reference closure contains 650
resolved, 322 prelude-imported, nine property, two conflict, and 185 unmapped edges.

Stage 4 gives every row visible BBNF-expectation, fixture, WPT, recovery,
diagnostic, serialization, and owner statuses. All 2,102 rows remain `UNPROVEN`:
1,489 have pinned-but-`UNRUN` nearest WPT routes and 613 have `UNMAPPED` routes;
all recovery, diagnostic, and serialization planes remain `UNPROVEN`. No percentage
or coverage credit is reported.

## Binding successor-gate disposition

| Gate | Status | Evidence |
|---|---|---|
| Canonical scope and normative parsing dependencies | **BLOCKED** | 96 canonical sources are hash-bound; eight external parsing authorities remain unproven. |
| Dated publication epoch | **BLOCKED** | 84 pinned, 11 missing, one typed HTTP block. |
| Exact WPT corpus, routing, and execution | **BLOCKED** | Commit/tree/full manifest are pinned; 624 routes are unmapped and no WPT result is claimed. |
| Source-aware carrier/tokenizer/normativity extraction | **PASS** | Comment, quote, raw-text, live-comment-literal, legacy, IDL, algorithm, and terminal no-grammar cases are externally tested. |
| Grammar AST/profile/reference closure | **BLOCKED** | ASTs and closure are executable; 229 exact failures/conflicts remain. |
| Full external observation matrix | **BLOCKED** | All required planes exist, but 2,102 rows remain incomplete. |
| BBNF-only candidate authority boundary | **PASS** | Candidate schema/type input is explicitly false and mutation-tested fail-closed. |
| Independent mutations and deterministic offline replay | **PASS** | 26 tests pass, including two complete regenerations and network/lazy-fetch guards. |
| Truthful receipt and advancement discipline | **PASS** | Exact blockers are reported; no hostile pass or product credit is requested. |

## Verification

Run from this directory:

```sh
export GIT_NO_LAZY_FETCH=1
export GIT_TERMINAL_PROMPT=0
python3 -B src/regen.py --check --allow-blocked
python3 -B -m unittest discover -s tests -p '*.py'
python3 -B src/regen.py --check
```

Observed result:

```text
deterministic artifact check          BLOCKED as recorded; exit 0 with --allow-blocked
external source-isomorphic tests      26 PASS (3.627 s)
fail-closed artifact check            BLOCKED; 2,975 blockers; exit 2
CSSWG HEAD / dirty entries            c757353... / 0
WPT HEAD / tree / dirty entries       37cd7ff... / 12fa64c... / 0
```

The test suite's regeneration test builds all four stages twice with network
connection creation patched to fail and compares both byte encodings to each other
and to the stored artifacts.

## KISS and corpus binding

The implementation is seven small Python files: the four direct stages plus one
common fail-closed module, one one-shot fetcher, and one regeneration driver. It is
1,902 source lines / 70,325 bytes, with five source-isomorphic test modules and one
19-line support loader totaling 328 test lines / 15,146 bytes. It has no package
framework and no third-party Python dependency.

For the 17 non-generated subject files other than this receipt and the fetched
publication receipt, the reproducible digest
`sha256(concat(sorted(path + NUL + file_sha256 + NUL)))` is
`c6e5ea8b247f838955c9909fcfc4bee78416c48a36a8e303989696d65ab3f418`.
Generated data is bound individually above; ignored checkout/cache bytes are
excluded by construction.
