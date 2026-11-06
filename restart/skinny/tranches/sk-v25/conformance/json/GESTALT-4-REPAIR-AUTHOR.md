MODEL DECLARATION: gpt-5.6-sol / xhigh

# JSON W0 Gestalt-4 ordered repair author receipt

Date/zone: 2026-07-19 / `America/New_York`  
Disposition: **CORRECTED EXTERNAL INPUT ORACLE / AWAITING TWO FRESH HOSTILE PASSES**  
Product disposition: **`INCOMPLETE_ASSAY`; ZERO CANDIDATE, LOCK, PROFILE, PRODUCT, OR MOVEMENT CREDIT**

This is an author evidence ledger, not a hostile verdict. It is excluded from the
subject by the existing root `GESTALT-*.md` review namespace. It cannot award a
clean pass. No critic or adjudicator was launched by this repair author.

## Authority and unchanged reopened subject

| Input | SHA-256 |
| --- | --- |
| Binding `GESTALT-4-ADJUDICATION-V3.md` | `09457ca94044aca50eb6b9c40622d00318625e6126816d9eb08ea0d17cb3dfbe` |
| `../../LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `../../waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| Preserved critic A | `caedb328ce76f26bb5ba9291b0393a21c5706763f8dda6528bb2ce62c4633fed` |
| Preserved critic B | `d08dfddab887e4c264b1b9c7a81699e9d9423f6ea5d393447d353e85c7c221ba` |
| Reopened subject manifest | `d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a` |

The governing hashes matched before edits. All prior `GESTALT-*.md` bytes, the
prior `AUTHOR.md`, and the prior sibling reseal remain unchanged append-only
history. The repair touched no candidate, product, runtime crate, package, graph,
parse-that, Value, playground, CSS, sibling-assay, governance, or wave file.

## Ordered mechanisms and RED-to-green counterexamples

### M1 — public output-cap precedence

Before repair, a corrupt private document combined with an invalid public cap
returned `typed_projection:projection_integrity`; an intact document with the same
cap returned `configuration`. The new external matrix crosses 13 corrupt document
families (source identity/type/bytes, source digest, root, counters, node identity,
kind, span, child order, member relation, compact UTF-16 facts, scalar validity,
and literal kind) with eight invalid cap families (`-1`, boolean, finite float,
NaN, infinity, string, null, arbitrary object) for both public byte APIs: 208
invalid-cap combinations. Every combination now returns
`configuration:exact_non_negative_integer_required` with the correct field before
document inspection. Existing per-field corruption tests retain integrity
precedence for valid caps.

One function at `src/json_oracle/parser.py:490` owns exact non-negative integer
validation. Public `observe` (`:594`) and public `canonicalize`
(`src/json_oracle/projection.py:206`) call it before `_integrity`; `_observe`
(`parser.py:575`) and `_canonicalize` (`projection.py:152`) reuse it. No shim,
compatibility route, or second validator exists.

### M2 — governed startup and execution closure

Before repair, `PYTHONOPTIMIZE=1` and a harmless `NODE_OPTIONS` preload both
escaped the receipt and the complete verifier still exited zero. After repair:

| Mutation | Exit | Result before governed subject work |
| --- | ---: | --- |
| ambient `PYTHONOPTIMIZE=1` | 78 | `governed startup environment rejected` |
| ambient Node `--import=data:` preload | 78 | `governed startup environment rejected` |
| direct exact executable with added `-O` | 1 | `governed Python startup mismatch` |

The executable `verify` is the pre-import outer boundary. It rejects ambient
Python and Node startup/preload/resolver controls, resolves itself without `PATH`,
constructs exactly `LC_ALL=C`, `TZ=UTC`, and
`__CF_USER_TEXT_ENCODING=0x1F8:0x0:0x0`, and executes the exact sealed CPython
binary with `-I -S -B -X utf8`. macOS strips `DYLD_*` controls before the protected
shell can observe them; the negative gate proves none reaches the exact Python
environment.

`src/json_oracle/governance.py` is the one child-process registry. It owns the
exact Python/Node/helper paths, Python flags, minimal environment, Python
module/code bootstraps, and command schema (`:11-173`). `verify.py` rejects a
wrong optimization/isolated/safe/no-user-site/no-bytecode/UTF-8 mode before assay
import (`:8-26`), then enforces the complete flag vector, executable, original
argv prefix, xoptions, environment, and `__debug__` through the one registry
(`governance.py:136`). Every normal Python test/generator child and every Node
identity/producer child is built there with no `os.environ.copy()`, implicit
inheritance, or `PATH` resolution.

`data/runtime-closure.json` revision 2 binds:

- the exact launcher and Python/Node/Git/`otool`/`sw_vers`/shell/environment-helper
  argv schemas and resolved executable hashes;
- all Python flags including `optimize=0`, `isolated=1`, `safe_path=true`,
  `no_user_site=1`, `no_site=1`, `dont_write_bytecode=1`, and `utf8_mode=1`;
- the exact three-name child environment and sole dynamic test-suite name;
- the complete Python/standard-library tree and recursively discovered Homebrew
  dynamic closure at `a3c45710552b7fc8dda24e8cc25615b3f22c56302d725f1359c2c4d38085e94a`;
- the complete Node/package tree and dynamic closure at
  `be37fca7d3ed1504a2ad774b13780b3e6808e9fa39e583d807af1f664a038314`;
- the sealed macOS build/kernel/version and exact system-helper identities.

The top-level verifier summary receipts the actual `sys.argv`, `sys.orig_argv`,
effective environment, executable, launcher bytes, verifier bytes, runtime
manifest, source manifest, and subject manifest.

### M3 — admitted default resource envelope

No public surface or default changed: input 1,048,576 bytes, depth 500, 60,000
nodes, 40,000 aggregate entries, 131,072 decoded UTF-16 units, 16 MiB observation,
and 1 MiB JCS projection remain exact. Base lone-surrogate semantics remain
admitted. There is no input-name, fixture, grammar-name, or counter-shape branch.

The frozen representation now retains UTF-16 as immutable big-endian words rather
than a Python integer tuple plus one nested tuple per lone surrogate. Scalar facts
are derived on demand (`parser.py:117-153`); raw leaf spelling is derived from the
single owned source/span. Compact slotted builders replace the former per-node
dictionaries and are progressively replaced in place by frozen slotted nodes
(`parser.py:155-176,433-447`). A compact source digest preserves integrity when
raw strings are no longer duplicated. Observation/JCS bytes and all public facts
remain unchanged.

The binding V3 independently reproduced pre-repair peaks of 140,640,256,
140,623,872, and 140,296,192 B. The exact governed post-repair input contains
1,048,576 bytes, 29,999 unique lone-surrogate keys/entries, 59,999 nodes, 131,072
decoded units, and depth one. All samples exited zero with the exact counters and
the same complete output:

| Sample | Stdout incl. newline | SHA-256 | Wall | Peak RSS |
| --- | ---: | --- | ---: | ---: |
| 1 | 14,956,823 B | `14327c58be9842c89fa2c29bf0f5dfcb1f6647ad14ab32336a3bb0eeab3041d8` | 0.693653 s | 95,846,400 B |
| 2 | 14,956,823 B | `14327c58be9842c89fa2c29bf0f5dfcb1f6647ad14ab32336a3bb0eeab3041d8` | 0.680067 s | 95,223,808 B |
| 3 | 14,956,823 B | `14327c58be9842c89fa2c29bf0f5dfcb1f6647ad14ab32336a3bb0eeab3041d8` | 0.673647 s | 95,600,640 B |

The maximum is 38,371,328 B below the unchanged 134,217,728 B limit. The existing
scalar-valid simultaneous base/scalar/JCS matrix, one-dimensional node/entry/depth
gates, output caps, exact default axes, 318 suite rows, JCS scalar matrix, and
104/100,000 number facts all remain green.

## Complete governed commands and results

All ordinary proof commands entered through `./verify` or the exact command schema
bound in `data/runtime-closure.json`; caches were absent before and after.

```sh
./verify --describe-runtime > /tmp/json-g4-runtime-replay.json
cmp /tmp/json-g4-runtime-replay.json data/runtime-closure.json
```

Result: exit 0, byte-identical SHA-256
`2b4eb71036611a8e7aad880378316cab07e59f0c490d73248a1216f1a9cfd0fd`.

```sh
PYTHONOPTIMIZE=1 ./verify --describe-runtime
NODE_OPTIONS='--import=data:text/javascript,globalThis.__json_w0_unsealed=1' \
  ./verify --describe-runtime
```

Result: both exit 78 before Python subject work. The direct `-O` mutation exits 1
at the early verifier entry check. The external governance suite additionally
checks 19 visible startup controls, three platform-stripped loader controls, five
Python flag mutations, exact child argv, and exact effective Python/Node
environments.

```sh
./verify \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite \
  --constitution ../../LOCK-CONSTITUTION.md \
  --wave ../../waves/BB-VALUE-TAPE-P0.md
```

Final durable-receipt replay: exit 0 in 26.58 s. The top-level verifier used
203,243,520 B peak RSS
(verifier orchestration, not the 128 MiB subject-process gate) and created two
fresh cache-empty subject roots. Each root passed 47 external tests and regenerated
suite observations, number corpus, fixtures, source manifest, and file manifest.
Root test times were 10.9369 s and 11.1573 s. All ten regeneration phases passed.
Network was unused, and the final replay observed all 16 root review reports while
keeping them outside the same corrected subject seal.

The same proof established:

- 318 exact JSONTestSuite rows: 95 `y`, 188 `n`, 35 governed `i`, all contracts
  true;
- 104 checked ECMAScript boundary rows and 100,000 unique finite-binary64 rows at
  `330db74dcc212bff1f8eb7e2124d99b827db3a57edc743929e0221c3fae619d8`;
- target-absent and poisoned-target number generation;
- 15 fixture rows and byte-identical two-root fixture regeneration;
- six source files, eight external-test files, two verifier files, 45 sealed
  files, 1,280,004 sealed bytes, and exact live/declaration path-set equality;
- no `__pycache__` directory or `.pyc` file.

Unchanged generated semantic identities prove output equivalence:

| Artifact | SHA-256 |
| --- | --- |
| 318 expected observations | `69647f10714a3ad20c6615fcc6ad55d56c825af52d83c9de0e9da02dc9d648d6` |
| Suite manifest | `b4df1d9d69075bce2d1b082803986b00845422b258daca77c5da0b2dcb7427df` |
| Fixture manifest | `12f9da36fea88a33787e552a614d2798a39e283e72a2708168ff005931a5231e` |
| Number corpus | `0c679aeafc8de364eb2e2ddbc02357950f611cbb5cb508c562d9f2b35e302d19` |
| Spec manifest | `e6059e0c3d41196cc55e801491d42395197eba4a18d9db761b4838261254e9b0` |
| I-case policy | `f93fe7bda7ac329a1ed15614f411ffb7e3f1ccb82881c06c6b57c892a33c518c` |
| Node producer | `921cbcb65c2997eb4afffcf930bf816a985d589ecd07b27088003bc9a90a7bbe` |

## Exact changed-file ledger

`ADDED` means absent from the unchanged `d3cf8521...` manifest. Sizes are
old -> new bytes. The self-excluded manifest is listed separately.

| Subject path | Old SHA-256 | New SHA-256 | Size |
| --- | --- | --- | ---: |
| `README.md` | `ae5e6f868897d4d1d58ad12e72acbbc5ab0f198269cc24e426b43b8f38b8d91b` | `e6286b411304e6fea9b93d39f5a77b46963633a797972ea2f408899047c9d0f0` | 11,848 -> 13,162 |
| `data/runtime-closure.json` | `0ee773adcc05c245f7fe5487410fc4dbd8490d74240e3c3a2c3cb4c7790f1f7f` | `2b4eb71036611a8e7aad880378316cab07e59f0c490d73248a1216f1a9cfd0fd` | 1,317 -> 6,074 |
| `data/source-manifest.json` | `0e40aa023462dd67f41e07d1c0bbaadcf11293af03f381db4fddf894eada4897` | `55970f2dea844d4dd8a3b0427cc2f61afba77f7bdff2d6d880d8faddb11c1ee3` | 2,002 -> 2,643 |
| `src/json_oracle/generate.py` | `e7e895412280b605532d4eeed8f3e07066ded32845447e67b50180f02582a133` | `fdb9c416e955fb10e90cd0434b463b3f62ad10090eec20bc524f397b0d6270ba` | 11,804 -> 12,144 |
| `src/json_oracle/governance.py` | `ADDED` | `b4849bcb6c1f7288dbb23d9945e97c56ded5be8fb6c318d2fa50df9bb5540d2e` | 0 -> 4,789 |
| `src/json_oracle/parser.py` | `d5e41eaa02a3d81aeb09352d98f83ab622a071f425d4f9d7a183c65a2042ffd5` | `6582d801327dd7a9c4ca542f982b56fd2c8fe534e0d621f4bd658edae8283430` | 20,741 -> 22,025 |
| `src/json_oracle/projection.py` | `4be760cac40f3a3c4143518057ba46799848dae774c5e655c073de10ca12fb5a` | `b0dfd9a0983d0ec261c1dcfd0c12dabe9f6ad3384d92e2d41222f3d8fe288a1f` | 7,285 -> 7,288 |
| `tests/json_oracle/resource_probe.py` | `ADDED` | `65f47d2938ac71584b7d8acafffbe9054c03dd0b65204a269424064fb15148a6` | 0 -> 2,611 |
| `tests/json_oracle/test_cli.py` | `7d4576c51ac1f980164c9cd9cd029fe5495d89bf865bc341135da50b3148365b` | `9bf5c6d395bbde21850a8d14e019e5b271570b2e4621fa32a90a7c9b0febd836` | 8,966 -> 9,295 |
| `tests/json_oracle/test_governance.py` | `ADDED` | `2d40c37fd4d9f0fc0d20b65874fb71916a06f6214b30ba076c9437cb9e5d3b37` | 0 -> 4,270 |
| `tests/json_oracle/test_parser.py` | `c1a71c46c1bf6f74ffb4ecf62fab7956f8819085ca6cc47cb62a6115401d32ba` | `d093cdc8510088853dc5b546c9d98f30901baabcd577fc3a33ab9b7f74834d1e` | 11,447 -> 11,380 |
| `tests/json_oracle/test_projection.py` | `472ee89ab816e547475a101756c1d9aea87c64b3d2f8a3c3f6f6c3100e78d908` | `739f05891e5d1442fb508bb748b5dcaa66a2092d0e0a802f8d6ced5da24b537d` | 7,343 -> 9,305 |
| `verify` | `ADDED` | `1a499c620d3c6373326aa47f0bd5ef4b360f75031c1146790b77b04a4432c357` | 0 -> 999 |
| `verify.py` | `13749d48ed1c763279cb822f5914cd04704e8c7b819cb9daa3d9db0ad0320336` | `fae8484de037c02cbbcd9fff1fb98da9e7a9c47530b506ea30e0cc433303e72d` | 16,885 -> 19,095 |
| self-excluded `data/file-manifest.json` | `d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a` | `7ddfd1691ed17017503751922b2c26d6d224b0698eb7f280fedbc45fc0ec260c` | 6,828 -> 7,469 |

No other old manifest row changed. The sealed subject grew from 41 to 45 files
only for one governance owner, one pre-import launcher, one external governance
suite, and one executable resource probe.

## KISS and LOC effect

Measured against the reopened sources, authored source/test/verifier/README lines
are 2,386 -> 2,991, net +605. The split is:

- owning representation and output APIs: +57 lines;
- the single startup/child-process authority plus launcher/verifier/generator:
  +287 lines;
- external negative/resource proof: +239 lines;
- corrected README contract: +22 lines.

Mechanically derived runtime/source/file manifests add 181 lines. Product and
candidate LOC remain exactly +0. The representation deletes retained raw strings,
per-unit Python integers, per-lone-surrogate nested tuples, builder dictionaries,
ambient subprocess inheritance, and repeated test subprocess recipes. New process
complexity has one registry and one launcher, not a compatibility path or second
runtime authority. The exact resource source construction has one reusable
external probe rather than a duplicate test payload.

## Total-tranche / wave / feature / greater-product self-audit

- **Total-tranche fit:** the directory remains the smallest independent JSON byte
  falsification organ for VT.W0. The three repairs close only local failure
  precedence, evidence identity, and admitted-resource truth. No broader tranche
  mechanism was imported.
- **Wave fidelity:** strict RFC 8259 bytes, BOM/UTF-8/complete-input precedence,
  exact numbers, duplicate order, UTF-16/scalar facts, spans, immutable
  observations, source preservation, the exercised RFC 8785 projection, all
  default axes/output caps, deterministic generation, fixtures, and suite/runtime
  identities are retained. CSS and the remaining VT.W0 exit surface remain RED.
- **Feature truth:** the corrupt-document/invalid-cap cross-product, startup
  optimization and Node preload negatives, exact closure replay, and exact
  lone-surrogate counterexample are now directly executable. No semantic corpus,
  default, cap, or API was shrunk to make them pass.
- **Greater-product effect:** repository search finds no production import of this
  assay. No product graph edge, package, schema, Value/tape owner, parse-that
  dependency, candidate sharing, profile claim, or production source moved. This
  remains observation-only external evidence.

The author found no discrepancy after this four-plane audit. That statement is
not a verdict: the constitution still requires two new consecutive hostile
gestalt passes over the corrected subject. A correct future finding resets the
count again.

## Seal and routed remainder

| Final identity | SHA-256 / count |
| --- | --- |
| Runtime closure manifest | `2b4eb71036611a8e7aad880378316cab07e59f0c490d73248a1216f1a9cfd0fd` |
| Source manifest | `55970f2dea844d4dd8a3b0427cc2f61afba77f7bdff2d6d880d8faddb11c1ee3` |
| Self-excluded subject manifest | `7ddfd1691ed17017503751922b2c26d6d224b0698eb7f280fedbc45fc0ec260c` |
| Sealed subject census | 45 files / 1,280,004 bytes |
| External review reports before this receipt | 15 |
| Consecutive clean count | 0 of 2 |
| Complete VT candidates from this assay | 0 of 4 |
| Candidate / PT-BB lock / VT.W0 / profile / product credit | 0 / 0 / 0 / 0 / 0 |
| Production/general-product movement | NONE / PROHIBITED |

The sibling `../JSON-W0-GESTALT-4-RESEAL.md` is created after this receipt so it
can non-circularly anchor both the subject manifest and these excluded author
bytes. The routed remainder is exactly two fresh hostile passes and independent
adjudication. This author stops before critics, candidate work, or product
movement.
