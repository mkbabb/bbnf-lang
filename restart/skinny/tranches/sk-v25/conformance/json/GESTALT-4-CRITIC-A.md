# GESTALT-4 — Critic A independent local correctness review

Model: `gpt-5.6-sol`  
Reasoning effort: `xhigh`  
Canonical task: `/root/json_w0_g4_review_a_r3`  
Delegation: none

## Review boundary

I reviewed the sealed JSON W0 source, external tests, fixtures, data and manifests,
offline verifier, README, lock constitution, and `BB-VALUE-TAPE-P0.md`. I did not
inspect prohibited review, author, campaign, history, diff, session, or agent
materials. The sealed subject was not edited. All probes were read-only or ran in
the verifier's disposable roots; this report is in the excluded root review
namespace.

The documented verifier completed successfully on the documented environment:
two fresh roots, 41 tests per root, 318 regenerated JSONTestSuite rows, 104
checked number-boundary rows, a 100,000-value unique binary64 differential, and
byte-identical suite, fixture, number-corpus, source-manifest, and file-manifest
regeneration. Its reported sealed-root identity was
`d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a`.

## Numbered observations

### 1. Correct — invalid public output budgets lose configuration precedence when the document is corrupt

**Locations.** `README.md:72-76` says every budget rejects booleans and other
non-exact non-negative integers as typed `configuration` before input is read or
inspected. `src/json_oracle/parser.py:524-553` validates `max_output_bytes` inside
`_observe`, but public `observe` evaluates `_integrity(document)` first.
`src/json_oracle/projection.py:144-150,201-204` has the same ordering for
`max_projection_bytes`. `tests/json_oracle/test_parser.py:41-50` tests an invalid
observation cap only with a valid document, while
`tests/json_oracle/test_projection.py:70-77` tests a corrupt document only with a
valid cap; the precedence cross-product is absent.

**Reproduction.** From the JSON subject root:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=src python3 -S -B -c '
from dataclasses import replace
from json_oracle import canonicalize, observe, parse
d = replace(parse(b"0"), _root=999)
for name, operation in (
    ("observe", lambda: observe(d, True)),
    ("canonicalize", lambda: canonicalize(d, True)),
):
    try:
        operation()
    except Exception as error:
        print(name, type(error).__name__, error.category, error.code)
'
```

Observed twice, deterministically:

```text
observe ProjectionIntegrityError typed_projection projection_integrity
canonicalize ProjectionIntegrityError typed_projection projection_integrity
```

With the same invalid caps and an intact document, both calls instead return the
declared `ConfigurationError`. The corrupt document is therefore inspected and
determines the failure before the invalid public budget is classified.

**Correction.** Validate each public cap at the outer `observe` and `canonicalize`
entry before calling `_integrity`, preferably through one shared exact-domain
validator also used by the private constructor. Add cross-product tests using a
corrupt document with `True`, a negative integer, float/NaN, string, and arbitrary
object caps; every case must return the cap's typed `configuration` failure before
`projection_integrity`. Retain the present integrity result when the cap is valid.

**Retained facts.** Exact immutable `bytes` input, the five parse-budget domains,
CLI argument-before-I/O precedence, BOM-before-malformed-suffix precedence, and
the source-derived integrity reparse all behaved as declared. Bounded construction
and no-partial-output behavior also remained intact. The defect is the two public
output-cap entry orderings, not the parser's ordinary budget accounting.

**Effect.** Tranche fit remains sound: an independent JSON byte oracle is still a
small and useful W0 assay. Wave fidelity is not sealed because the stated failure
precedence is part of the observable contract. Feature truth is false for the
combined corrupt-document/invalid-cap API case. Greater-product topology is
unchanged; the correction belongs locally in the oracle and adds no product edge.
This assay earns no VT candidate credit before or after correction.

### 2. Correct — the verifier accepts an unsealed Python optimization startup mode

**Locations.** `README.md:170-178` says the verifier checks startup flags and
environment before subject import. `verify.py:86-144` and
`data/runtime-closure.json:30-49` bind selected Python identity, flags, and six
environment variables, but omit `sys.flags.optimize` and `PYTHONOPTIMIZE`.
`verify.py:147-155` can compare only that incomplete description, and the replay
inherits the ambient process environment. The summary at `verify.py:450-461`
continues to report the same binary-tree closure identities.

**Reproduction.** Run the documented verifier with one ordinary Python startup
variable added:

```sh
PYTHONOPTIMIZE=1 LC_ALL=C TZ=UTC PYTHONDONTWRITEBYTECODE=1 \
PYTHONHASHSEED=0 PYTHONPATH=src python3 -S -B verify.py \
  --suite /private/tmp/bbnf-json-suite.XlKwAW/JSONTestSuite \
  --constitution ../../LOCK-CONSTITUTION.md \
  --wave ../../waves/BB-VALUE-TAPE-P0.md
```

Observed result: exit `0`; both 41-test roots and every regeneration passed. The
summary still reported Python closure
`a3c45710552b7fc8dda24e8cc25615b3f22c56302d725f1359c2c4d38085e94a`, Node
closure `be37fca7d3ed1504a2ad774b13780b3e6808e9fa39e583d807af1f664a038314`, and
sealed root `d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a`.
The executing Python mode was nevertheless optimized (`sys.flags.optimize == 1`),
so the claimed execution identity is not exact. Current ordinary outputs happened
to agree; that does not seal the configuration, and optimization can remove
assertions or otherwise change future subject/test behavior while retaining the
same reported identity.

**Correction.** Require and receipt `sys.flags.optimize == 0` (and reject or bind
`PYTHONOPTIMIZE`) before subject import. More robustly, construct replay child
environments from a sealed allowlist rather than copying unbound ambient Python
and Node startup variables, and bind all retained behavior/resource-affecting
flags. Add a negative verifier probe proving an alternate optimization mode is
rejected before tests or generation begin. Refreeze `data/runtime-closure.json`
and the sealed file manifest after that correction.

**Retained facts.** Governance hashes, macOS identity, complete Python and Node
runtime-tree hashes, non-system dependency closure, cache rejection, two-root
copying, target-absent number generation, poisoned-target independence tests, and
cross-root byte equality all passed in the documented mode. The 104 checked rows
and 100,000-value differential support the compact CPython-to-ECMAScript number
route, and ordinary RFC 8785 boundaries, UTF-16 key sorting, string escaping,
duplicate-name rejection, lone-surrogate rejection, nonfinite rejection, and
projection caps behaved consistently. This finding is an execution-startup
identity gap, not an observed JCS byte discrepancy.

**Effect.** Tranche fit is retained, but the oracle cannot be called fully sealed
until its executing configuration is closed. VT.W0 fidelity is reopened at the
deterministic-artifact/evidence-identity gate. Feature truth for the claimed
startup closure is false; semantic JSON facts observed under the documented mode
remain retained. Greater-product movement remains prohibited and no new product
authority should consume the evidence as final. Candidate effect is zero: this
oracle is not one of the four complete independent VT candidates.

### 3. Correct — a base-valid simultaneous-ceiling lone-surrogate shape exceeds the 128 MiB RSS envelope

**Locations.** `README.md:95-106` targets at most 128 MiB process RSS for accepted
adversarial shapes, and `README.md:123-131` says the public regression combines
1,048,576 input bytes, 131,072 decoded units, 59,999 nodes, 29,999 unique entries,
and depth one below that envelope. The enforced shape at
`tests/json_oracle/test_cli.py:138-204` reaches those counters using three-byte
ASCII names and a scalar-valid `x` tail. It does not exercise the base-valid lone
surrogate representation: `src/json_oracle/parser.py:121-133` retains each such
unit as a tuple fact, and `src/json_oracle/parser.py:457-466` expands every tuple
to a separate observation object.

**Reproduction.** Create 29,999 unique object keys from pairs of high-surrogate
escapes, append enough `\\uD800` escapes to the first key to total 131,072 decoded
units, pad the complete object to 1,048,576 bytes, and invoke the base CLI in three
fresh processes while collecting each child with `wait4`:

```python
entry_count = 29_999
decoded_units = 131_072
extra_units = decoded_units - entry_count * 2

def key(index):
    first = 0xD800 + index // 2048
    second = 0xD800 + index % 2048
    return f"\\u{first:04X}\\u{second:04X}".encode("ascii")

members = [b'"' + key(0) + b"\\uD800" * extra_units + b'":0']
members.extend(b'"' + key(index) + b'":0' for index in range(1, entry_count))
source = b"{" + b",".join(members) + b"}"
source += b" " * (1_048_576 - len(source))
```

Write `source` to a disposable input, then run the same
`python3 -S -B -m json_oracle.cli INPUT` command and environment used by the
public regression. All three runs exited `0` with `status: accepted`, exact
counters `{input_bytes: 1048576, decoded_utf16_units: 131072, nodes: 59999,
aggregate_entries: 29999, max_depth: 1}`, and 14,956,823 stdout bytes including
the newline. Peak child RSS was respectively **142,163,968**, **141,262,848**, and
**140,296,192** bytes. Each exceeds 134,217,728 bytes while remaining below the
16 MiB output cap, so neither rejection nor output exhaustion explains the
overage.

Two weaker interpretations were explicitly disproved: a padded one-string
131,072-lone-surrogate input measured 84,541,440–87,556,096 bytes, and the checked
ASCII-key combined shape with only its surplus units changed to lone surrogates
measured 116,785,152–117,080,064 bytes. The failure requires the valid stronger
combination above; it is not an input-construction artifact because the source was
written before each fresh child and stdout was routed to a file.

**Correction.** Either compact the immutable lone-surrogate/scalar observation
facts enough to restore the envelope or lower one or more active defaults so no
base-accepted simultaneous shape crosses it. Add this exact unique-key input to
the fresh-process public regression for at least the base route, assert its exact
counters and acceptance, and enforce both the 16 MiB output and 128 MiB RSS caps.
Reprofile and update the platform-bound table only after three corrected runs.

**Retained facts.** The scalar-valid combined shape, the one-string surrogate
shape, and the partial-surrogate combined shape remain below 128 MiB. Resource
failures still publish deterministic consumed counters, and the reproduced
counterexample publishes a complete accepted observation rather than a partial
document. The defect is the active default envelope over the full admitted base
fact domain.

**Effect.** The JSON oracle remains the right bounded W0 experiment, but tranche
resource fit is not closed. Wave fidelity and feature truth reopen at the declared
accepted-shape memory policy. The repair is local to representation/default policy
and adds no greater-product edge, but downstream controls must not treat current
defaults as a sealed 128 MiB contract. Product movement remains prohibited and
candidate credit remains zero.

## Retained whole-assay facts

- The frozen suite census is internally consistent: 95 `y`, 188 `n`, and 35
  explicit `i` cases; all 318 declared contracts replayed. Base results contain
  116 acceptances and 202 rejections after applying the `i` policies.
- Parser grammar, strict UTF-8, complete-input, exact-number-lexeme, ordered
  duplicate-member, decoded UTF-16/scalar, half-open byte-span, iterative-depth,
  deterministic-counter, and immutable-observation cases passed the reviewed
  ordinary and boundary probes.
- JCS behavior matched the checked RFC vectors and the independently regenerated
  Node/V8 corpus; the generator ignored poisoned target metadata and regenerated
  identically with the target absent in both roots.
- Zero/limit-minus-one/limit/limit-plus-one tests cover every parse budget. The
  checked scalar-valid default-ceiling shapes stayed within their enforced caps,
  but Observation 3 proves that test set is not total over base-valid scalar facts
  and that the active accepted-shape RSS envelope is currently false. These remain
  local platform-bound assay facts, not product performance credit.
- The file-manifest anchor, source/test/verifier manifest, cached-publication
  hashes, and excluded review namespace were consistent in the documented run.
  Observation 2 prevents treating the surrounding startup configuration as exact
  until corrected.
- The README's honest boundary is accurate: this artifact supplies no generalized
  BBNF-derived value/tape API, CSS L4 totality, generated Rust/TypeScript/WASM/ARM
  parity, package consumer, profile dominance, or production edge.

## Final readiness

**Result: NOT READY — REOPEN JSON W0.**

- Correct observation count: **3**
- Incorrect observation count: **0**
- Unproven observation count: **0**
- This pass clean: **no**
- Consecutive clean count after this pass: **0 of 2 required**
- Candidate credit: **0**; credited VT candidates remain **0 of 4 from this assay**
- Tranche state: **local assay retained, correction required**
- Wave state: **VT.W0 JSON oracle reopened; no W0 exit credit**
- Feature state: **failure-precedence, execution-identity, and resource-envelope gates open**
- Product state: **no production edge or movement**
- Movement state: **PROHIBITED pending correction, reseal, hostile adjudication, and two consecutive clean passes**
