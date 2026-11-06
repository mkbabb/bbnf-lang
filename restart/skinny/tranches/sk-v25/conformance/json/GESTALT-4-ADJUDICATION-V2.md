# GESTALT-4 adjudication V2 — JSON W0 byte oracle

Date: 2026-07-19 (`America/New_York`)  
Disposition: **NOT CLEAN / REOPEN LOCAL ASSAY**  
Movement: **PROHIBITED**  
Credit: **ZERO**  
Current subject seal: `data/file-manifest.json` SHA-256
`d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a`

## Supersession and boundary

This report supersedes `GESTALT-4-ADJUDICATION.md` for the GESTALT-4 ruling and
terminal disposition. V1 remains unchanged as append-only history. V2 corrects
V1's incomplete fact count by adjudicating all three numbered A observations and
all seven numbered B observations, including B's retained-fact claims.

The two critic reports, V1, current subject, governing inputs, earlier
adjudication, repair receipt, reseal, and current manifests were inspected
read-only. Reports and receipts are evidence, never authority. No sealed subject
byte was changed. V2 is excluded from the subject census by the root review
namespace.

The current manifest still declares 41 sealed records and 41 live sealed paths.
Every declared size and hash matched the live file. That proves the reviewed byte
identity only; it does not prove the behavioral claims under adjudication.

## Exact inputs and anchors

| Input or evidence anchor | SHA-256 |
| --- | --- |
| `../../LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `../../waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| `GESTALT-4-CRITIC-A.md` | `caedb328ce76f26bb5ba9291b0393a21c5706763f8dda6528bb2ce62c4633fed` |
| `GESTALT-4-CRITIC-B.md` | `d08dfddab887e4c264b1b9c7a81699e9d9423f6ea5d393447d353e85c7c221ba` |
| `GESTALT-4-ADJUDICATION.md` | `356f832d51ce0e1711006273c614eab1c76a4938aec584277ccd368f95b74a6f` |
| `GESTALT-3-REPAIR-AUTHOR.md` | `8c5e2afbb37aec2256ae70baabfabf096221176ac72d386a326d790ca2989f37` |
| `GESTALT-3-ADJUDICATION.md` | `27a17d61977aa20082421f67e75c4762ebf8c6ce712bf1a8bba05f53b2ef97af` |
| `../JSON-W0-AUTHOR-RESEAL.md` | `1969c88d6d30f89e262e2296e8582b78dbc6a2a730761ab78228e0e3ddeefdf3` |
| `AUTHOR.md` | `6d0756b3f589211d55bb0cb93d47ab84b4a6f80ba41a03d24d9247330d7fe242` |
| `README.md` | `ae5e6f868897d4d1d58ad12e72acbbc5ab0f198269cc24e426b43b8f38b8d91b` |
| `data/file-manifest.json` | `d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a` |
| `data/source-manifest.json` | `0e40aa023462dd67f41e07d1c0bbaadcf11293af03f381db4fddf894eada4897` |
| `data/runtime-closure.json` | `0ee773adcc05c245f7fe5487410fc4dbd8490d74240e3c3a2c3cb4c7790f1f7f` |
| `data/spec-manifest.json` | `e6059e0c3d41196cc55e801491d42395197eba4a18d9db761b4838261254e9b0` |
| `data/json-test-suite-manifest.json` | `b4df1d9d69075bce2d1b082803986b00845422b258daca77c5da0b2dcb7427df` |
| `data/ecmascript-number-corpus.json` | `0c679aeafc8de364eb2e2ddbc02357950f611cbb5cb508c562d9f2b35e302d19` |
| `fixtures/manifest.json` | `12f9da36fea88a33787e552a614d2798a39e283e72a2708168ff005931a5231e` |

Relevant live source identities also match the source manifest: parser
`d5e41eaa02a3d81aeb09352d98f83ab622a071f425d4f9d7a183c65a2042ffd5`,
projection
`4be760cac40f3a3c4143518057ba46799848dae774c5e655c073de10ca12fb5a`,
command adapter
`599be1c14e604cdef1e6953769f640a63dea862b050baad2eb8824f33a80895f`,
generator
`e7e895412280b605532d4eeed8f3e07066ded32845447e67b50180f02582a133`,
public-route tests
`7d4576c51ac1f980164c9cd9cd029fe5495d89bf865bc341135da50b3148365b`,
and verifier
`13749d48ed1c763279cb822f5914cd04704e8c7b819cb9daa3d9db0ad0320336`.

## Ten-observation ruling ledger

| Observation at its stated scope | Ruling | Consequence |
| --- | --- | --- |
| A1 — invalid public output budgets lose declared precedence on corrupt documents | **CORRECT** | Repair-bearing |
| A2 — an interpreter optimization startup mode is absent from the claimed execution identity | **CORRECT** | Repair-bearing |
| A3 — an admitted base lone-surrogate ceiling shape exceeds the RSS policy | **CORRECT** | Repair-bearing |
| B1 — ambient startup controls permit executed work absent from the claimed identity | **CORRECT** | Same owner as A2 |
| B2 — base JSON behavior is complete and internally consistent under the declared policy | **UNPROVEN** | Narrow exercised facts retained |
| B3 — RFC 8785 behavior survived the stated independent differential checks | **CORRECT** | Test-scope fact only |
| B4 — all public configuration domains and precedence rules are sound retained facts | **INCORRECT** | Directly conflicts with A1 |
| B5 — admitted resource shapes remain sound | **INCORRECT** | Directly conflicts with A3 |
| B6 — generation is target-independent and sealed file evidence is otherwise coherent | **CORRECT** | Narrow byte-evidence fact |
| B7 — this is a suitable W0 oracle organ with no product or candidate standing | **CORRECT** | Architectural boundary retained |

Totals across all ten observations: **7 CORRECT, 2 INCORRECT, 1 UNPROVEN**.

## Individual proofs

### A1 — CORRECT

The declared policy requires invalid budgets to fail before input or document
inspection. Both public output-producing APIs inspect document integrity before
their inner constructors validate the supplied output cap. The tests pair an
invalid cap only with an intact document and a corrupt document only with an
ordinary cap. The missing cross-product deterministically returns integrity rather
than configuration. Separate typed failures survive; declared precedence does not.

### A2 — CORRECT

The runtime descriptor records selected interpreter startup facts but omits the
optimization startup mode. Replay starts from ambient process state. Critic A's
accepted alternate-mode run retained the ordinary descriptor and sealed-root
identity. Current verifier and runtime-manifest structure confirm that this mode
cannot be distinguished. Recorded runtime-tree bytes and ordinary-mode outputs
survive; exact executing-configuration closure does not.

### A3 — CORRECT

Base JSON admits escaped lone-surrogate facts, stores them as structured scalar
records, and expands them in the public observation. The enforced simultaneous
shape instead uses scalar-valid names and a scalar-valid tail. Critic A's admitted
base-only shape reached the exact published counters and remained inside the
16 MiB output policy, yet three sealed-platform RSS samples exceeded the declared
128 MiB ceiling. Exact portable RSS values are not inferred; one admitted
sealed-platform counterexample is sufficient to falsify the envelope.

### B1 — CORRECT

The verifier's producer identity queries, number producers, and replay children
inherit ambient startup state not represented in the runtime descriptor. Critic B
demonstrated additional startup-loaded code and changed interpreter semantics under
the same claimed identity. Source inspection confirms that inheritance path. This
is the same execution-identity mechanism as A2, with broader child-runtime reach.

### B2 — UNPROVEN

The heading claims complete base behavior, not merely success on enumerated cases.
The sealed 318-row census, policy mapping, parser tests, and source inspection
support strict UTF-8, BOM precedence, complete-input parsing, exact number lexemes,
decoded facts, duplicates, spans, source ownership, and intentional base acceptance
of non-scalar and extreme-number spellings on the exercised scope. They do not form
an exhaustive proof of every base input and interaction under the declared policy.
No base grammar counterexample is established here, so the proper ruling is
UNPROVEN rather than INCORRECT; the named exercised subfacts remain evidence.

### B3 — CORRECT

The claim is scoped to surviving the stated checks. The critic records zero
mismatches for 30,014 decimal-number cases and 5,000 nested mixed-value cases,
with exact result digests. The sealed evidence separately binds 104 threshold rows
and a 100,000-value binary64 differential. Current projection structure agrees
with the checked duplicate, scalar, ordering, escaping, and finite-number policies.
This ruling does not promote finite differentials into complete RFC 8785 totality,
and the startup-identity defect narrows the execution receipt without creating an
observed semantic mismatch.

### B4 — INCORRECT

B4's all-public retained-fact heading conflicts directly with A1. Its own body
acknowledges that a corrupt document is inspected before an invalid output cap.
That behavior may be typed and deterministic, but it violates the declared rule
that invalid configuration wins before inspection. Determinism of a wrong
precedence cannot establish public configuration conformance.

Narrower subfacts survive: exact integer domains are enforced when reached;
command configuration precedes file acquisition; source type, input size, BOM,
UTF-8, and syntax have the stated parser ordering; ordinary scalar and canonical
route ordering is deterministic; and accepted and rejected command results retain
their typed status behavior. The corrupt-document/invalid-cap cross-product does
not survive.

### B5 — INCORRECT

B5's admitted-resource heading conflicts directly with A3. Its 14-shape matrix,
scalar-valid simultaneous-ceiling samples, repeated-output digest, and hash-seed
checks may all remain true for those exercised inputs. Output and projection byte
construction are also explicitly capped. None of those narrower facts covers the
base-valid structured lone-surrogate combination. Because that admitted shape
exceeds the public RSS policy, the universal admitted-shape soundness claim is
false.

### B6 — CORRECT

Current number generation constructs metadata from checked constants, runtime
identity, producer bytes, and fresh output; it does not read the corpus target.
Dedicated sealed tests cover poisoned and absent targets. The complete live
41-record census matches its manifest, and source/test/verifier records match the
source manifest. This ruling is limited to target non-inheritance and sealed byte
coherence. B1 still prevents the producer execution environment from being called
an exact closure, as B6 itself expressly recognizes.

### B7 — CORRECT

The governing wave defines a narrow JSON carrier as an assay organ rather than a
portfolio implementation. Current cross-subject inspection found no product import
of this implementation. It lacks the generalized grammar-derived types,
value/tape surface, CSS scope, target parity, package, consumer, and complete
vertical required for standing. Its appropriate status remains external oracle
input only.

## Fact count versus repair mechanisms

The ten rulings are claim counts, not repair counts. Seven correct observations
include four adverse findings and three narrow retained facts. The four correct
adverse findings collapse to three owning mechanisms because A2 and B1 are the
same startup-identity defect. B4 and B5 are incorrect closure claims contradicted
by A1 and A3; they do not add duplicate owners. B2 requires evidence-scope
narrowing but introduces no fourth implementation defect.

The merged mechanisms are:

1. **Public output-cap precedence** — A1 owns the defect; B4 is the contradicted
   closure claim.
2. **Interpreter and child-process startup identity** — A2 and B1 merge here.
3. **Base lone-surrogate representation and resource coverage** — A3 owns the
   defect; B5 is the contradicted closure claim.

## Ordered repair prescription

1. Land external failing gates for the corrupt-document/invalid-cap matrix, altered
   startup identity, and the admitted base lone-surrogate simultaneous-ceiling
   shape. Preserve the existing scalar-valid resource gate.
2. Validate each public output cap at the outer boundary before document integrity
   inspection, while retaining integrity precedence for valid caps.
3. Close interpreter and child-process startup identity with a sealed minimal
   process description, rejection of unbound startup modes before subject import,
   governed child environments, and negative evidence that altered startup-loaded
   work cannot reach tests or producers under the ordinary receipt.
4. Compact the base surrogate-fact representation or reduce active defaults until
   the admitted counterexample satisfies both output and sealed-platform RSS
   policy without weakening typed failure or complete observation.
5. Narrow completeness prose to exercised evidence, run the complete local proof,
   and refreeze runtime, source, file, README, author, and external receipt
   identities only after all three mechanisms pass.
6. Require two fresh consecutive hostile passes over the corrected seal.

## Credit, movement, and terminal disposition

This assay receives **zero candidate, product, lock, profile, or production
credit**. Correct local facts do not authorize architecture selection or a product
edge.

| Plane | Decision |
| --- | --- |
| Tranche fit | **RETAIN LOCAL ASSAY** |
| Wave fidelity | **REOPEN / RED** |
| Feature truth | **FAIL on the three merged mechanisms; retain unrelated facts only at their stated exercised scope** |
| Consecutive clean count | **0 of 2 required** |
| Candidate and product movement | **DENIED** |
| Greater-product movement | **PROHIBITED** |

Terminal result: **NOT CLEAN — REOPEN JSON W0**. Movement remains prohibited
pending ordered correction, reseal, hostile adjudication, and two consecutive
clean passes.
