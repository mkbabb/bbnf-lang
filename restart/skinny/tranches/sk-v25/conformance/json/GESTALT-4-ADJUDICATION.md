# GESTALT-4 adjudication — JSON W0 byte oracle

Date: 2026-07-19 (`America/New_York`)  
Disposition: **NOT CLEAN / REOPEN LOCAL ASSAY**  
Movement: **PROHIBITED**  
Current subject seal: `data/file-manifest.json` SHA-256
`d3cf8521991b1bac5dd20d1d741b78ce7aa63be24e48a1bf7c8c703ec6b4c48a`

## Adjudication boundary

This is a fresh third-party adjudication of the two GESTALT-4 critic reports
against the current sealed JSON assay. The earlier repair receipt and
adjudication were read as evidence, not authority. Source, tests, verifier,
receipt, and manifests were inspected read-only. No subject byte was changed.
This append-only report is excluded from the subject census by the declared root
review namespace.

The live file-manifest audit found 41 declared records, 41 live sealed paths,
path uniqueness, and exact size and SHA-256 agreement for every record. That
establishes which bytes were reviewed; it does not make behavioral closure claims
self-proving.

## Exact inputs and evidence anchors

| Input or anchor | SHA-256 |
| --- | --- |
| `../../LOCK-CONSTITUTION.md` | `b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5` |
| `../../waves/BB-VALUE-TAPE-P0.md` | `745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562` |
| `GESTALT-4-CRITIC-A.md` | `caedb328ce76f26bb5ba9291b0393a21c5706763f8dda6528bb2ce62c4633fed` |
| `GESTALT-4-CRITIC-B.md` | `d08dfddab887e4c264b1b9c7a81699e9d9423f6ea5d393447d353e85c7c221ba` |
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

The current source-manifest records independently matched the live parser,
projection, command adapter, generator, four test modules, and verifier. The
especially relevant live identities are parser
`d5e41eaa02a3d81aeb09352d98f83ab622a071f425d4f9d7a183c65a2042ffd5`,
projection
`4be760cac40f3a3c4143518057ba46799848dae774c5e655c073de10ca12fb5a`,
command adapter
`599be1c14e604cdef1e6953769f640a63dea862b050baad2eb8824f33a80895f`,
public-route tests
`7d4576c51ac1f980164c9cd9cd029fe5495d89bf865bc341135da50b3148365b`,
and verifier
`13749d48ed1c763279cb822f5914cd04704e8c7b819cb9daa3d9db0ad0320336`.

## Per-finding rulings

| Finding | Ruling | Owning mechanism |
| --- | --- | --- |
| Critic A finding 1 — invalid public output budgets lose precedence on corrupt documents | **CORRECT** | Public output-cap validation order |
| Critic A finding 2 — optimization startup mode is outside the sealed execution identity | **CORRECT** | Interpreter and child-process startup identity |
| Critic A finding 3 — admitted base lone-surrogate ceiling shape exceeds the RSS policy | **CORRECT** | Base observation representation and resource gate coverage |
| Critic B finding 1 — ambient startup controls can alter executed work without altering the claimed identity | **CORRECT** | Same startup-identity mechanism as A2 |

There are **4 correct, 0 incorrect, and 0 unproven adverse findings**. Critic B's
observations 2 through 7 are expressly framed as retained facts rather than
adverse findings, so they are not added to the finding count. Their closure scope
is narrowed below where necessary.

### A1 — correct

The declared configuration policy says an invalid budget fails before input is
read or inspected. The outer observation entry validates document integrity
before the inner observation constructor validates its public output cap.
Canonical projection has the same order: integrity validation precedes the inner
projection-cap validator. A corrupt private document paired with an invalid cap
therefore produces the integrity failure, whereas the same cap paired with an
intact document produces the configuration failure.

The sealed tests cover an invalid observation cap only with an intact document,
and corrupt-document projection only with an ordinary cap. They do not cover the
precedence cross-product. The narrow retained fact is that both failure classes
are typed and deterministic when exercised separately. The broader claim that
all public budget domains satisfy the declared precedence is false.

### A2 and B1 — correct and merged

The runtime descriptor binds selected interpreter flags and selected environment
values, but omits the interpreter optimization startup mode and does not describe
the corresponding startup controls for the JavaScript producer runtime. Runtime
identity queries, producer executions, and replay children inherit ambient process
state. The enforced descriptor therefore cannot distinguish the ordinary run from
an execution with changed interpreter semantics or additional startup-loaded code.

Critic A supplied an accepted alternate optimization-mode verifier run. Critic B
supplied both the same identity collision and an accepted run with additional
startup-loaded JavaScript code. Current source inspection confirms the enabling
mechanism: the omitted startup-mode fields, implicit inheritance for producer
children, and replay environments derived from the ambient parent. Matching
ordinary outputs and matching runtime-tree hashes do not bind code executed through
those unrecorded controls.

The narrow retained facts are the recorded runtime-tree bytes, declared runtime
versions, and outputs observed in the documented startup mode. The overbroad claim
of exact executing-configuration identity is not retained. A2 and B1 are one
closure defect, not two independent repair mechanisms.

### A3 — correct

The resource policy covers accepted adversarial shapes, while the public
simultaneous-ceiling regression uses scalar-valid short names plus one scalar-valid
tail. Base parsing also admits escaped lone-surrogate facts. Each such fact is
retained as a structured scalar record and expanded again into the public node
observation. The current regression does not exercise that representation at the
simultaneous decoded-unit, node, entry, and input ceilings.

Critic A's base-valid evidence reaches exactly 1,048,576 input bytes, 131,072
decoded units, 59,999 nodes, 29,999 unique entries, and depth one. It is accepted,
its 14,956,823-byte output including the terminator remains inside the 16 MiB
output policy, and three fresh-process peaks were 142,163,968, 141,262,848, and
140,296,192 bytes. Each exceeds the declared 134,217,728-byte ceiling. Source
inspection confirms that the shape is admitted and that the retained surrogate
facts follow the claimed expansion path.

The ruling is narrow: the reported sealed-platform counterexample falsifies the
current accepted-shape envelope. It does not assert portable peak values, and it
does not implicate the scalar-required or canonical routes because this particular
shape is intentionally base-only. Critic B's measured scalar-valid matrix remains
useful evidence for those exercised shapes, but its broader resource-closure title
cannot survive the admitted base counterexample.

## Retained facts and rejected closure

- The current 41-record seal and source-manifest are internally exact.
- Target-absent number generation and target-metadata non-inheritance remain
  supported by current source structure and dedicated sealed tests.
- The 318-row suite census, checked number artifacts, and fixture identities remain
  retained evidence on their exercised scope.
- Typed parser, scalar, projection, and ordinary resource behavior remains retained
  where directly exercised by the sealed evidence.
- None of those facts closes the corrupt-document/output-cap precedence cross-product,
  the executing startup configuration, or the full admitted base resource domain.
- The assay remains an external input oracle. This adjudication authorizes no
  architecture selection, product import, candidate advancement, or production
  edge.

## Merged repair obligations

1. Validate each public output cap at the outer API boundary before document
   integrity inspection. Add the complete corrupt-document/invalid-cap matrix and
   preserve the existing integrity result for valid caps.
2. Close interpreter and child-process startup identity as one mechanism. Reject
   unbound startup modes before subject import, construct governed child
   environments from a sealed minimal description, bind the relevant interpreter
   flags and startup-loaded-code controls, and add negative gates proving altered
   startup identity cannot reach tests or producers under the ordinary receipt.
3. Cover the admitted base lone-surrogate simultaneous-ceiling shape in a fresh
   public-process gate. Compact its immutable representation or reduce active
   defaults until every accepted sample satisfies both the existing output policy
   and the sealed-platform RSS ceiling. Retain the scalar-valid multi-route gate;
   it covers a distinct region of the admitted domain.
4. Refreeze the runtime description, source manifest, file manifest, README, and
   external receipt only after all three owning mechanisms pass. Then require two
   fresh consecutive hostile passes over the new seal.

## Movement decision and terminal disposition

| Plane | Decision |
| --- | --- |
| Tranche fit | **RETAIN LOCAL ASSAY** — the byte oracle remains a suitable bounded falsification organ. |
| Wave fidelity | **REOPEN / RED** — failure precedence, evidence identity, and accepted-shape resources are open. |
| Feature truth | **FAIL ON THE THREE MERGED MECHANISMS**; unrelated exercised JSON facts remain narrow evidence only. |
| Greater-product movement | **PROHIBITED** — the defects are local and create no product authority or edge. |
| Consecutive clean count | **0 of 2 required**. |

Terminal result: **NOT CLEAN — REOPEN JSON W0**. The current seal must not move
forward as closed evidence. Movement remains prohibited pending correction,
reseal, hostile adjudication, and two consecutive clean passes.
