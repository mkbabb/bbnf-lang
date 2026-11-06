# BB-C.W0 G3 deletion R3 — repair author record

MODEL: `gpt-5.6-sol`; REASONING: `ultra`; WORKFLOW: `v2`

Status: **R3 TEST REPAIR IMPLEMENTED; EXECUTION EVIDENCE RED; NO R3
COMPLETION CLAIM; HOSTILE PASSES REMAIN ZERO.**

This record claims only the adjudicated G3 R3 repair. It claims no BB-C.W0
completion, optimizer, graph, package, performance, wave, movement, or BB-A
credit. No file was staged or committed.

## Frozen subject

The only tracked R3 edit is `crates/core/tests/egraph_semantic.rs`. Accepted
production and IR-test bytes stayed frozen.

| Path | SHA-256 |
|---|---|
| `crates/ir/src/egraph/rules/universal.rs` | `9835616cc8900eb5e0e5fde769e34d149bfcdf8e2493f0aef74b958426768b5b` |
| `crates/ir/src/egraph/rules/mod.rs` | `da0a7b1b9120e6d3110582254824fc6cb6509b181aadd13332c39ab80cc897c4` |
| `crates/ir/tests/egraph/egraph_universal.rs` | `e4e9beab261c1a10a52d537f2d773e8f69c051e763918f3d35814d1252576ede` |
| `crates/core/tests/egraph_semantic.rs` | `ef08223fdce747c821a0d11db54057314aaefb8a57bc41c8f14a9ba12b5aeaf2` |

The new independent test is
`egraph_semantic_nullable_alt_required_context_diagnostic`. Its grammar is:

```text
prefix = "!", prefix | "!" ;
start = prefix, nullable, "z" ;
nullable = "x" | epsilon ;
```

It explicitly selects entry rule `start`, rejects input `!`, and places the
direct G3 form `Alt([Literal("x"), Epsilon])` in a required concatenation.
The recursive prefix is intentionally independent of G3: it advances the
failure offset while retaining the nested rule context needed by the current
FOLLOW-set diagnostic API. A direct `nullable, "z"` probe and a
`nullable >> "z"` probe both produced empty diagnostics at offset zero and
were rejected as inadequate.

The final test asserts, in causal order, failure, offset `1`, exactly one
diagnostic at offset `1` for rule `prefix`, expected bytes `[b'x', b'z']`, and
the complete partial value:

```text
Tagged {
    tag: 4,
    span: (0, 1),
    children: [
        Tagged {
            tag: 0,
            span: (0, 1),
            children: [Span(0, 1), Nil],
        },
        Nil,
    ],
}
```

Exploratory frozen probes established the intended causal differential before
the final replay: current bytes reported expected bytes `[120, 122]`, while
pre-deletion G3 bytes reached the reordered diagnostic assertion and reported
`[120]`. Those exploratory streams were not retained as final evidence and
therefore receive no R3 evidence credit.

## Sealed path dependency sources

All replay metadata and attempted compilation resolved the four path packages
only below `/private/tmp/bbnf-g3-r3-8003fa19464878e2`; no active sibling path
appeared in either metadata projection. Locked/offline metadata schema version
`1` was projected with bound `jq-1.8.1`, executable SHA-256
`ef35d26cab5fffd780c5272142dc2d5ae37ae02cae7885204030e48020f74cd1`.

| Package source | Deterministic archive SHA-256 |
|---|---|
| parse-that `rust/parse_that` | `c1582888466c770a58b31c126b3186255c235b8f30c554d5cc706d7f23532835` |
| parse-that `rust/regex` | `c0a804862d70b1fe63e64aff6284f0c25d6a325d4f9aadb4cc33a5114d5ba4f1` |
| pprint `rust` | `315acb5d78b3acbdc96af7fad17ed9fb986b470c430fa94aa4af3381145bfd82` |
| pprint `rust/derive` | `960c182cc37b08b2eef26946ba72db9ff4f5a58ab09b941668a06d78dd1bdc9b` |

Each archive reproduced with identical A/B hashes. Archive inspection found
zero `.git`, `target`, cache, `.DS_Store`, or Python bytecode entries. The
four source trees contained zero symlinks before and after. Independently
generated terminal after-ledgers compare byte-for-byte with all four before
ledgers, proving the active sibling sources were not mutated.

Metadata hashes:

- historical JSON: `096fd158e5f9ebd3f3e8be7037ae72a2c45f9468a67aed9c0a15276324a12f3c`;
- current JSON: `5306db52dfa7c4b65d076ad0c2f89d6b925975a08ae1ae6c8b01d669cdfce3b7`;
- each four-row path projection:
  `22ddd9ef788ceec084104f2f9e116593265e6979e40d8a0a6dea33532fd04127`.

The working, historical, and current-crater locks stayed unchanged:

- working/current: `480ce3dcd92ed7778e5fc922cc531039e187a96258ec78f6fa6937defb6a40e9`;
- historical: `a6a75b64633dac671206b06bea3c8b8b18ccd180b04712dc04dcc4c423160908`.

## Terminal replay blocker

The final unique replay used separate `targets-v2/historical` and
`targets-v2/current` directories. Its recorded common Apple ARM settings were
`-j 1`, incremental off, dev/test debuginfo off, dev/test codegen units `16`,
and an empty `RUSTC_WRAPPER`. Host identity was Apple M5 Max / arm64, macOS
26.4.1, rustc `1.96.0-nightly (02c7f9bec 2026-04-10)`, Cargo
`1.96.0-nightly (eb94155a9 2026-04-09)`, target
`aarch64-apple-darwin`. The identity artifact SHA-256 is
`9ab24510369b88dd6c9ee1be559018b6f0b4efc7ff8bd9712ab865e585c2266a`.

The historical build never reached the test binary. Apple `cc` was killed
while linking Wasmtime's build script:

```text
error: linking with `cc` failed: signal: 9 (SIGKILL)
error: could not compile `wasmtime` (build script) due to 1 previous error
```

Cargo returned `101`, but the hardened replay rejects exit codes unless the
named test, exact oracle marker, and `test result: FAILED` footer are all
present, and rejects any compile, signal, kill, or OOM marker. It therefore
stopped correctly. The raw failure stream SHA-256 is
`477b35c110195ebbc187bc70f70534cfdecd7dbe8e2b5f0d245fa44a7e425a72`;
the exit receipt (`101`) SHA-256 is
`39b8dc3fc8b44765c8e6f1adee04c5b465e555ab791cc42d0d9e810d5b64297c`.

No `runs-sealed-v2/COMPLETE` exists. Consequently there is no valid retained
historical diagnostic RED, no complete set of historical REDs, no current A/B
pair, and no normalized A/B equality receipt. Earlier `runs/`, `runs-final/`,
and `runs-sealed/` attempts are also preserved visibly incomplete and confer
no evidence credit.

## Preserved evidence and terminal status

All nine R1/R2 evidence hashes remain identical; their retained aggregate list
has SHA-256
`2de56d83ab6e31e80b01b000d693527f05a0806955027b0fe6fc57f3c8c25faf`.
The terminal integrity audit verifies archive content addresses, zero forbidden
archive entries, zero symlinks, unchanged source ledgers, unchanged locks, and
the frozen subject hashes above.

The test repair is implemented, but the adjudicated R3 execution-evidence gate
remains **RED** because the host resource failure prevented the required sealed
replay. No production movement follows.
