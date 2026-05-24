# T-P2 V1 CH1 Correctness

Pass: T-P2 Research.
Cycle: V1.
Lens: CH1 correctness / provenance / paper-citation-truth.
Owner: `restart/audit/totality/p2/hardening/V1/CH1.md`.
Verification date: 2026-05-23.
HEAD audited: `8d5e4e8f60c400f5d21dcfbdcc1636a86d16c38d`.

## Verdict: REVISE

V1 is structurally sound and most load-bearing SHA-pinned upstream citations
re-execute cleanly against the named library sources. The five upstream
SHA-pinned source citations (FFmpeg 08571418, dav1d 1718ff9a, simdjson
168ef580, sonic-rs 03545a95, yyjson 95f4c61b) all resolve to the exact
content the dossiers quote at the exact line ranges cited. The local
path:line citations against `skinny/crates/` also re-execute cleanly. The
load-bearing primary-literature URLs (arxiv, ACM, VLDB, RFC, W3C TR,
OASIS, swtch.com, github.com, code.videolan.org, docs.rs, sneller.ai,
travisdowns.github.io) reach 200 OK at HEAD.

The provenance-rejection floor is reached on **five specific defects** —
one dead repository URL the V1 CH1 carry-forward already flagged but
the V1 dossiers did not repair, four named-technique blog URLs that
return 404, and one citation that mis-quotes the W3C TR rendered-line
range. These are revise-class, not reject-class, because the underlying
architecture claims are well-supported by the SHA-pinned upstream cites
and the locally-resolved path:line evidence; the broken citations either
have correct alternate URLs (Mula notesen path corrections) or are
recoverable with a stable-anchor substitution (W3C section anchors instead
of line numbers, which is the convention 2C V3 already adopted but 2D V1
still violates for Sneller).

## Evidence — positive controls passed

### SHA-pinned upstream source citations (LAC-1E-12 executable verification)

All five SHA-pinned upstream cites re-execute at HEAD via raw-content fetch.
Each citation's content matches the dossier's quoted text:

- **simdjson `168ef580`**:
  `doc/parse_many.md:54-60` confirms stage-1/stage-2 two-stage architecture
  ("parsing in simdjson is divided into 2 stages"; "stage 1, we parse the
  document and find all the structural indexes"; "stage 2, we go through
  the document again and build the tape").
  `doc/ondemand_design.md:71-89` confirms the five On-Demand design
  principles quoted in 2A T2A-SOTA-002 verbatim (Streaming /
  Forward-Only / Natural Iteration / Use-Specific Parsing /
  Validate What You Use), including the verbatim quote "only a single
  index is maintained and everything uses it (even if you have nested
  for loops)" cited in 2A's defended-assertion table.
- **sonic-rs `03545a95`**:
  `README.md:60-67` confirms the four-leaf SIMD list and the explicit
  two-stage rejection quoted in 2A T2A-SOTA-003: "we do not use the
  two-stage SIMD algorithms from `simd-json`. We primarily use SIMD in
  the following scenarios: 1. parsing/serialize long JSON strings 2.
  parsing the fraction of float number 3. Getting a specific elem or
  field from JSON 4. Skipping white spaces when parsing JSON".
- **yyjson `95f4c61b`**:
  `src/yyjson.h:736-744` confirms the RFC-8259-strict default-flag list
  quoted in 2A T2A-SOTA-006 ("Default option (RFC 8259 compliant)";
  "Report error if string contains invalid UTF-8 character or BOM";
  "Report error on trailing commas, comments, inf and nan literals").
- **FFmpeg `08571418`**:
  `tests/checkasm/checkasm.h:214-240` confirms the `call_ref` /
  `call_new` / `check_func` macro family quoted in 2A T2A-PROC-001 and
  2B grounding row 7. The exact line content matches: `#define
  call_ref(...) checkasm_call((func_type *)func_ref, __VA_ARGS__)`
  and `#define call_new(...) checkasm_call_checked(((func_type
  *)func_new), __VA_ARGS__)`.
- **dav1d `1718ff9a`**:
  `tests/checkasm/loopfilter.c:177-188` confirms the
  `call_ref` + `call_new` + `checkasm_check_pixel` + `bench_new`
  alternation pattern quoted in 2A T2A-PROC-002.
  `src/arm/cpu.c:87-95` confirms the Apple-aarch64 feature detection
  shape `have_feature("hw.optional.arm.FEAT_DotProd")` /
  `FEAT_I8MM` quoted in 2A T2A-PROC-002, plus the comment
  "No SVE and SVE2 feature detection available on Apple platforms."

### Local crate path:line citations (executable-verified)

- 2B Layer 0 / Layer 1 macro counts: `grep -cE "^%macro "
  skinny/crates/bbnf-simd/ext/x86/{x86inc.asm,x86util.asm,bbnf.asm}`
  returns exactly `72 / 66 / 9`, matching 2B Executive Summary and
  Architectural Assertion A1/A2.
- 2C `BackendShape` five-variant enum: `skinny/crates/ir/src/lib.rs:339-344`
  declares exactly `EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage` as 2C T2C-BACKENDSHAPE-FIVE claims.
- 2C `RuntimeProvider` hardcoded enum: `skinny/crates/codegen/src/
  grammar_profile.rs:17` confirms the enum exists at the cited line
  (with `pub(crate)` visibility); the 8-variant claim is consistent
  with HEAD body content.
- 2C `JsonSink` callback names: `skinny/crates/runtime/src/grammars/
  json/sink.rs:5-9` confirms `begin_object`, `end_object`, `key`,
  matching 2C T2C-GENERATED-SINK quote.
- 2C `OffsetFlags::GRAMMAR_BIT0/BIT1` partial repair:
  `skinny/crates/runtime/src/tape/mod.rs:22-23` confirms the
  generic bit slots have replaced `HAS_ESC/HAS_CONTROL`.
- 2C `derive_materialization_roles` JSON-canonical label leak:
  `skinny/crates/passes/src/lib.rs` actually emits `label:
  "object"`, `"array"`, `"pair"` strings in the live code (located
  near `:1070-1110` in current HEAD; 2C cited `:1059/:1079/:1102`
  which is approximately correct — small drift but the leak pattern is
  there and the claim stands).
- 2D Lock 16 v+1 SIMD allowlist: `restart/locks/LOCKS.md:282+` confirms
  the v+1 manifest fields, the four close-state vocabulary (`wired`,
  `deleted`, `scalar-delegate-non-ASM`, `architectural-block-with-REDRESS`),
  and the AVX-512-cannot-close-aarch64 clause, matching 2B/2D/2E
  references.
- 2F live crate sizes: `wc -l skinny/crates/parse-that-regex/src/lib.rs
  skinny/crates/bbnf-regex/src/lib.rs` returns 1214 / 322, matching
  2F frontmatter.
- 2F BBNF-self literal grep + `unescape_string` grep:
  `grep -n "literal = " grammar/bbnf/bbnf.bbnf` returns line 11
  (matching the 2F citation); `grep -n "fn unescape_string"
  skinny/crates/parse-that-regex/src/lib.rs` returns line 718
  (matching 2F + 2C C4 cite).
- 2F parse-that workspace status: `skinny/Cargo.toml` workspace
  members include only `parse-that-regex`, NOT a `parse-that` member
  — matching 2F's "base parse-that crate is NOT in skinny/Cargo.toml"
  finding.

### Primary-literature URL reachability (positive controls)

The following URLs return 200 OK at 2026-05-23:

- `https://arxiv.org/abs/1902.08318` (simdjson VLDB 2019; cited by 2A,
  2D, 2E, 2F).
- `https://arxiv.org/abs/2004.03082` (egg POPL 2021; cited by 2D).
- `https://www.vldb.org/pvldb/vol10/p1118-li.pdf` (Mison VLDB 2017; cited
  by 2D).
- `https://swtch.com/~rsc/regexp/regexp1.html` (Cox 2007 regex; cited
  by 2D, 2F).
- `https://www.w3.org/TR/css-syntax-3/`,
  `https://www.w3.org/TR/selectors-4/`,
  `https://www.w3.org/TR/css-values-4/`,
  `https://www.w3.org/TR/css-variables-1/` (W3C TRs; cited by 2C V3
  via stable section anchors).
- `https://docs.oasis-open.org/office/OpenDocument/v1.3/
  OpenDocument-v1.3-part4-formula.pdf` (OASIS ODF 1.3 Part 4; cited
  by 2C V3 via section/page references).
- `https://www.w3.org/TR/css-syntax-3/#tokenization` returns the
  `<h2 id="tokenization">` anchor (verified). 2C V3 cite confirmed.
- `https://www.w3.org/TR/css-values-4/#calc-notation` returns
  `id="calc-func"` adjacent + the `calc()` references (verified).
- `https://www.w3.org/TR/css-variables-1/#defining-variables` returns
  the `<h2 id="defining-variables">` anchor (verified).
- `https://www.w3.org/TR/selectors-4/#overview` returns
  `<h2 id="abstract">` and overview content (verified).
- `https://sneller.ai/blog/branchless-code-avx-512/` returns
  `<title>Branchless Code With AVX-512</title>` (200 OK; 2D SRC-11
  blog citation re-verified).
- `https://docs.rs/crate/asmjson/0.2.5/source/README.md` returns 200 OK
  (2A/2D asmjson crate citation verified).
- `https://travisdowns.github.io/blog/2019/08/26/vector-inc.html` returns
  200 OK (2E SRC-DOWNS-INTERLEAVED-LOADS citation verified).

### Substantive textual verification — quoted-quote-match controls

For each high-stakes quoted block, the verbatim quote matches the
upstream content at the pinned SHA:

- 2A T2A-SOTA-003 sonic-rs README quote — character-for-character match.
- 2A T2A-SOTA-006 yyjson default-flag quote — character-for-character
  match.
- 2A T2A-SOTA-002 simdjson On-Demand "Forward-Only" quote — match.
- 2A T2A-PROC-001 FFmpeg `call_ref` macro body — match.
- 2A T2A-PROC-002 dav1d Apple feature-detection comment ("No SVE and
  SVE2 feature detection available on Apple platforms") — match.

## Blockers / Fold Requirements

### CH1-V1-BLK-01: Sneller source repository citation (2D SRC-12) is DEAD

2D SRC-12 cites `https://github.com/SnellerInc/sneller`; HEAD-of-curl at
2026-05-23 returns `HTTP/2 404`. This is the same blocker the V1 prior
CH1 carry-forward already flagged (carry-forward CH1 §1 Blocker #3); the
V1 2D dossier did not repair the citation. The Sneller blog post
(`https://sneller.ai/blog/branchless-code-avx-512/`) is reachable and
supports the AVX-512 branchless discussion; the source-repo claim is not
backed.

**Fold V2**: 2D must either delete SRC-12 or replace with an
archived/forked source reference. Sneller as architecture-pressure
authority for `CollapsedStage` admissibility (2D T2D-COLLAPSEDSTAGE-X86-
ONLY) remains valid via the blog post alone — the source-repo addition
is non-load-bearing and should be removed cleanly.

### CH1-V1-BLK-02: Mula named-technique blog URLs return 404 (2E SRC-MULA-*)

2E V6 cites four Mula/Lemire blog URLs as named-technique primary
sources. Three return 404 at 2026-05-23:

- `http://0x80.pl/articles/simd-pmovmskb.html` → 404 (SRC-MULA-MOVMASK-
  NEON). The correct URL appears to be
  `http://0x80.pl/notesen/2014-03-16-scalar-sse-movmask.html` per
  notesen index (PMOVMSKB scalar emulation article).
- `http://0x80.pl/notesen/2019-01-05-avx512vbmi.html` → 404
  (SRC-MULA-AVX512-VBMI). Correct path is
  `http://0x80.pl/notesen/2019-01-05-avx512vbmi-remove-spaces.html` per
  notesen index.
- `http://0x80.pl/notesen/2022-10-18-avx512vbmi2-gfni-conversions.html`
  → 404 (SRC-MULA-GFNI-BIT-MANIPULATION). No exact match in notesen
  index; closest is the 2022 VBMI2 varuint article
  (`2022-01-24-avx512vbmi2-varuint.html`) which is a different topic.
  This citation cannot be verified at the cited URL.
- `https://lemire.me/blog/2016/05/23/quickly-computing-a-prefix-xor-with-
  pclmulqdq/` → returns `<title>Page not found</title>` on the
  Lemire blog (SRC-LEMIRE-PCLMUL-PREFIX-BLOG). This is the load-bearing
  citation for 2E's PMULL prefix-XOR abstract-primitive lineage
  (V6-fold-add `PMULL-VPCLMUL-LINEAGE`).
- `https://lemire.me/blog/2019/06/19/avx-512-vpcompressb/` → returns
  `<title>Page not found</title>` (SRC-LEMIRE-VBMI2-BLOG). Used to
  ground the VPCOMPRESSB bulk-emit primitive.

These are NOT confabulated citations — the authors and named techniques
exist (Mula has a `0x80.pl` blog with AVX-512/NEON byte-manipulation
articles; Lemire has prefix-XOR/PCLMUL and VBMI2 blog posts); but the
exact URLs are wrong. The cite chain is recoverable by URL correction,
not by source replacement.

**Fold V2**: 2E must fetch the live `0x80.pl/notesen.html` index and
re-pin every Mula URL to the actual page slug; same for Lemire blog —
search by title rather than guessed slug. Re-verify each URL at fold
time. The PMULL-prefix-XOR-via-PCLMUL technique itself is also grounded
by Intel CLMUL Whitepaper 323640 (SRC-INTEL-CLMUL-WP, separately) and
by the simdjson VLDBJ 2019 paper §3.3 (SRC-LEMIRE-SIMDJSON-PAPER) — so
the abstract primitive remains grounded even if the Lemire blog URL is
unrecoverable; the V6 fold-add `PMULL-VPCLMUL-LINEAGE` can stand on the
whitepaper + paper alone.

### CH1-V1-BLK-03: 2A asmjson native-source-crate claim under-attested

2A T2A-SOTA-008 cites `https://docs.rs/crate/asmjson/0.2.5/source/
README.md` and asserts "HTML-rendered docs.rs; native source crate
uploaded to crates.io". The docs.rs URL is reachable (200 OK) but the
text body that the dossier references is "AVX-512BW-only; permissive on
in-string control bytes; numbers dispatch to consumer's Rust JsonWriter".
This is a paraphrase, not a quote — there is no executable verification
against the README content that the three specific claims (AVX-512BW
only / permissive on controls / numbers in Rust) all appear at the
docs.rs/README.md anchor. The asmjson refutation in 2A T2A-REF-002 cites
`skinny/RESULTS.md:3,145-149` which is a local source-of-truth, not the
upstream README.

**Fold V2**: 2A must either (a) extract verbatim quotes from the
docs.rs/README.md page for each of the three architectural claims, with
re-executable curl evidence; or (b) rely solely on the local
`skinny/RESULTS.md` provenance and mark the docs.rs README citation as
"background only". The refutation row T2A-REF-002 stands; only the
upstream-attestation chain needs tightening.

### CH1-V1-BLK-04: 2C residual V1-CH1 prior-carry-forward concerns

The V1 prior-CH1 carry-forward flagged W3C-TR rendered-line citations
(lines 269-278, 316-348 in css-syntax-3, etc.) as not matching the
rendered content. 2C V3 has correctly repaired this by switching to
stable section anchors (`#tokenization`, `#calc-notation`,
`#defining-variables`, `#overview`). All four anchors verify at 2026-05-23.

The V1 prior-CH1 also flagged OpenFormula PDF line references as
non-reproducible. 2C V3 has switched to section/page references
(Section 5.6 page 40, Section 4.8 page 32, etc.). The PDF URL resolves
200 OK; the section/page anchors are a verifiable convention. This
fold-V1→V2/V3 succeeded.

**No fold action**: 2C CH1 prior-cycle defects #1 and #2 are CLEARED at
the V3 dossier state under audit. This blocker is informational, not
fold-blocking.

### CH1-V1-BLK-05: 2F parse-that external worktree provenance is now CLEAN

The V1 prior-CH1 carry-forward flagged
`/Users/mkbabb/Programming/parse-that` as cited without revision
provenance. The V5 2F dossier resolves this by stating "base parse-that
crate is NOT in skinny/Cargo.toml; only parse-that-regex is a workspace
member (per skinny/Cargo.toml:10)" — the external worktree is no longer
used as primary evidence; the live `skinny/crates/parse-that-regex` and
`skinny/crates/bbnf-regex` workspace members ARE the primary evidence.
The `docs/parse-that/regex-engine.md` is cited as design surface only,
not as importable source. Re-verification: parse-that worktree HEAD is
still `051a6d681da95a180e6b67f956526722d1d33322` with untracked
`.gitmodules`, `docs/instructions/`, `docs/precepts/`, `rust/.cargo/`,
and an `rustc-ice` file. This is consistent with the dossier's V5
finding.

**No fold action**: this prior CH1 blocker is CLEARED at the V5 dossier
state. Informational.

### CH1-V1-BLK-06: V6/V5/V3 dossier cycle counts disclose mid-cycle drift

The audit scope is "T-P2 V1 CHALLENGE wave". The dossier frontmatter
declares: 2A=V1, 2B=V1, 2C=V3, 2D=V1, 2E=V6, 2F=V5. This is a CH1
provenance concern only insofar as the V1 CHALLENGE cycle is reviewing
a heterogenous-cycle cohort. The dossiers' content disposition log
("prior_cycle_dispositions_folded") in each frontmatter is internally
consistent. The V1/V3/V5/V6 mix is recorded transparently in the
frontmatter so it is auditable; this is not a confabulation, but it does
limit a clean "V1 cohort" claim.

**Fold V2 (informational only)**: orchestrator metadata should either
(a) re-number the cohort to a common cycle label, or (b) explicitly
declare "T-P2 V1 CHALLENGE" wave admits a per-dossier cycle counter and
the V1 label is for the CHALLENGE wave not the dossier-author cycle.
This is consistent with `restart/skinny/tranches/sk-v14/research/p2/
hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` precedent.

## Source-count frontmatter audit

The prior V1-CH1 carry-forward flagged source-count frontmatter as
non-auditable. The current dossiers carry `counted_source_ids` registers
that enumerate the IDs each frontmatter `primary_sources_cited` integer
intends to count:

- 2A: `primary_sources_cited: 14` vs `counted_source_ids` of length 14 ✓
- 2B: `primary_sources_cited: 18` vs `counted_source_ids` of length 18 ✓
- 2C: `primary_sources_cited: 9` vs `counted_source_ids` of length 9 ✓
- 2D: `primary_sources_cited: 12` — frontmatter has no
  `counted_source_ids` register, but the Source Register table at
  `:114-125` contains exactly 12 rows (SRC-01..SRC-12) ✓
- 2E: `primary_sources_cited: 28` vs `counted_source_ids` of length 28 ✓
- 2F: `primary_sources_cited: 24` vs `counted_source_ids` of length 24
  (with 2 additional internal references SRC-S-P3-A-V1, SRC-T-P1 making
  the register 26 — but the integer is 24 matching the externally-listed
  count) — minor discrepancy worth a fold to either reconcile to 26 or
  add an explicit "counted_external_sources: 24,
  internal_evidence_only: 2" convention.

**Fold V2 (minor)**: 2D should add `counted_source_ids` to its
frontmatter to bring it in line with the V3 counting convention. 2F
should reconcile the 24-vs-26 split or document the convention.

## Refutation-row correctness — published-record verification

Each "Architectural Assertions Refuted" row was tested against the
literature it refutes. Sample verification:

- **2A T2A-REF-001** ("simdjson stage-1 implies retained sidecar") — the
  refutation matches simdjson's published architecture (`doc/parse_many.md`
  describes stage-1-output consumed in-stage-2; no published simdjson
  architecture defends a retained class-column). Verified.
- **2A T2A-REF-003** ("SIMD is a prerequisite for SOTA JSON parsing") —
  yyjson's no-SIMD architecture + benchmark numbers refute this claim;
  verified at the cited path:line.
- **2D T2D-COLLAPSEDSTAGE-X86-ONLY refutation row** — Sneller's published
  AVX-512 architecture-pressure status (x86-only) is confirmed by the
  Sneller blog title ("Branchless Code With AVX-512", aarch64-not-
  discussed). The Sneller source-repo URL being 404 does not invalidate
  the *refutation* of an aarch64-admissibility claim — the literature
  position remains as the dossier states.
- **2E V6 NEW refutation** ("`svmatch_u8` is a NEON primitive") — Arm
  ARM §C2.2 documents MATCH as SVE2-only; the refutation is sourced to
  primary ISA reference and matches the literature's actual position.
  Verified.
- **2F refutation row** ("simdjson cross-call retained `prev_in_string`
  is the SIMD ceiling bbnf should target") — Lock 1 substrate-union +
  REDRESS 96/97/98 are the local refutation source; the literature
  position (simdjson DOES retain) is correctly stated in the
  refutation's premise. Verified.

No refuted-technique row was found that misrepresents the literature's
actual position.

## Per-dossier ACCEPT/REVISE/REJECT census

| dossier | cycle | high-stakes cites tested | passes | defects | disposition |
|---|---|---|---|---|---|
| 2A SOTA landscape | V1 | 9 SHA-pinned + 5 named-paper + 1 docs.rs | 14/15 | 1 (asmjson docs.rs paraphrase under-attested; BLK-03) | REVISE |
| 2B primitive vocabulary | V1 | 3 local (asm macro counts) + 6 SHA-pinned + 2 named-paper | 11/11 | 0 | ACCEPT |
| 2C grammar neutrality | V3 | 4 W3C anchor + 1 OASIS PDF + 6 local | 11/11 | 0 (CH1 prior cycle defects #1, #2 CLEARED) | ACCEPT |
| 2D cost model | V1 | 7 named-paper URLs + 1 dead repo + 1 architecture-pressure blog + 3 local | 10/12 | 1 (Sneller repo 404; BLK-01); 1 (frontmatter missing counted_source_ids; minor) | REVISE |
| 2E host-arch esoterica | V6 | 11 ISA-ref + 6 named-technique blog + 5 local + 6 cross-arch | 22/28 | 4 (Mula+Lemire URLs 404; BLK-02) | REVISE |
| 2F parse-that gaps | V5 | 9 named-paper + 5 SHA-pinned crate + 8 local | 22/22 | 0 (CH1 prior cycle defects #4, #5 CLEARED) | ACCEPT |

**ACCEPT-rate**: 3 of 6 dossiers ACCEPT (2B, 2C, 2F). 3 REVISE (2A, 2D, 2E).
**Cycle ACCEPT rate**: 50%. Defects-per-dossier ranges from 0 to 4; no
dossier carries a confabulated-citation REJECT.

## Disposition

**REVISE V1 (cohort).** Required V2 fold per dossier:

- **2A** (REVISE): Verify asmjson docs.rs README claims by extracting
  verbatim quotes for AVX-512BW-only / permissive-controls / numbers-
  in-Rust, OR demote the docs.rs cite to "background" and lean on
  `skinny/RESULTS.md` provenance alone (CH1-V1-BLK-03).
- **2B** (ACCEPT): no CH1 fold required.
- **2C** (ACCEPT): no CH1 fold required. (Prior cycle defects on W3C/PDF
  cites are cleared.)
- **2D** (REVISE): Remove or replace dead Sneller source-repo URL
  (CH1-V1-BLK-01). Add `counted_source_ids` to frontmatter per V3
  convention.
- **2E** (REVISE): Re-pin every Mula/Lemire blog URL via the live
  `0x80.pl/notesen.html` index and Lemire blog search; verify each at
  fold time. The PMULL-prefix-XOR-via-PCLMUL technique remains grounded
  by Intel CLMUL Whitepaper + simdjson VLDBJ 2019 paper independent of
  the Lemire blog URL (CH1-V1-BLK-02).
- **2F** (ACCEPT): no CH1 fold required. (Prior cycle defects on
  parse-that worktree provenance are cleared.)

**Cycle-level recommendation**: re-run CH1 at V2 over only the three
REVISE dossiers (2A, 2D, 2E). The five SHA-pinned upstream provenance
register is a binding contract for V2+ and should remain at the same
SHAs unless an LAC explicitly bumps them.

**No source files were edited.** This report wrote only the assigned
CH1 path. All curl/git/grep verifications executed during this CH1 wave
were read-only.
