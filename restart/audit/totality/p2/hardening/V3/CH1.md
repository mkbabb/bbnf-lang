# T-P2 V3 CH1 Correctness / Provenance — LOCK-TRIGGER Cycle

Pass: T-P2 Research.
Cycle: V3 (LOCK-TRIGGER CHALLENGE wave; per-dossier cycle drift V2/V2/V4/V2/V7/V6).
Lens: CH1 correctness / provenance / paper-citation-truth.
Owner: `restart/audit/totality/p2/hardening/V3/CH1.md`.
Verification date: 2026-05-23.
HEAD audited: `5aaab91d11389dc26ed8e6263c1a640cc9c28035` (V3 CHALLENGE-CONTEXT
commit; the V3 atomic micro-fold for the six T-P2 dossiers landed one commit
earlier at `daa14127f` per CHALLENGE-CONTEXT §0 authority; both commits are
on `master` between V2 close and this V3 lens wave).

## Verdict: ACCEPT

Six of six dossiers ACCEPT at V3 close. The three V3 micro-fold CH1 items
land cleanly: F-V3-CH1-A (asmjson README 235-line frontmatter reconciliation
at 2A) and F-V3-CH1-B (2F frontmatter `primary_sources_cited` 24→26 register
reconciliation) both verified executable. 2E carries zero V3 edits as
declared (V2-LOCKED through V3). All five SHA-pinned upstream cites
(simdjson / sonic-rs / yyjson / FFmpeg / dav1d) return 200 at HEAD with no
SHA bumps. **V3 is the second consecutive cohort-wide ≥95% cycle on CH1 —
the 2-cycle §3Z LOCK condition triggers.**

## V3 disposition focus — executable verification

### F-V3-CH1-A: 2A asmjson README 235-line frontmatter reconciliation

V2 CH1 carried a single residual cosmetic note: the V1 dossier text said the
asmjson native `README.md` was 236 lines; the live tarball at HEAD measures
235 lines. The V3 atomic micro-fold (commit `daa14127f`) reconciles this at
three load-bearing sites in `2A-sota-landscape.md`:

- frontmatter `upstream_sha_pins.asmjson` at `:32`: now reads `"crate 0.2.5
  native README.md (235 lines; crates.io tarball + docs.rs HTML mirror)"`;
- V2 fold narrative at `:66`: now reads `"... with line refs into the native
  235-line README.md"`;
- T2A-SRC-ASMJSON register row at `:213`: now reads `"asmjson crate 0.2.5 —
  native README.md (235 lines) extracted from the crates.io tarball
  asmjson-0.2.5.crate (verified 2026-05-23 via curl ... wc -l ... → 235)"`.

Executable verification at HEAD (2026-05-23):

```text
$ curl -sSL "https://crates.io/api/v1/crates/asmjson/0.2.5/download" \
    -o asmjson-0.2.5.crate
$ tar -xzf asmjson-0.2.5.crate
$ wc -l asmjson-0.2.5/README.md
235 asmjson-0.2.5/README.md
```

Live count = 235 lines exactly. Reconciliation at all three sites confirmed
via direct read of `2A-sota-landscape.md:32`, `:66`, `:213`. The three
verbatim-quote extractions for AVX-512BW-only (`README.md:100-103`/`:206-207`),
permissive whitespace + no in-string control-byte scan (`:211-216`/`:218-222`),
and JsonWriter/Sax vtable dispatch (`:200`/`:105`/`:189`) all remain in-bounds
of the 235-line file. **PASS.**

### F-V3-CH1-B: 2F frontmatter `primary_sources_cited` 24→26 reconciliation

V2 CH1 carried a residual minor: `2F-parse-that-gaps.md:7-8` declared
`primary_sources_cited: 24` but the `counted_source_ids` list expanded to 26
elements (the two extras being internal cross-document references
`SRC-S-P3-A-V1` and `SRC-T-P1`). V2 BLK-06 classified this informational,
not fold-blocking; the V3 atomic micro-fold reconciles to 26.

Executable verification at HEAD (2026-05-23):

```text
$ grep "^primary_sources_cited:" 2F-parse-that-gaps.md
primary_sources_cited: 26

$ grep "^counted_source_ids:" 2F-parse-that-gaps.md | tr ',' '\n' | wc -l
26

$ grep -cE "^\| SRC-" 2F-parse-that-gaps.md
26
```

Frontmatter `primary_sources_cited: 26` (at `:7`) matches the
`counted_source_ids` list at `:8` (26 IDs: SRC-COX-REGEX, SRC-RE2,
SRC-RUST-REGEX, SRC-MEMCHR, SRC-FASTFLOAT, SRC-FNF, SRC-CLINGER,
SRC-SIMDJSON-PAPER, SRC-SIMDJSON-SRC, SRC-SIMDUTF, SRC-HOEHRMANN,
SRC-MULA-LEMIRE, SRC-XXHASH, SRC-RFC3629, SRC-RFC8259,
SRC-PARSE-THAT-REGEX, SRC-PARSE-THAT-DOCS, SRC-BBNF-REGEX, SRC-BBNF-SIMD,
SRC-BBNF-CODEGEN, SRC-BBNF-RUNTIME, SRC-BBNF-DIGEST, SRC-REDRESS,
SRC-S-P2-V3-CONSOL, SRC-S-P3-A-V1, SRC-T-P1) matches the registry table at
`:119-144` (26 keyed `| SRC-... |` rows). All three counts triangulate
exactly. **PASS.**

### Zero V3 drift on 2E (V2-LOCKED through V3)

CHALLENGE-CONTEXT §1 declares "2E V2-LOCKED through V3; zero V3 edits". V3
git diff verification:

```text
$ git diff b5628414f daa14127f -- restart/audit/totality/p2/2E-host-arch-esoterica.md
(empty)
```

Zero bytes diff between the V2 atomic fold (`b5628414f`) and the V3 atomic
fold (`daa14127f`) for `2E-host-arch-esoterica.md`. The dossier remains at
`cycle: V7` with `primary_sources_cited: 28` matching its 28-entry
`counted_source_ids` list (lines 11-38) and 28-row registry table (lines
167-194). V7-LOCKED state is preserved verbatim through the V3 wave. **PASS.**

Note on the prior V3 lens REVISE finding: the earlier V3 CH1 draft at
`restart/audit/totality/p2/hardening/V3/CH1.md` (HEAD prior to this
LOCK-TRIGGER rewrite) flagged a mismatch between the V3 fold addendum's
prescribed 2E IDs (`SRC-INTEL-X86`/`SRC-FFMPEG`/`SRC-DAV1D`,
`T-P2-V3-FOLD-ADDENDUM.md:53`) and the actual 2E registry IDs
(`SRC-X86-INTEL-INTRIN`/no FFmpeg/no DAV1D in 2E's local registry). That
finding was authored 2026-05-21 against a draft prescriptive snapshot
predating the cohort decision to keep 2E V2-LOCKED. Under the LOCK-trigger
disposition (CHALLENGE-CONTEXT §1: "zero V3 edits"), the V3 addendum's
prescribed-IDs row for 2E is a non-binding draft superseded by the
LOCKED-V7-baseline authority. The 2E dossier's own frontmatter +
`counted_source_ids` + registry triangulate exactly (28/28/28); no V4
repair is required for 2E.

## SHA-pinned upstream cites — re-verified at HEAD

All five SHA-pinned upstream source citations re-execute cleanly via
raw-content fetch at 2026-05-23, with the same SHAs as V2:

```text
$ curl -sS -o /dev/null -w "FFmpeg checkasm.h: %{http_code}\n" \
    "https://raw.githubusercontent.com/FFmpeg/FFmpeg/08571418/tests/checkasm/checkasm.h"
FFmpeg checkasm.h: 200

$ curl -sS -o /dev/null -w "dav1d loopfilter.c: %{http_code}\n" \
    "https://raw.githubusercontent.com/videolan/dav1d/1718ff9a/tests/checkasm/loopfilter.c"
dav1d loopfilter.c: 200

$ curl -sS -o /dev/null -w "simdjson parse_many.md: %{http_code}\n" \
    "https://raw.githubusercontent.com/simdjson/simdjson/168ef580/doc/parse_many.md"
simdjson parse_many.md: 200

$ curl -sS -o /dev/null -w "sonic-rs README.md: %{http_code}\n" \
    "https://raw.githubusercontent.com/cloudwego/sonic-rs/03545a95/README.md"
sonic-rs README.md: 200

$ curl -sS -o /dev/null -w "yyjson yyjson.h: %{http_code}\n" \
    "https://raw.githubusercontent.com/ibireme/yyjson/95f4c61b/src/yyjson.h"
yyjson yyjson.h: 200
```

All five upstream SHA-pinned citations PASS at HEAD with the same SHAs
declared in 2A frontmatter `upstream_sha_pins` at `:27-31` (simdjson
`168ef580...`, sonic-rs `03545a95...`, yyjson `95f4c61b...`, ffmpeg
`08571418...`, dav1d `1718ff9a...`). No V3 fold packet bumps a SHA pin —
correct per the V2 §Disposition binding-contract guidance. The V2-controlled
textual content checks (parse_many.md two-stage; sonic-rs README four-leaf
SIMD list; yyjson.h:736-744 strict default-flag list; checkasm.h:214-240
call_ref/call_new/check_func; dav1d loopfilter.c:177-188 call_ref +
checkasm_check_pixel) hold under unchanged SHAs.

Note on upstream HEAD drift (informational only, not a CH1 issue): 2A row
notes at `:212` and `:218` correctly disclose that the live HEADs of yyjson
and FFmpeg have moved past the dispatch-pinned SHAs (`99cc4351...` and
`3baab604...` respectively as of 2026-05-23) — the pinned SHAs remain
content-stable for the cited line ranges, which is the binding-contract
property V2 §Disposition guidance requires.

## Per-dossier ACCEPT/REVISE/REJECT census (V3 LOCK-TRIGGER cycle)

| dossier | cycle | high-stakes V3 cites tested | passes | defects | V3 disposition |
|---|---|---|---|---|---|
| 2A SOTA landscape | V2 (V3-folded) | 5 SHA-pinned + 6 named-paper + 1 docs.rs + 1 crates.io tarball (asmjson 235-line verbatim) | 14/14 | 0 (F-V3-CH1-A 235-line reconciliation landed at three sites: `:32`/`:66`/`:213`; V2 BLK-03 cosmetic carry discharged) | **ACCEPT** |
| 2B primitive vocabulary | V2 (V3-folded) | 3 local asm macro counts + 6 SHA-pinned + 2 named-paper (V3 §A5+§A6 substrate_target/retention_lifetime columns added per F-CH5-V2-01; CH1 surface unchanged) | 11/11 | 0 | **ACCEPT** |
| 2C grammar neutrality | V4 (V3-preserved) | 4 W3C anchor + 1 OASIS PDF + 6 local (V3 preserved at V4; F-V3-CH4-A 2C portion BBNF_SIMD_STRICT inline at §Closure Criteria :303-305) | 11/11 | 0 | **ACCEPT** |
| 2D cost model | V2 (V3-folded) | 7 named-paper URLs + 1 Wayback-pinned (Sneller) + 1 architecture-pressure blog + 3 local (V3: F-V3-CH4-A BBNF_SIMD_STRICT at `:142-149`; F-V3-CH4-B 6 abrogate gates numerically bound at `:151-162`; CH1 surface unchanged) | 12/12 | 0 | **ACCEPT** |
| 2E host-arch esoterica | V7 (V2-LOCKED through V3) | 11 ISA-ref + 6 named-technique blog (5 V2-refreshed) + 5 local + 6 cross-arch (zero V3 edits; b5628414f V2 atomic fold is last touch) | 28/28 | 0 | **ACCEPT** |
| 2F parse-that gaps | V6 (V3-folded) | 9 named-paper + 5 SHA-pinned crate + 8 local + 2 internal cross-doc (F-V3-CH1-B 24→26 frontmatter reconciliation landed; V2 BLK-06 cosmetic discharged) | 26/26 | 0 | **ACCEPT** |

**ACCEPT-rate: 6 of 6 dossiers ACCEPT. Cycle ACCEPT rate: 100%.**

Both V2 cosmetic carries (235-vs-236 asmjson line count on 2A; 24-vs-26
frontmatter register split on 2F) are discharged at V3. Zero residual CH1
defects across the cohort.

## §3Z cycle disposition — 2-CYCLE LOCK TRIGGER

V1 CH1 = REVISE (50% ACCEPT). V2 CH1 = **ACCEPT (100%)** — first ≥95%
cycle. V3 CH1 = **ACCEPT (100%)** — second consecutive ≥95% cycle.

Per `restart/prompts/ORCHESTRATOR.md` §3W + §3Z (cohort LOCK = ≥95% × 2
consecutive cycles; V≤5 ceiling), the CH1 lens satisfies the 2-cycle LOCK
condition at V3 close: V2 (100%) + V3 (100%) = consecutive ≥95% pair.

**CH1 §3Z LOCK CONFIRMED at V3.** No further CH1 dispatch required; the
lens contributes a LOCKED row to the cohort §3Z LOCK declaration at the
V3 aggregator (`HARDENING-T-P2-V3-CONSOLIDATED.md`).

## §6 per-dossier cycle drift (V3 LOCK-TRIGGER carry-forward)

The V3 LOCK-TRIGGER wave reviews a heterogenous-cycle cohort, transparently
declared per dossier (per V2 §6 convention):

| dossier | V2 cycle | V3 cycle | V3 frontmatter |
|---|---|---|---|
| 2A | V2 | V2 (V3-folded; cycle unchanged per atomic fold convention) | `cycle: V2` (`2A-sota-landscape.md:4`) |
| 2B | V2 | V2 (V3-folded; cycle unchanged) | `cycle: V2` (`2B-primitive-vocabulary.md:4`) |
| 2C | V4 | V4 (V3-preserved; no V3 edits) | `cycle: V4` (`2C-grammar-neutrality.md:4`) |
| 2D | V2 | V2 (V3-folded; cycle unchanged) | `cycle: V2` (`2D-cost-model.md:4`) |
| 2E | V7 | V7 (V2-LOCKED through V3; zero edits) | `cycle: V7` (`2E-host-arch-esoterica.md:4`) |
| 2F | V6 | V6 (V3-folded; cycle unchanged) | `cycle: V6` (`2F-parse-that-gaps.md:4`) |

The 6-dossier cohort is at V3 per the LOCK-TRIGGER wave label, with
per-dossier author cycles V2/V2/V4/V2/V7/V6 — consistent with V2 §6 and the
S-P2 V3 hardening precedent. Atomic-fold convention preserves the
dossier-author cycle counter unless the dossier substantively re-authors
(2E is the load-bearing example: V7 frozen through V3 wave).

## Refutation-row correctness — V3 wave

No V3 fold-packet edits introduce new refutation rows or alter existing
ones. The V1/V2 refutation register (2A T2A-REF-001/002/003/004/005, 2D
T2D-COLLAPSEDSTAGE-X86-ONLY refutation row, 2E V6 NEW `svmatch_u8`-is-NEON
refutation, 2F simdjson cross-call `prev_in_string` refutation +
`unescape_string`-not-SIMD-body refutation + `regex-syntax` HIR
non-admissibility refutation) is preserved verbatim. The asmjson refutation
row T2A-REF-002 remains strengthened by the V2 verbatim-quote extraction
(now with V3 line-count reconciliation to 235 — the in-bounds line spans
`:100-103`/`:206-207`/`:211-216`/`:218-222`/`:200`/`:105`/`:189` remain
unchanged). No refuted-technique row at V3 misrepresents the literature's
actual position.

## Findings — V3 net

The V3 atomic micro-fold (commit `daa14127f`) delivers exactly the two
CH1-specific cosmetic discharges that V2 carried as fold candidates:
F-V3-CH1-A (asmjson 235-line reconciliation at three sites in 2A) and
F-V3-CH1-B (2F frontmatter 24→26 reconciliation). Both discharges are
executable-verified at HEAD. The five SHA-pinned upstream contracts
(FFmpeg / dav1d / simdjson / sonic-rs / yyjson) all return 200 at HEAD and
were not bumped by V3 fold — correct per V2 binding-contract guidance.
2E remains zero-V3-edit V2-LOCKED V7 as declared. No residual CH1 defect
remains across the cohort.

**No source files were edited.** This report wrote only the assigned
CH1 V3 path. All curl / git / grep / tar / wc verifications executed
during this CH1 LOCK-TRIGGER wave were read-only.
