# T-P2 V1 CH1 Correctness

Pass: T-P2 Research.
Cycle: V1.
Lens: CH1 correctness / provenance.
Owner: `restart/audit/totality/p2/hardening/V1/CH1.md`.

## Verdict: REVISE

V1 is directionally useful, and most high-load-bearing external sources exist
and support their broad claims, but the packet is not provenance-clean enough
to advance. CH1 found nontrivial reproducibility defects: wrong line citations
against W3C specifications, one dead source repository URL, moving-branch
GitHub anchors where CH1 needs stable source identity, and an unpinned external
local worktree used as primary evidence. These are revise-class defects because
the underlying architecture claims are mostly supportable, but the citations
must be repaired before T-P2 can converge.

## Evidence

Audited scope: `2A-sota-landscape.md`, `2B-primitive-vocabulary.md`,
`2C-grammar-neutrality.md`, `2D-cost-model.md`,
`2E-host-arch-esoterica.md`, and `2F-parse-that-gaps.md`, against
`restart/prompts/totality/PASS-2-RESEARCH.md`.

- Positive controls passed. Raw upstream fetches confirm the key 2A claims for
  simdjson stage 1/stage 2 (`doc/parse_many.md:54-57`), On-Demand skip/use
  semantics (`doc/basics.md:343-350`, `doc/ondemand_design.md:71-89`),
  sonic-rs targeted SIMD and direct struct claims (`README.md:60-90`), yyjson
  strict/default flags (`src/yyjson.h:736-837`), FFmpeg checkasm
  call/ref/bench process (`checkasm.h:214-240`, `:396-430`;
  `checkasm.c:679-737`), dav1d call-ref/call-new/bench evidence
  (`loopfilter.c:177-188`), and Arm ACLE/Neon feature/intrinsic names.
- Local cited source paths largely resolve. Notable verified anchors include
  `skinny/REDRESS.md` rows for REDRESS 88/89/96/97/98/119/122/126,
  `skinny/RESULTS.md`, `crates/egraph/src/{language,extract,csp_scheduler}.rs`,
  `skinny/crates/*`, and the external parse-that worktree paths.
- Benchmark traceability is partly sound. The 4.718x ASCII run-skip claim in
  2B/2E traces to REDRESS-126, including fixture hash, sample count, platform
  command, scalar/candidate ns/iter, and routed non-admission disposition.
  The REDRESS-119 direct residual table is also traceable by row/platform.

## Blockers / Fold Requirements

1. **2C W3C line citations do not support the claimed findings.** Current
   fetches of `https://www.w3.org/TR/css-syntax-3/` at lines 269-278 and
   316-348 show document metadata / process / table-of-contents content, not
   token-stream or token-diagram definitions. Current fetches of
   `https://www.w3.org/TR/selectors-4/` at lines 237-297 show CSS styling, not
   selector families; lines 2282-2291 do discuss selector lists but do not cover
   the full cited family claim. Current fetches of
   `https://www.w3.org/TR/css-values-4/` at lines 1008-1048 show table of
   contents entries, not `calc()` / `min()` / `max()` / `clamp()` behavior, and
   `https://www.w3.org/TR/css-variables-1/` at lines 94-138 shows stylesheet
   boilerplate. Fold V2 to section anchors or stable snapshots that actually
   support each CSS claim; do not cite rendered TR line numbers.
2. **2C OpenFormula PDF line references are not reproducible.** The source
   exists, but "lines 1567-1585" / "1604-1633" are not verifiable against a PDF
   URL without a named text-extraction artifact, page/section, or quote target.
   Fold to ODF 1.3 Part 4 section/page references, or check in a generated
   text-extract artifact with checksum.
3. **2D cites a dead source repository.** `https://github.com/SnellerInc/sneller`
   returned HTTP 404. The Sneller blog post exists and supports general
   branchless AVX-512/K-mask discussion, but the dossier's "plus source
   repository" claim is not backed. Remove the repository citation or replace
   it with a live archived/source reference before using Sneller as primary
   source evidence.
4. **Moving-branch web source anchors are not stable enough for convergence.**
   2A/2B/2E cite GitHub `master`/`main` line anchors for simdjson, sonic-rs,
   yyjson, FFmpeg, and dav1d. They resolve today, but CH1 needs a stable
   provenance surface for T-P3 amendments. Fold V2 to pinned commit URLs,
   release tags, crate tarball checksums, or an explicit "verified on
   YYYY-MM-DD at upstream HEAD <sha>" register.
5. **2F relies on an external local worktree without revision provenance.**
   `/Users/mkbabb/Programming/parse-that` resolves and HEAD is
   `051a6d681da95a180e6b67f956526722d1d33322`, but the worktree has untracked
   files. V2 must record the HEAD and dirty/untracked state, or vendor/import a
   snapshot, before using those paths as primary evidence for `bbnf-regex`.
6. **Source-count frontmatter is not auditable.** Several dossiers report
   `primary_sources_cited` counts that do not match their source registers if
   external papers, named posts, library source, and local primary evidence are
   counted consistently. Fold V2 to a single counting convention and include a
   source register ID for every counted item.
7. **External benchmark claims need corpus/platform rows.** The local REDRESS
   numbers are traceable, but external performance claims such as asmjson /
   Sneller architecture pressure should name corpus, hardware, command or
   vendor-provided benchmark context, and strictness/conformance status. If the
   source lacks those details, classify it as architecture pressure only.

## Disposition

REVISE V1. Required V2 fold:

- Replace brittle W3C/PDF line citations with stable section/page/source
  anchors that support the exact CSS/OpenFormula claims.
- Remove or replace the dead Sneller repository citation.
- Pin moving upstream source references or record verified upstream SHAs.
- Record parse-that external worktree HEAD/dirty state or vendor a snapshot.
- Normalize source-count frontmatter and external benchmark provenance.

No source files were edited. This report wrote only the assigned CH1 path.
