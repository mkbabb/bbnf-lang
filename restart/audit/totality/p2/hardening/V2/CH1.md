# T-P2 V2 CH1 Correctness

## Verdict

REVISE.

V2 resolves the substantive V1 CH1 provenance defects: the shared V2 addendum
pins the originally moving upstreams, replaces brittle W3C/OASIS line citations
with stable anchors or section/page references, removes the dead Sneller
repository as source authority, and records parse-that HEAD plus dirty state.
The remaining defects are revise-class, not reject-class: some V2 source rows
still cite unpinned generic repository roots, and the `primary_sources_cited`
counts are not consistently auditable under the new convention.

## Findings

1. Provenance repair is mostly effective. `T-P2-V2-FOLD-ADDENDUM.md`
   section "V2 Provenance Register" records simdjson, sonic-rs, yyjson,
   FFmpeg, dav1d, egg, OR-Tools, RE2, Rust regex, fast_float, Sneller, and
   the parse-that sibling worktree. The six dossiers all cite the addendum in
   frontmatter or their V2 fold-authority sections.

2. W3C/OASIS/OpenFormula citation repair is acceptable. `T-P2-V2-FOLD-ADDENDUM.md`
   section "V2 Provenance Register" bans rendered-line citations and gives stable
   CSS anchors plus OpenFormula page/section policy. `2C-grammar-neutrality.md`
   section "Technique Grounding Table" now cites CSS Syntax `#tokenization` /
   `#typedef-token-stream`, Selectors `#overview` / `#grammar`, CSS Values
   `#calc-notation`, CSS Variables
   `#defining-variables` / `#using-variables`, and ODF 1.3 Part 4 Formula
   sections/pages for Sheets.

3. Dead Sneller repository authority is repaired. `T-P2-V2-FOLD-ADDENDUM.md`
   section "V2 Provenance Register" marks the former source-repository
   citation removed and limits Sneller to architecture pressure.
   `2D-cost-model.md` repeats that downgrade in "Executive Summary",
   "Technique Grounding Table", and "Source Register";
   `2E-host-arch-esoterica.md` section "V2 Shared Provenance Register" also
   inherits the downgrade.

4. The originally flagged moving-source set is repaired. `2A-sota-landscape.md`
   section "Source Register" uses commit-pinned GitHub URLs for simdjson, sonic-rs,
   yyjson, FFmpeg, and dav1d, and the addendum records the verified
   2026-05-21 HEADs. This resolves the V1 moving-branch blocker for those
   libraries.

5. Residual CH1 defect: some V2 source rows still use unpinned generic upstream
   repository roots. `2D-cost-model.md` section "Source Register" lists simdjson as
   `https://github.com/simdjson/simdjson` rather than the pinned V2 SHA in its
   source register. `2F-parse-that-gaps.md` section "Source Registry" lists RE2,
   Rust regex, memchr, fast_float, simdjson source, and xxHash as generic
   GitHub roots; RE2, Rust regex, fast_float, and simdjson have addendum SHAs
   but the 2F source rows do not bind to them, while memchr and xxHash have no
   SHA/source-date authority in the addendum.

6. Parse-that local authority is repaired and correctly downgraded.
   `T-P2-V2-FOLD-ADDENDUM.md` section "V2 Provenance Register" records
   `/Users/mkbabb/Programming/parse-that` at HEAD
   `051a6d681da95a180e6b67f956526722d1d33322` with dirty/untracked paths, and
   `2F-parse-that-gaps.md` sections "Executive Summary" and
   "V2 Parse-That Import Boundary" treat it as conditional import authority only.
   Local verification matched the recorded HEAD and dirty/untracked shape.

7. Residual CH1 defect: the source-count convention is still not consistently
   auditable. `T-P2-V2-FOLD-ADDENDUM.md` section "V2 Provenance Register" defines
   the count convention, and `2D-cost-model.md` section "Source Register" is clean
   (`primary_sources_cited: 11` with 11 source-register rows). But
   `2B-primitive-vocabulary.md` section "Sources" reports 24 while listing
   5 external bullets and many local primary bullets without counted IDs;
   `2C-grammar-neutrality.md` section "Sources" reports 15 while its source
   section has 7 broad bullets; `2E-host-arch-esoterica.md` section
   "Source Registry" reports 18 with 11 registry rows plus an inherited
   register; and `2F-parse-that-gaps.md` section "Source Registry" reports 20
   while its registry has 21 `SRC-*` rows.
   This is not a correctness rejection, but it fails the V1 CH1 source-count
   redress requirement.

## Required Redress If Any

1. Pin or source-date every external library-source row still expressed as a
   generic repository root. At minimum, update the 2D simdjson row to cite the
   V2 SHA, update 2F's RE2/Rust regex/fast_float/simdjson rows to bind to the
   addendum SHAs, and either add memchr/xxHash SHA/source-date authority or
   downgrade those rows to non-counted background.

2. Normalize `primary_sources_cited` across 2A-2F. Either make each count equal
   the number of enumerated source-register IDs, or add an explicit
   `counted_source_ids` list so CH1 can reproduce the number without guessing
   how broad bullets and inherited addendum entries are grouped.

## Evidence Checked

- `restart/prompts/totality/PASS-2-RESEARCH.md` CH1 requirements and output
  schema.
- `restart/audit/totality/p2/hardening/V1/CH1.md` V1 revise set.
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`
  consolidated V1 fold actions.
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md` provenance register,
  stable citation replacements, source-count convention, parse-that authority,
  and V2 dossier fold requirements.
- `restart/audit/totality/p2/2A-sota-landscape.md` V2 fold authority and pinned
  JSON/checkasm source register.
- `restart/audit/totality/p2/2B-primitive-vocabulary.md` V2 fold authority,
  admission ledger, and source section.
- `restart/audit/totality/p2/2C-grammar-neutrality.md` stable W3C/OASIS rows,
  Lock 14 contract, and source section.
- `restart/audit/totality/p2/2D-cost-model.md` Sneller downgrade, addendum
  inheritance, source register, and unpinned simdjson row.
- `restart/audit/totality/p2/2E-host-arch-esoterica.md` shared provenance
  inheritance and source registry.
- `restart/audit/totality/p2/2F-parse-that-gaps.md` parse-that conditional
  authority, import boundary, source registry, and unpinned GitHub roots.
- Local parse-that check: `/Users/mkbabb/Programming/parse-that` HEAD
  `051a6d681da95a180e6b67f956526722d1d33322`; untracked `.gitmodules`,
  `docs/instructions/README.md`, `docs/precepts/`, `rust/.cargo/config.toml`,
  and `rust/parse_that/rustc-ice-2026-04-22T15_05_48-88533.txt`.
