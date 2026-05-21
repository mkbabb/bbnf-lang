# T-P2 V3 CH1 Correctness / Provenance

## Verdict

REVISE.

V3 fixes the V2 generic-source pin defects and makes source-count arithmetic
reproducible through `counted_source_ids`. The remaining provenance defect is
2E-local: its frontmatter counted IDs do not exactly match the IDs in its own
source registry, so `primary_sources_cited: 11` is not fully reproducible from
the local registry rows.

## Evidence

1. V2 required the exact CH1 repair set under review here: pin or source-date
   external library rows still expressed as generic repository roots, and make
   `primary_sources_cited` auditable through source-register counts or explicit
   `counted_source_ids`
   (`restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md:24`,
   `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md:35`,
   `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md:40`).

2. V3 adds binding source authority for the formerly moving upstreams:
   simdjson, RE2, Rust regex, fast_float, memchr, and xxHash now have SHAs or
   2026-05-21 source-date verification
   (`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:24`-`35`).

3. The dossiers carry those pins where the V2 defects were named. 2D binds
   simdjson to `168ef580757d75270475b379e83c2b39787a6765`
   (`restart/audit/totality/p2/2D-cost-model.md:63`,
   `restart/audit/totality/p2/2D-cost-model.md:206`). 2F binds RE2, Rust
   regex, memchr, fast_float, simdjson, and xxHash to explicit HEAD SHAs
   (`restart/audit/totality/p2/2F-parse-that-gaps.md:71`-`80`). 2A's JSON and
   checkasm upstream rows are pinned to SHAs and verified dates
   (`restart/audit/totality/p2/2A-sota-landscape.md:156`-`199`).

4. Count arithmetic is present in all six dossiers and matches the V3 addendum:
   2A has 15 IDs, 2B has 24, 2C has 7, 2D has 11, 2E has 11, and 2F has 21
   (`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:47`-`54`;
   `restart/audit/totality/p2/2A-sota-landscape.md:7`-`8`;
   `restart/audit/totality/p2/2B-primitive-vocabulary.md:7`-`8`;
   `restart/audit/totality/p2/2C-grammar-neutrality.md:7`-`8`;
   `restart/audit/totality/p2/2D-cost-model.md:10`-`11`;
   `restart/audit/totality/p2/2E-host-arch-esoterica.md:7`-`8`;
   `restart/audit/totality/p2/2F-parse-that-gaps.md:7`-`8`).

5. Where dossiers have keyed source registries, 2D and 2F match exactly:
   their counted IDs are the same IDs defined in their registry tables
   (`restart/audit/totality/p2/2D-cost-model.md:194`-`208`,
   `restart/audit/totality/p2/2F-parse-that-gaps.md:66`-`90`). 2A, 2B, and 2C
   have unkeyed source sections, so the exact ID-to-registry comparison does
   not apply locally; their count authority is the frontmatter plus the V3
   addendum list (`restart/audit/totality/p2/2A-sota-landscape.md:151`-`201`,
   `restart/audit/totality/p2/2B-primitive-vocabulary.md:354`-`369`,
   `restart/audit/totality/p2/2C-grammar-neutrality.md:190`-`198`).

6. Residual defect: 2E frontmatter counts `SRC-INTEL-X86`, `SRC-FFMPEG`, and
   `SRC-DAV1D`, but its source registry defines `SRC-X86-INTEL` instead and has
   no `SRC-FFMPEG` or `SRC-DAV1D` registry rows
   (`restart/audit/totality/p2/2E-host-arch-esoterica.md:7`-`8`,
   `restart/audit/totality/p2/2E-host-arch-esoterica.md:43`-`57`). FFmpeg and
   dav1d are only present in the inherited V2 provenance table, not as the
   counted IDs named in frontmatter
   (`restart/audit/totality/p2/2E-host-arch-esoterica.md:59`-`73`).

## Required V4 Repairs

1. Repair 2E's counted-ID mapping. Either change the frontmatter and V3
   counted-source convention from `SRC-INTEL-X86`, `SRC-FFMPEG`, and
   `SRC-DAV1D` to IDs that actually exist in the 2E registry, or add registry
   rows with those exact IDs.

2. Decide whether 2E's current uncounted registry rows `SRC-REDRESS` and
   `SRC-BBNF-A64` are counted primary evidence or support-only evidence. If
   counted, include them in `counted_source_ids` and adjust the count; if not,
   mark them support-only so the local registry cannot accidentally satisfy the
   source-count claim by substitution.

3. Preserve the V3 pin repairs. No V4 source-pin repair is required for the V2
   generic GitHub-root set unless a dossier introduces a newer moving source
   without a SHA or source-date.
