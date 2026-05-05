# Wave 4.1 PASS-3 narrow-amendment classification

V3 hardening returned AMENDMENT-REQUIRED on PASS-3 with two binding + two non-blocking surgical edits per `restart/audit/hardening/HARDENING-PASS-3-V3.md` §12. Each item is classified DONE / PARTIAL / ABSENT against the current PASS-3.md surface.

| Item | Site | rg / line verification | State | Surgery scope |
|---|---|---|---|---|
| P3-1 | `restart/audit/pass-3-runtime/PASS-3.md:386-396` | "Exact PASS-3 benchmark rows" table at lines 386-396 carries `Row | Target | Surface under test`; no Competitor floor or Platform columns; only the §10 carry at line 468 names per-row competitor + platform inlining | ABSENT | Extend the bench-row table with Competitor floor + Platform columns sourced from `restart/README.md:328-334`; non-throughput rows carry `(no Lock-8 claim)`. Per Lock 8's per-row attribution standard. |
| P3-2 | `restart/audit/pass-3-runtime/PASS-3.md:399-409` | "Generated API budget" table at lines 399-409 carries `Surface | Budget gate`; visitor + tape projection rows reference "+2 percent ceiling per regen" without the W3 baseline anchor; PASS-2 §6 generated_loc table is the canonical anchor source | ABSENT | Extend the budget table with a "W3 baseline LOC" column populated from PASS-2.md §6 (lines 380-392): css_l4 ≈ 107 K, bbnf ≈ 21 K, json ≈ 14 K. The +2% delta is enforceable only against an anchored value. |
| P3-3 | `restart/audit/pass-3-runtime/PASS-3.md:342` | yaml-row host-route cell currently reads "as declared in `[workspace.metadata.bbnf.grammars.yaml]`"; documentary-vs-enforceable ambiguity | PARTIAL | Replace with "decomposed via `host::primitives` + `@host fn` chain in the metadata block per `restart/README.md:155`; no Rust per-grammar code emerges from the onboarding two surfaces". |
| P3-4 | `restart/audit/pass-3-runtime/PASS-3.md:115` (§3 visitor commitments) + §6b ledger | §6b ledger at lines 350-366 contains `BBNF-LIFE*`, `BBNF-LAYOUT*`, `BBNF-OPT*`, `BBNF-GRAMMAR*`, `BBNF-POINTER*` rows but no `BBNF-VISIT*` rows; visitor cookbook routing absent from §3 close | ABSENT | Append cookbook-routing sentence at end of §3 visitor commitments and extend §6b ledger with three `BBNF-VISIT001/002/003` rows per `agent-3-visitor-surface-designer.md:60-68`. |

## Surgery routing

Wave-4.1 narrow-amendment commit lands:

1. §7 — bench-row table extension with Competitor floor + Platform columns (P3-1, binding, Lane 4).
2. §7 — generated API budget table extension with W3 baseline LOC column (P3-2, binding, Lane 6).
3. §6a — yaml-row host-route cell rewrite (P3-3, non-blocking, Lane 3).
4. §3 — visitor cookbook routing sentence + §6b ledger extension with three `BBNF-VISIT*` rows (P3-4, non-blocking, Lane 9).

Acceptance per V3 §12 expected post-conditions: Lane-4 per-row attribution honoured at the present-document level; Lane-6 +2% delta gates against named anchors; yaml-row host-route is enforceable rather than documentary; visitor diagnostic codes index into the §6b ledger.
