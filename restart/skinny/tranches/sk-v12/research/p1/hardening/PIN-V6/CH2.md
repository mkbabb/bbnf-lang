# SK-V12 S-P1 PIN-V6 CH2 - Generality / Lock 14

Verdict: ACCEPT
Score: 98%

## Blocking Findings

None.

Review base was `HEAD f3e68a43bb5c7765457c48907a6f0853d1f71bc5`. CH2's contract is Lock 14 grammar generality: no grammar-name leak, and no JSON-only evidence may stand in for CSS L4, Sheets, or BBNF-self proof (`restart/prompts/ORCHESTRATOR.md:81-88`; `restart/prompts/skinny/PASS-1-PROFILE.md:129-135`). S-P1 still advances only under the two-clean-cycle rule in `ORCHESTRATOR.md` Section 3Z (`restart/prompts/ORCHESTRATOR.md:104-123`; `restart/prompts/skinny/PASS-1-PROFILE.md:166-183`).

The current fold does not promote JSON-only evidence into CSS L4 admission. The pin capture manifest says `skinny/RESULTS.md` remains row authority, the tracked pin replay ledger has 458 command rows, the pre-pin replay surface is historical only, and CSS L4 is unprofiled because there is no generated CSS L4 Track 1 runtime, same-plane lightningcss comparator, or strict equality oracle (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:28-29`, `:51-72`, `:171-177`). The P1 agents preserve that boundary: P1-A rejects root CSS snippets/report fixtures/lightningcss-only runs as substitutes (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:145-158`, `:181-183`); P1-B keeps JSON product rows as guard/diagnostic evidence (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:46-50`, `:187-195`); P1-D says PMU values move no row and create no CSS row (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:67-80`, `:258-261`); P1-E records CSS L4 as a hard bring-up prerequisite, not fallback authorization (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:47-50`, `:113-160`, `:175-200`); and P1-F extracts only the JSON result surface while stating that JSON sonic/serde rows cannot fill the CSS L4 lightningcss close bar (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:24-33`, `:80-93`, `:207-210`).

CSS L4 absence remains a prerequisite, not a substitute. The user pin makes generated CSS L4 authoritative, requires `generated_track1_mbps > lightningcss_mbps + 1`, and keeps Sheets/BBNF-self fallback-only after a CSS redress attempt fails (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`, `:80-103`). The handoff repeats the live state: CSS L4 has no admitted row, Sheets/BBNF-self have no admitted row and are fallback-only, ADMIT requires generated CSS L4 plus equality/lightningcss evidence, and S-P3 must select CSS first (`restart/skinny/tranches/sk-v12/HANDOFF.md:34-39`, `:53-65`, `:74-85`, `:110-128`). P1-C names the concrete source blockers: no CSS runtime module, codegen still guards non-JSON emission, JSON template policy is embedded, `GrammarConfig` is not landed, and the pin root has no CSS comparator row (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:91-110`, `:116-123`).

Lock-14 and `GrammarConfig` leaks are still bounded and routed. The user pin requires the seven Lock-14 leaks to be resolved by W1's `GrammarConfig` surface before CSS L4 emission is legal (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:97-103`, `:141-150`). The value API audit names the 5 major plus 2 embedded leaks and routes the legal shape through `GrammarConfig`, generated grammar metadata, and parameterized view generation (`restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63-107`, `:160-207`). The handoff requires `GrammarConfig` or equivalent metadata before leaving JSON-only templates and says grammar-name branches in generic crates fail closed (`restart/skinny/tranches/sk-v12/HANDOFF.md:119-123`, `:153-155`).

Generated-size and O(N) routing is present. S-P3 must record generated CSS runtime size before redress, including generated LOC, module byte size, regen/check command, and an O(N) grammar-size guard; overflow blocks W1b until traced (`restart/skinny/tranches/sk-v12/HANDOFF.md:121-123`). The CSS L4 gate or companion report must consume generated LOC, generated module byte size, O(N) grammar-size status, Lock 14 status, Lock 16 status, same-wave consumer class, JSON guard state, gate status, wave id, and REDRESS id (`restart/skinny/tranches/sk-v12/HANDOFF.md:144-151`).

The stale pre-pin SPEC clauses are fenced as context until pin-aware S-P3. `SPEC.md` now declares itself pre-pin implementation context until the pin-aware S-P1 -> S-P2 -> S-P3 sequence rewrites it, and its authority list points at `skv12-p1-pin-replay.tsv` rather than the pre-pin replay TSV (`restart/skinny/tranches/sk-v12/SPEC.md:5-23`). The old W1 language that allowed fallback after CSS preflight failure remains visible in the pre-pin SPEC (`restart/skinny/tranches/sk-v12/SPEC.md:183-189`, `:400-408`), but it is superseded by the user pin and handoff refusal rules (`restart/skinny/tranches/sk-v12/HANDOFF.md:5-10`, `:22-23`, `:157-170`, `:172-177`). The S-P1 hardening status also avoids a paper close: it says pre-pin convergence is historical only, names the pin replay/self-time authorities, and requires two new consecutive all-ACCEPT pin cycles before S-P2 dispatch (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:5-25`, `:27-48`, `:50-82`). PIN-V5 was five ACCEPT and one REVISE, with the fold applying the authority cleanup and routing to a new challenge cycle (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md:10-20`, `:22-43`).

## Nonblocking Notes

- ACCEPT here means CH2 finds the current S-P1 fold grammar-safe. It does not mean CSS L4 has admitted; CSS L4 remains absent and routed to S-P2/S-P3/W1.
- The file name `HARDENING-S-P1-CONVERGED.md` is still historically confusing, but its contents now correctly say pin convergence is in progress, not closed (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:1-6`, `:78-82`).
- Commands run:

```bash
git status --short --branch
# ## master...origin/master [ahead 2387]

git rev-parse HEAD
# f3e68a43bb5c7765457c48907a6f0853d1f71bc5

rg --files restart/skinny/tranches/sk-v12 restart/prompts SPEC.md | sort
# SPEC.md at repo root was absent; tranche SPEC is restart/skinny/tranches/sk-v12/SPEC.md.

wc -l restart/prompts/ORCHESTRATOR.md restart/prompts/skinny/PASS-1-PROFILE.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/SPEC.md restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/CONSOLIDATED.md
# Read-set sizing confirmed: 2862 total lines across the minimum files.

awk -F '\t' 'NR>1{total++; if(tolower($2 FS $3 FS $4 FS $5) ~ /(css|lightningcss|sheets|bbnf)/) hits++; if($3=="parse") parse++; if($3=="direct") direct++; if($3=="typed") typed++} END{print "total=" total " semantic_nonjson_hits=" hits+0 " parse=" parse+0 " direct=" direct+0 " typed=" typed+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# total=458 semantic_nonjson_hits=0 parse=170 direct=204 typed=84

awk -F '\t' 'NR>1 && ($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ || $4=="update-center" || $3=="css" || $3=="sheets" || $3=="bbnf-self") {print NR ":" $0; bad++} END{print "bad",bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# bad 0

rg -n "css_l4|lightningcss|CSS L4" skinny/RESULTS.md restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# no output; exit 1

find skinny/crates/runtime/src/grammars -maxdepth 2 -type d | sort
# skinny/crates/runtime/src/grammars
# skinny/crates/runtime/src/grammars/json
# skinny/crates/runtime/src/grammars/sheets_witness

rg -n "skv12-p1-replay.tsv|skv12-p1-pin-replay.tsv|pre-pin" restart/skinny/tranches/sk-v12/SPEC.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md
# Confirmed SPEC/status/manifest route pin replay authority and demote pre-pin replay/context.
```

## Exact Fold Edits If REVISE

Not applicable. Verdict is ACCEPT; no CH2 fold edits are required.
