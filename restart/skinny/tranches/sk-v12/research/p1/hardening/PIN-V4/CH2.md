# SK-V12 S-P1 PIN-V4 CH2 - Generality / Lock 14

Verdict: ACCEPT
Score: 98%

## Blocking Findings

None.

Review base was `HEAD 1669c551` (`docs(sk-v12-p1-hardening): fold pin replay challenge PIN-V3`). CH2 scope is grammar generality and Lock 14: JSON profile facts may nominate primitive families, but they must not admit CSS L4, Sheets, or BBNF-self, and they must not hide unresolved grammar-config or generated-size prerequisites. This matches the CH2 contract in `restart/prompts/skinny/PASS-1-PROFILE.md:129-135`; the cycle still follows `restart/prompts/ORCHESTRATOR.md:104-121`.

No JSON-only evidence is promoted to CSS L4 admission. P1-A says parse evidence is diagnostic, records no generated CSS L4 runtime, and rejects report fixtures, root CSS snippets, or lightningcss-only runs as substitutes for a skinny generated Track 1 parser (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:142-158`, `:181-183`). P1-B keeps JSON direct/typed rows as guard and diagnostic evidence, splits Track 1 from Track 2/oracle families, and says those rows cannot satisfy the CSS L4 `> lightningcss_mbps + 1` bar (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:39-50`, `:112-113`, `:160-168`, `:187-195`). P1-D says PMU values do not move rows, admit direct/typed rows, or create the missing CSS L4 row (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:67-80`, `:228-231`, `:258-261`). P1-E requires CSS L4 as a first-class row, keeps Track 2/oracle symbols out of generated Track 1 antecedents, and marks CSS L4 as the one legitimately absent lane (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:32-50`, `:143-161`, `:165-201`). P1-F extracts only the 41 JSON rows and explicitly states that no JSON sonic/serde row populates the CSS lightningcss close bar (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:24-33`, `:80-93`, `:196-210`).

CSS L4 missing-baseline treatment remains a prerequisite, not a substitute. The user pin makes CSS L4 authoritative, raises admission to generated CSS L4 Track 1 greater than `lightningcss_mbps + 1`, and keeps `parse_only` diagnostic (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`, `:80-103`). The handoff repeats that generated CSS L4 has no admitted row, Sheets/BBNF-self are fallback-only, and ADMIT requires generated CSS L4 Track 1, independent oracle/Track 2 strict equality, and lightningcss evidence (`restart/skinny/tranches/sk-v12/HANDOFF.md:34-39`, `:53-65`, `:74-85`). The manifest states the result authority remains `skinny/RESULTS.md`, records 458 pin-era replay rows, and says CSS L4 remains unprofiled because no generated CSS L4 Track 1 runtime, lightningcss same-plane comparator, or strict equality oracle exists (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:28-29`, `:70-73`, `:172-177`). P1-C records the concrete blockers: no generated CSS runtime module, codegen still guarded by JSON runtime emission, embedded JSON template policy, missing `GrammarConfig`, and no CSS comparator row under `/tmp/skv12-pin-p1` (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:96-110`, `:116-123`).

Lock-14 and `GrammarConfig` leaks remain bounded and routed. The pin carries Lock 14 grammar-neutrality and requires the seven leaks to be resolved by W1's `GrammarConfig` surface before CSS L4 emission is legal (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:97-103`, `:141-150`). The value API audit still names 5 major plus 2 embedded leaks and routes the minimal legal shape through `GrammarConfig`, per-grammar metadata modules, and parametrized view generation (`restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63-107`, `:160-207`). The handoff requires `GrammarConfig` or equivalent generated metadata before CSS L4 emission can leave JSON-only templates and makes grammar-name branches in generic crates fail closed (`restart/skinny/tranches/sk-v12/HANDOFF.md:119-120`, `:153-155`).

Generated-size and O(N) routing remain bounded. PIN-V1 folded generated CSS runtime size, module byte size, regen/check command, and an O(N) grammar-size guard into the handoff (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V1/CONSOLIDATED.md:30-35`). The current handoff requires generated LOC, module byte size, regen/check command, and an O(N) grammar-size guard before W1b redress, with overflow blocking W1b until traced (`restart/skinny/tranches/sk-v12/HANDOFF.md:121-123`). The CSS L4 gate telemetry must consume generated LOC, generated module byte size, O(N) grammar-size status, Lock 14, Lock 16, JSON guard state, same-wave consumer class, gate status, wave id, and REDRESS id (`restart/skinny/tranches/sk-v12/HANDOFF.md:144-151`).

Sheets and BBNF-self remain fallback-only. The user pin says Sheets and BBNF-self become fallbacks only after a CSS L4 redress attempt fails, not after preflight failure (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-24`). The handoff repeats fallback-only status in the current state, priority list, FIXPOINT requirements, S-P3 requirements, and refusal conditions (`restart/skinny/tranches/sk-v12/HANDOFF.md:37-38`, `:63-65`, `:87-95`, `:126-127`, `:159-163`). The P1 packet preserves that boundary in P1-A, P1-C, P1-E, and P1-F (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:181-183`; `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:89-94`, `:120-121`; `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:47-50`; `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:80-87`).

The prior PIN cycle folds no longer leave a CH2 blocker. PIN-V1 accepted CH2 while CH5 required Track 1/Track 2 and generated-size/O(N) cleanup; the fold records those fixes (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V1/CONSOLIDATED.md:12-17`, `:23-35`). PIN-V2 accepted CH2 and folded replay schema/log authority issues outside CH2 (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V2/CONSOLIDATED.md:12-19`, `:23-35`). PIN-V3 accepted CH2 and folded the last replay-ledger corpus-key defect for PIN-V4 review (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md:12-19`, `:23-40`).

Commands run:

```bash
git status --short
# no output

git rev-parse --short HEAD
# 1669c551

awk -F '\t' 'NR>1 {total++; if($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/) bad_mode++; if($4=="update-center") bad_update++; if($1=="pmu" && $3=="parse" && $4 ~ /update/) print NR ":" $4 ":" $5 ":" $9} END{print "total=" total " bad_mode=" bad_mode+0 " bad_update=" bad_update+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# 66:update_center:track1:...
# 67:update_center:track2:...
# total=458 bad_mode=0 bad_update=0

awk -F '\t' 'NR>1 {total++; if(tolower($2 FS $3 FS $4 FS $5) ~ /(css|lightningcss|sheets|bbnf)/) hits++; if($3=="parse") parse++; if($3=="direct") direct++; if($3=="typed") typed++} END{print "total=" total " semantic_nonjson_hits=" hits+0 " parse=" parse+0 " direct=" direct+0 " typed=" typed+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# total=458 semantic_nonjson_hits=0 parse=170 direct=204 typed=84

find skinny/crates/runtime/src/grammars -maxdepth 3 -type f | sort
# generated JSON files plus sheets_witness only; no css_l4/css_l4_declaration_values module

rg -n "css_l4|lightningcss|CSS L4" skinny/RESULTS.md || true
# no output

awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "xctrace_total=" total " bad=" bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
# xctrace_total=212 bad=0

awk -F '\t' 'NR>1 && $6==54 {n++; cmd="rg -q \"Output file saved as\" \"" $9 "\" && rg -q \"Reached specified time limit|Target app exited\" \"" $9 "\""; if(system(cmd)==0) ok++} END{print "rc54=" n " ok=" ok+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
# rc54=185 ok=185
```

## Nonblocking Notes

- `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V4/` did not exist before this review; the only file written for this task is this CH2 output.
- The absence of CSS L4 evidence is still load-bearing for S-P2/S-P3. ACCEPT here means the packet fences that absence correctly; it does not mean SK-V12 has admitted CSS L4.
- The replay ledger uses `update_center` in the corpus column while the command operand still points at `skinny/test_data/update-center.json` for the file alias (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv:66-67`). That matches the PIN-V3 fold and is not a CH2 defect.

## Exact Fold Edits If REVISE

Not applicable. Verdict is ACCEPT; no CH2 fold edits are required.
