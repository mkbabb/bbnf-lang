# SK-V12 S-P1 PIN-V5 CH2 - Generality / Lock 14

Verdict: ACCEPT
Score: 98%

## Blocking Findings

None.

Review base was `HEAD ecda8b131efca2fbf9a4acfe67efef2a3c13e8b4`. CH2 scope is grammar generality and Lock 14: profile facts may nominate grammar-neutral primitive families, but no JSON-only evidence may admit CSS L4, Sheets, BBNF-self, or any non-JSON row. That matches the S-P1 CH2 contract in `restart/prompts/skinny/PASS-1-PROFILE.md:129-135`; convergence still follows `restart/prompts/ORCHESTRATOR.md:104-121`, which requires two consecutive cycles at >=95% ACCEPT with zero open critical defects and no orphan unresolved REVISE.

No JSON-only evidence is promoted to CSS L4 admission. The pin manifest keeps `skinny/RESULTS.md` as row authority, records 458 pin-era replay rows, and says CSS L4 is unprofiled because there is no generated CSS L4 Track 1 runtime, same-plane lightningcss comparator, or strict equality oracle (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:28-29`, `:70-73`, `:172-177`). P1-A records parse evidence as diagnostic, rejects report fixtures/root CSS snippets/lightningcss-only runs as substitutes, and says the missing CSS L4 parser does not authorize Sheets or BBNF-self fallback (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:142-158`, `:181-183`). P1-B keeps JSON product rows as guard/diagnostic evidence and separates Track 2/oracle work from generated Track 1 antecedents (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:46-50`, `:162-168`, `:187-195`). P1-D says PMU values do not move rows or create the CSS L4 row (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:67-80`, `:228-231`, `:258-261`). P1-E marks CSS L4 as the one legitimately absent hot-leaf lane and keeps comparator/oracle symbols out of Track 1 claims (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:32-50`, `:143-161`, `:165-201`). P1-F extracts only JSON rows and states JSON sonic/serde rows cannot fill the CSS L4 lightningcss close bar (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:24-33`, `:80-93`).

CSS L4 missing-baseline treatment remains a prerequisite, not a substitute. The user pin makes CSS L4 authoritative, sets the close floor to generated CSS L4 Track 1 greater than `lightningcss_mbps + 1`, keeps parse-only diagnostic, and requires `GrammarConfig` before CSS L4 emission is legal (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`, `:80-103`, `:141-150`). The handoff repeats that generated CSS L4 has no admitted row, Sheets/BBNF-self are fallback-only, ADMIT requires generated CSS L4 Track 1 plus oracle/equality/lightningcss evidence, and S-P3 must land `GrammarConfig`, generated-size telemetry, and O(N) guarding before W1b (`restart/skinny/tranches/sk-v12/HANDOFF.md:34-39`, `:53-65`, `:74-85`, `:112-123`). P1-C records the concrete blocker list: no generated CSS runtime module, non-JSON codegen emission is guarded, JSON template policy is embedded, `GrammarConfig` is not landed, and `/tmp/skv12-pin-p1` has no CSS comparator row (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:91-110`, `:116-123`).

Lock-14 and `GrammarConfig` leaks are bounded and routed. The pin carries Lock 14 grammar-neutrality and requires the seven leaks to be resolved by W1's `GrammarConfig` surface before CSS L4 emission (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:97-103`, `:148-150`). The value API audit names 5 major plus 2 embedded leaks and routes the minimal legal shape through `GrammarConfig`, per-grammar metadata modules, and parametrized view generation (`restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63-107`, `:160-207`). The handoff makes `GrammarConfig` or equivalent generated metadata mandatory before leaving JSON-only templates and says grammar-name branches in generic crates fail closed (`restart/skinny/tranches/sk-v12/HANDOFF.md:119-123`, `:153-155`).

Generated-size/O(N) routing is bounded. The current handoff requires generated CSS runtime size before redress, including generated LOC, module byte size, regen/check command, and an O(N) grammar-size guard; overflow blocks W1b until traced (`restart/skinny/tranches/sk-v12/HANDOFF.md:121-123`). The CSS L4 gate telemetry must consume generated LOC, generated module byte size, O(N) grammar-size status, Lock 14 status, Lock 16 status where applicable, same-wave consumer class, JSON guard state, gate status, wave id, and REDRESS id (`restart/skinny/tranches/sk-v12/HANDOFF.md:144-151`).

Sheets and BBNF-self remain fallback-only. The user pin says they become fallbacks only after a CSS L4 redress attempt fails, not after preflight failure (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-24`). The handoff repeats fallback-only status in the current state, priority list, FIXPOINT conditions, S-P3 requirements, and refusal conditions (`restart/skinny/tranches/sk-v12/HANDOFF.md:37-38`, `:63-65`, `:87-95`, `:126-127`, `:159-163`). The P1 packet preserves that boundary in P1-A, P1-C, P1-E, and P1-F (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:181-183`; `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:91-94`, `:120-121`; `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:47-50`; `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:80-87`).

Prior-cycle state supports clean PIN-V5 convergence if the rest of the cycle is also clean. PIN-V3 had five ACCEPT and one REVISE, so it did not satisfy §3Z (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md:12-19`). Its fold normalized the replay corpus-key defect for PIN-V4 review (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V3/CONSOLIDATED.md:23-40`). PIN-V4 returned six ACCEPT, zero REVISE, zero REJECT and is explicitly the first consecutive all-ACCEPT S-P1 cycle under the user pin (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V4/CONSOLIDATED.md:12-20`). PIN-V4 routes to PIN-V5, and S-P1 converges only if PIN-V5 also returns all ACCEPT (`restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V4/CONSOLIDATED.md:44-47`). This CH2 ACCEPT can therefore serve as the CH2 component of the second consecutive all-ACCEPT cycle; full S-P1 convergence still depends on the other five PIN-V5 lenses and the consolidation also returning clean ACCEPT.

## Nonblocking Notes

- `restart/skinny/tranches/sk-v12/research/p1/hardening/PIN-V5/` did not exist before this review; it was created only to write this assigned output path.
- The absence of CSS L4 evidence is load-bearing for S-P2/S-P3. ACCEPT here means the packet fences that absence correctly; it does not mean SK-V12 has admitted CSS L4.
- Commands run:

```bash
git status --short
# no output

git rev-parse HEAD
# ecda8b131efca2fbf9a4acfe67efef2a3c13e8b4

awk -F '\t' 'NR>1{total++; if(tolower($2 FS $3 FS $4 FS $5) ~ /(css|lightningcss|sheets|bbnf)/) hits++; if($3=="parse") parse++; if($3=="direct") direct++; if($3=="typed") typed++} END{print "total=" total " semantic_nonjson_hits=" hits+0 " parse=" parse+0 " direct=" direct+0 " typed=" typed+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# total=458 semantic_nonjson_hits=0 parse=170 direct=204 typed=84

awk -F '\t' 'NR>1 && ($5 !~ /^(track1|track2|real_typed_track1|real_typed_track2)$/ || $4=="update-center" || $1=="json" || $3=="css" || $3=="sheets" || $3=="bbnf-self") {print NR ":" $0; bad++} END{print "bad",bad+0}' restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# bad 0

find skinny/crates/runtime/src/grammars -maxdepth 2 -type d | sort
# skinny/crates/runtime/src/grammars
# skinny/crates/runtime/src/grammars/json
# skinny/crates/runtime/src/grammars/sheets_witness

rg -n "css_l4|lightningcss|CSS L4" skinny/RESULTS.md restart/skinny/tranches/sk-v12/research/p1/skv12-p1-pin-replay.tsv
# no output; exit 1

awk -F '\t' 'NR>1{total++; if($7!="PASS") bad++} END{print "xctrace_total=" total " bad=" bad+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
# xctrace_total=212 bad=0

awk -F '\t' 'NR>1 && $6==54 {n++; cmd="rg -q \"Output file saved as\" \"" $9 "\" && rg -q \"Reached specified time limit|Target app exited\" \"" $9 "\""; if(system(cmd)==0) ok++} END{print "rc54=" n " ok=" ok+0}' /tmp/skv12-pin-p1/xctrace/capture_status.tsv
# rc54=185 ok=185
```

## Exact Fold Edits If REVISE

Not applicable. Verdict is ACCEPT; no CH2 fold edits are required.
