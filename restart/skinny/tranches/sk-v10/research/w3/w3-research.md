# SK-V10 W3 Research - Parse-Only Firewall

Pass: Wave Research.
Cycle: W3.
Date: 2026-05-19.
Scope: read-only audit for `G-W3-PARSE-FIREWALL`.

## Inputs

- W2 closed under REDRESS 101. `direct_to_struct` is now 5 `A / GO` and 12
  `N-direct / NO-GO`.
- W3 is proof-only. It must not reopen the retired union/event/class-column
  substrate or move any row.
- SPEC Section 6 requires an audit for W3 aliases, parse-only SOTA claims, and
  W4-through-W3 dependencies.

## Active Packet Audit

Command:

```text
rg -n 'UnionTape|class column|class-column|streaming cursor|structural cursor|W4 cascade-lock|cascade-lock|parse-only SOTA|parse_only.*A / GO|parse_only.*GO' \
  restart/skinny/tranches/sk-v10/SPEC.md \
  restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md \
  restart/skinny/tranches/sk-v10/HANDOFF.md \
  restart/skinny/tranches/sk-v10/SYNTHESIS.md
```

Findings:

- `SPEC.md`, `DISPATCH-PROMPT.md`, `HANDOFF.md`, and `SYNTHESIS.md` mention W3
  aliases only as retirements, refusal conditions, or pre-blocked routes.
- No active wave manifest routes W4 through a W3 substrate. W4 is conditional
  on W3 firewall close, not on a W3 implementation consumer.
- The only `parse_only` mentions in active close docs keep it diagnostic
  `S / NO-GO`; no parse-only SOTA admission text was found.

## Result Table Audit

Command:

```text
node - <<'NODE'
const fs=require('fs');
const text=fs.readFileSync('skinny/RESULTS.md','utf8');
let rows=0,bad=[];
for (const line of text.split('\n')) {
  if (!line.startsWith('| ')) continue;
  const cols=line.split('|').slice(1,-1).map(s=>s.trim());
  if (cols[1] !== 'parse_only') continue;
  rows++;
  if (cols[2] !== 'S' || cols[3] !== 'NO-GO') bad.push(`${cols[0]} ${cols[2]}/${cols[3]}`);
}
console.log(`parse_only rows=${rows}; bad=${bad.length ? bad.join(', ') : 'none'}`);
NODE
```

Result:

```text
parse_only rows=17; bad=none
```

## Existing Gate Shape

`Report::validate_sk_v8_w0` already rejects parse-only SOTA movement:

- unchanged parse rows validate only as the inherited `S / NO-GO` baseline or
  as diagnostic `G/L/M/S` relabels;
- verdict movement from `NO-GO` to `GO` fails the baseline check;
- `TelemetryRow::validate_sk_v8_w0` rejects any parse row outcome outside
  `I/J/K/L/M/S` with the error shape `parse row admitted outside substrate
  guard`;
- `report::tests::w0_report_accepts_exact_opening_baseline` contains a
  negative parse-admission mutation and expects validation failure.

## Recommendation

Proceed to W3 plan as proof-only. No source behavior or row movement is needed.
The redress evidence should run the existing parse-admission rejection test and
the frozen `gate-json --check-results` consumer, then close W3 by updating
REDRESS and the active close documents.
