# H1 Wave Ledger — the honesty close: SK-V18 generalization closes (outcome S)

Date: 2026-06-03. Wave: SK-V18 H1 (§10, the LAST wave). Entry gate: G5/G6 ∧ PROVE closed — both met
(G6 `369f4ffe7`; PROVE `9eb2aca1f` + `1aba0422e`, Lock-14 102/0). Outcome: **S (honest residual — the
G6 absolute Mbps figure rides a quiet re-capture the host cannot currently provide; reported DIRECTIONAL
with the load caveat). Every other H1 conjunct GREEN.**

## 1 — Tasks (§10), all addressed
**(1) materialization_framing == lazy-rich-vs-eager-cssom** — DISCLOSED in `waves/prove/../h1/FRAMING.md`:
`track1_rich` is LAZY-rich (every typed field re-derived on demand from `(source, offset)` spans, nothing
retained), compared against lightningcss's EAGER owned CSSOM. The materialization-DEPTH is equal (the
9-field cssparser/lightningcss oracle proves the same typed product); the asymmetry is RETENTION (lazy vs
eager), disclosed EXPLICITLY. No unqualified "beats CSSOM"/"equal-work" claim is made.

**(2) corpus_in_timer == true** — the `css_canon_bench` plane: both sides parse the SAME real corpus bytes
COLD and produce a typed CSSOM-equivalent product; no micro-fixture, no warm path, no more-work-competitor.

**(3) g6_speedup_median_mbps (the DEFERRED G6 figure)** — produced on the corpus-in-timer symmetric timer
(bench profile, fat-LTO, opt-level=3, cold per-parse). `track1_rich` over lightningcss:
```text
corpus                    track1_rich   lightningcss   ratio
bootstrap                 2357.5 MB/s   1120.8 MB/s    2.10x
tailwindcss               2737.3 MB/s    837.8 MB/s    3.27x
material-components-web   2413.4 MB/s   1286.3 MB/s    1.88x
animate                   1824.8 MB/s   1217.0 MB/s    1.50x
```
**host_loadavg = 6.79** (1m, stamped) at capture — WELL above the 1.0 quiet bar (the SK-V18 cycle ran
sustained-cargo the whole session). Per §5-risk-7 + the §10 falsifier, an ABSOLUTE Mbps claim is
FORBIDDEN here; these figures are reported **DIRECTIONAL** (the ratios are robust — both planes ran under
the same load so the RATIO is load-fair; the absolute Mbps is not quiet-clean). A quiet re-capture
(loadavg < 1.0) is the follow-on to upgrade S→A; it does not change the directional verdict (G6's dispatch
NEON beats lightningcss on every corpus, as G6 already established).

**(4) regen_check_clean == true** — the per-target regen checks are byte-clean: `check-json` exit 0,
`check-css-l4-declaration-values` exit 0, `check-sheets` exit 0; the pre-commit `regen --check --staged`
passed on every SK-V18 commit. (`xtask regen --check` with no subcommand is not a command — the gate is
the per-target `check-*` set + the `--staged` variant.) No git-dirty generated grammar file.

**(5) G2 >SOTA-regression gate held** — same-run `track1_rich/lightningcss > 1.0×` on EVERY corpus
(2.10× / 3.27× / 1.88× / 1.50×), re-confirmed DIRECTIONALLY against the G2-recorded antecedent; no same-run
regression. The S-P1 absolutes (bootstrap 2.190× / tailwind 3.375× / material 1.658× / animate 2.101×) are
the DIRECTIONAL antecedent, not the binding floor; the present figures re-confirm the directional >SOTA.

## 2 — H1 telemetry
```text
materialization_framing  = lazy-rich-vs-eager-cssom   (disclosed; the retention asymmetry explicit)
corpus_in_timer          = true
regen_check_clean        = true                        (check-json/css/sheets exit 0; regen --check --staged green)
host_loadavg             = 6.79                         (1m; >= 1.0 → no absolute Mbps claim)
g6_speedup_median_mbps   = DIRECTIONAL                  (2.10x/3.27x/1.88x/1.50x track1_rich over lcss; absolute deferred to a quiet re-capture)
json_guard_held          = true                        (check-json byte-identical → the 51/51 parser unchanged)
css_sota_ratio_held      = true                        (> 1.0x on every corpus, directional)
```

## 3 — SK-V18 close
On H1 close, the SK-V18 generalization cycle CLOSES. The thesis is realized + honestly framed: ONE
grammar-driven generator emits JSON (SinkOnly), CSS (Scan), and Google Sheets (general recursive-descent)
from `.bbnf`, dispatched by SHAPE, JSON/CSS byte-identical, the CSS >SOTA claim framed as lazy-rich (not
"equal-work"), the G6 NEON speedup directional under load (absolute pending a quiet re-capture). The
negative control (PROVE) passed — generalization is REAL, proven by a structurally-novel third grammar.
Outcome **S** (honest residual); upgrades to **A** on a loadavg-clean G6 re-capture.
