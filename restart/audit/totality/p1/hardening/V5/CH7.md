---
agent: CH7
pass: T-P1-excavation
cycle: V5
lens: OVERFIT-PRUNE
disposition: ACCEPT
generated_at: 2026-05-23T21:10:00-04:00
inputs_audited:
  - restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7 (lens definition; lines 62-87)
  - restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md (V5 dispatch §0-§5; LOCK-trigger cycle)
  - restart/audit/totality/p1/hardening/V4/CH7.md (V4 ACCEPT; 9/9 = 100% — second consecutive ≥95% cycle; standalone LOCK-eligible)
  - restart/audit/totality/p1/hardening/V3/CH7.md (V3 ACCEPT; 9/9 = 100% — first ≥95% cycle)
  - restart/audit/totality/p1/hardening/V2/CH7.md (V2 REVISE; 6/9 = 66.7%)
  - restart/audit/totality/p1/1A-substrate-evidence.md (V4-LOCKED; zero V5 diff)
  - restart/audit/totality/p1/1B-codegen-evidence.md (V3-LOCKED; zero V5 diff)
  - restart/audit/totality/p1/1C-runtime-evidence.md (V4-LOCKED; zero V5 diff)
  - restart/audit/totality/p1/1D-skinny-lessons.md (V4-LOCKED; zero V5 diff)
  - restart/audit/totality/p1/1E-locks-evidence.md (V5 amended; single-cell trivial cosmetic anchor refresh `:126-128` → `:128-130` at 1E:35 self-cite)
  - restart/audit/totality/p1/1F-anti-pattern.md (V4-LOCKED; zero V5 diff)
  - restart/audit/totality/p1/1F-coherence-scan.md (V3-LOCKED; zero V5 diff)
  - restart/audit/totality/p1/1F-past-corpora.md (V3-LOCKED; zero V5 diff)
  - skinny/crates/bbnf-bench/src/nonjson_css_l4.rs (HEAD; AP-009 lightningcss_facts hit count + sibling functions)
  - crates/core/src/runtime/mod.rs (HEAD; reexport window :25-71 mechanical extraction at 133 raw - 6 in-window neutrals = 127)
  - restart/locks/LOCKS.md (HEAD; zero CH7/Overfit hits)
  - HEAD = 9833295d5 (T-P1 V5 atomic cosmetic fold; 1 inventory file amended over V4 8f4756113 baseline)
---

## Lens Contract

CH7 Overfit-Prune (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`) lenses every artefact for: (a) generated-vs-hand-written discipline; (b) Lock 14 generic-crate compliance; (c) every admit lands via a real parser/codegen/SIMD source change with strict-vs-strict comparator and per-iteration equality oracle; (d) every "generated" output passes a delete + regen ⇒ byte-equivalent round-trip; (e) no SCAFFOLD-ONLY landing counts as an admit. CH7 REJECT triggers immediate plan revise OR redress revert with REDRESS entry; CH7 may not be carried as "acknowledged but not blocking".

For the T-P1 V5 cycle, CH7 is the lens whose V3 disposition (100%) was the first ≥95% cycle and whose V4 disposition (100%) was the second consecutive ≥95% cycle — making CH7 STANDALONE LOCK-eligible at V4 close per §3Z (≥95% × 2 consecutive cycles; V≤5 ceiling). V5 is the LOCK-TRIGGER cycle whose CH7 obligation is to verify carry-forward integrity (V4-LOCKED inventories byte-identical) and confirm the single V5 cosmetic-fold edit (1E:35 anchor refresh `:126-128` → `:128-130`) introduces NO new fabrication and remains anchor-truthful at HEAD. The V5 fold packet is a single atomic micro-edit at 1E:35 (single-cell trivial cosmetic anchor refresh) — the smallest possible fold a confirming cycle can carry. **V5 introduces NO new CH7-class load-bearing edit.** V5 CH7 disposition therefore turns on (i) verifying the 1E:35 cosmetic anchor refresh is HEAD-truthful (the LAC-1E-12 block actually lives at the new `:128-130` anchor, not at the old `:126-128`), (ii) verifying NO new fabrication was introduced by V5 itself, and (iii) confirming the V3/V4 CH7 100% ACCEPT verdicts on the locked surfaces (7/8 inventories) carry forward byte-identical.

## Findings

### §1 — V5 disposition focus per dispatch (three checks)

| # | Focus | Result | Evidence |
|---|---|---|---|
| (i) | F-V5-CH6-1 1E:35 self-cite cosmetic anchor refresh `:126-128` → `:128-130` HEAD-truthful at the LAC-1E-12 promotion candidacy block | **ACCEPT** | 1E-locks-evidence.md:35 carries the single substitution `1E-locks-evidence.md:128-130` (V4 had `1E-locks-evidence.md:126-128`). HEAD verification (§2.1): `awk 'NR>=128 && NR<=130' restart/audit/totality/p1/1E-locks-evidence.md` returns the LAC-1E-12 promotion candidacy heading (`### LAC-1E-12 promotion candidacy (T-P1 V2 fold — §1.5 governance signal)`) at line 128 + the 2-line LAC-1E-12 governance prose at lines 129-130 verbatim. By contrast, `awk 'NR>=126 && NR<=128'` at HEAD shows the trailing row of LAC-1E-16 + a blank + the LAC-1E-12 heading — the V4 anchor `:126-128` no longer hosts the LAC-1E-12 promotion candidacy body. The V5 cosmetic refresh corrects a 2-line drift introduced when LAC-1E-13 through LAC-1E-16 entries grew (the LAC-1E-12 promotion candidacy block shifted 2 lines downward). The refresh is anchor-truthful: the old offset became stale, the new offset is true at HEAD. |
| (ii) | NEW-CH2-V3-02 orphan-cell propagation guard satisfied at the V5 fold site (only ONE site to check since the cite is a self-reference internal to 1E) | **ACCEPT** | The 1E:35 paragraph carries a single inline cross-reference to the LAC-1E-12 promotion candidacy block. §2.2 grep enumerates all `:126-128` and `:128-130` cite tokens across 1E: HEAD returns exactly ONE hit, located at 1E:35, citing the new `:128-130`. Zero `:126-128` orphan residuals at HEAD (the prior V4 anchor token is fully purged from 1E). Pre-fold state (V4 8f4756113): one `:126-128` cite at 1E:35. Post-fold state (V5 HEAD 9833295d5): one `:128-130` cite at 1E:35; zero `:126-128` residual. **NEW-CH2-V3-02 orphan-cell propagation guard satisfied** at the V5 fold site (1:1 substitution; zero orphan tokens; total token-class refresh). |
| (iii) | Self-test: V5 introduces NO new fabrication of the CH7-classification class | **ACCEPT** | The V5 fold packet is a single 1-line substitution (`:126-128` → `:128-130`) at 1E:35. Re-executed at HEAD: (a) the new anchor `:128-130` is HEAD-truthful — `awk 'NR==128'` returns the LAC-1E-12 heading, `awk 'NR>=129 && NR<=130'` returns the LAC-1E-12 governance prose (§2.1). (b) zero `:126-128` orphan residuals across 1E (§2.2). (c) zero diff across the 7 V4-LOCKED inventories (1A, 1B, 1C, 1D, 1F-anti-pattern, 1F-coherence-scan, 1F-past-corpora) per §2.3 + §2.4. (d) the V4-verified executable verifications all re-pass at V5 HEAD (§2.5-§2.9 carry-forward). The V5 fold author observed LAC-1E-12 + NEW-CH2-V3-02 procedural addendums at the single fold site: 1E:35 substitution was pre/post grep-verified before commit; the new anchor was HEAD-truthful at commit time. **V5 carries no new CH7-class fabrication anywhere.** |

### §2 — Executable verification (re-run at HEAD, commit 9833295d5)

#### §2.1 F-V5-CH6-1 cosmetic fold target — LAC-1E-12 block at new `:128-130` anchor

```
$ awk 'NR>=128 && NR<=130' /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1E-locks-evidence.md
### LAC-1E-12 promotion candidacy (T-P1 V2 fold — §1.5 governance signal)

T-P1 V2 promotes **LAC-1E-12 from candidate-addition to candidate-promoted-to-T-P3-§3C-priority** as the most substantive cross-lens governance signal surfaced by V1 hardening. ...

$ awk 'NR>=126 && NR<=128' /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1E-locks-evidence.md
| **LAC-1E-16** | **addition (NEW SK-V14)** | L08 audit-overlay column binding | ... | SK-V14 D-1E-16 |

### LAC-1E-12 promotion candidacy (T-P1 V2 fold — §1.5 governance signal)
```

**Expected per V5 dispatch §2 CH6 focus**: LAC-1E-12 promotion candidacy block lives at `:128-130` at HEAD (not at `:126-128` as V4 cited); the V5 cosmetic refresh corrects a stale anchor. **Observed**: line 128 is the LAC-1E-12 heading; lines 129-130 are the LAC-1E-12 governance prose. Line 126 is the trailing row of LAC-1E-16; line 127 is blank; line 128 is the LAC-1E-12 heading. The old `:126-128` anchor pointed to a 1-of-3 LAC-1E-12 fragment (trailing LAC-1E-16 row + blank + heading only); the new `:128-130` anchor points to the full heading + governance prose. **PASS** — V5 cosmetic refresh is anchor-truthful at HEAD.

#### §2.2 NEW-CH2-V3-02 orphan-cell propagation guard — pre/post token enumeration

```
$ grep -n ":126-128\|:128-130" /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1E-locks-evidence.md
35:**Sustained-UNKNOWN posture (anti-paper-close anchor; F-V4-CH6-1 close of V1 CH6 REVISE #4 + CH1 V3 finding 7 carry-forward).** ... Cross-reference to §1.5 LAC-1E-12 promotion candidacy block at `1E-locks-evidence.md:128-130`; all four UNKNOWNs are explicit anti-paper-close anchors per CH6 lens.

$ awk 'NR==35' /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1E-locks-evidence.md | grep -o ":128-130\|:126-128"
:128-130
```

**Expected per V5 dispatch §2 NEW-CH2-V3-02 propagation guard**: exactly ONE cite at 1E:35 carrying the new `:128-130` token; ZERO `:126-128` orphan residuals across 1E. **Observed**: exactly one hit, at 1E:35, carrying `:128-130`. Zero `:126-128` orphan residuals anywhere in 1E. **PASS** — single-cell trivial cosmetic refresh; total token-class refresh; orphan-cell propagation guard satisfied at the V5 fold site.

#### §2.3 V4-LOCKED inventories zero-drift verification (1A, 1C, 1D, 1F-anti-pattern)

```
$ git diff 8f4756113 HEAD --stat -- \
    restart/audit/totality/p1/1A-substrate-evidence.md \
    restart/audit/totality/p1/1C-runtime-evidence.md \
    restart/audit/totality/p1/1D-skinny-lessons.md \
    restart/audit/totality/p1/1F-anti-pattern.md
(no output; exit 0)
```

**Expected per V5 dispatch §1 artefact roster**: 1A, 1C, 1D, 1F-anti-pattern carry V4-LOCKED status with zero V5 edits. **Observed**: zero-line diff across all 4 V4-LOCKED axes. **PASS** — V4 CH7 ACCEPT verdicts on these 4 inventories carry forward to V5 byte-identical.

#### §2.4 V3-LOCKED inventories zero-drift verification (1B, 1F-coherence-scan, 1F-past-corpora)

```
$ git diff 0a9f1288c HEAD --stat -- \
    restart/audit/totality/p1/1B-codegen-evidence.md \
    restart/audit/totality/p1/1F-coherence-scan.md \
    restart/audit/totality/p1/1F-past-corpora.md
(no output; exit 0)
```

**Expected per V5 dispatch §1 artefact roster**: 1B, 1F-coherence-scan, 1F-past-corpora carry V3-LOCKED status with zero V4 + zero V5 edits. **Observed**: zero-line diff across all 3 V3-LOCKED axes (V3 baseline 0a9f1288c → V5 HEAD 9833295d5). **PASS** — V3 CH7 ACCEPT verdicts on these 3 inventories carry forward to V5 byte-identical.

#### §2.5 V4 §2.1 carry-forward — F-V4-CH1-1 AP-009 27 hits + zero "24 hits" orphans

```
$ grep -n "lightningcss_facts" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench/src/nonjson_css_l4.rs | wc -l
27

$ grep -nc "24 hits" /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1F-anti-pattern.md
0

$ grep -nc "27 hits" /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1F-anti-pattern.md
1
```

**Expected per V4 F-V4-CH1-1 carry-forward**: 27 hits at HEAD; AP-009 cites 27; zero "24 hits" orphans. **Observed**: 27 hits exactly at HEAD; 1F-anti-pattern carries 1 "27 hits" cite; 0 "24 hits" orphans. **PASS** — V4 cosmetic correction continues to hold at V5 HEAD.

#### §2.6 V4 §2.2 carry-forward — 127 reexports mechanical extraction + NEW-CH2-V3-02 propagation

```
$ awk 'NR>=25 && NR<=71' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/mod.rs \
  | python3 -c "import sys, re; t=sys.stdin.read(); print(sum(len([s for s in (m.group(1) if m.group(1) else m.group(2)).split(',') if s.strip()]) for m in re.finditer(r'pub use\s+[\w_:]+(?:::\{([^}]+)\}|::(\w+))', t)))"
133

# 133 raw - 6 in-window neutrals (StructBuilder :33, GenericAtRule :42, DtaError+ParseErr :58, CompoundHandle+StringHandle :63) = 127

$ grep -nc "127 grammar-named\|127 distinct grammar-named\|\*\*127 distinct" /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1C-runtime-evidence.md
7

$ grep -nc "126 grammar-named\|126 distinct" /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1C-runtime-evidence.md
0
```

**Expected per V4 F-V4-CH2-1 carry-forward**: 127 reexports at HEAD; 1C carries 127 token at the 7 enumerated sites (`:21, :23, :40, :92, :124, :162, :201` per V4 §3.3); zero `126` orphans. **Observed**: HEAD mechanical extraction yields 133 raw → 127 (with the 6 in-window neutrals enumerated per NEW-CH2-V2-03 discipline); 1C carries 7 distinct `127` cite phrasings (V4 §2.2 reported 4 distinct phrasing classes, which underspecified — the actual hit count is 7, matching the 7 enumerated sites V4 §3.3 listed); zero `126` orphans. **PASS** — V4 NEW-CH2-V3-02 propagation guard continues to hold at V5 HEAD with all 7 sites carrying the canonical 127 token.

#### §2.7 V4 §2.4 carry-forward — 1A row 100→117 cross-cite + zero `:100` orphans

```
$ grep -nE ":100[^0-9]|row 100" /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1A-substrate-evidence.md
(no output; exit 1)

$ grep -c "1D \`:117\`" /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1A-substrate-evidence.md
2
```

**Expected per V4 F-V4-CH5-1 carry-forward**: zero `:100` orphans across 1A; 2 `1D :117` cross-cite sites. **Observed**: zero `:100` orphans; 2 `1D :117` cites (at :10 and :84). **PASS** — V4 cross-cite refresh continues to hold at V5 HEAD.

#### §2.8 V4 §2.5 carry-forward — 1E Open Questions table at :159-168 with 4 UNKNOWNs + verify_actions

```
$ awk 'NR>=159 && NR<=168' /Users/mkbabb/Programming/bbnf-lang/restart/audit/totality/p1/1E-locks-evidence.md
| UNKNOWN | Why unknown | verify_action |
|---|---|---|
| L03 cursor elision | sustained from V4 — no `__EAGER_EMPTY_PATH` artifact at SK-V14 baseline | sustained from V4 |
| L16 full allowlist coverage | sustained from V4 — V+1 primitive manifest binding present in LOCKS.md but per-use-site mapping artifact still pending | sustained from V4 |
| **NEW SK-V14: Does SK-V14 SYNTHESIS §2 audit-overlay column binding require any current row's xtask gate-json delta beyond R1 + R2 + CH5 wave deliverables?** | The 4 NEW columns map to C-2 wave deliverable per `SYNTHESIS.md:272` C-2 row; the column gap is total (zero population per CH7 V3 §2.5) at SK-V14 zero-implementation baseline. | Verify in C-2 redress: capture `grep -c 'track2_entry_point\|comparator_plane\|per_iter_equality\|audit_overlay_verdict' skinny/RESULTS.md` output, then bind each column population to a `xtask gate-json` rejection rule. |
| **NEW SK-V14: Does Lock 1 V+1 fact-stream wording at `LOCKS.md:66-71` already admit CSS L4 as 5th substrate category, or does LAC-1E-14 require explicit `FactStream` taxonomy addition?** | ... | T-P3 disposes: either (a) explicit `FactStream` taxonomy addition extends BackendShape to 5 variants (changes Lock 10 too), or (b) fact-stream stays as `admitted_fact_output` substrate_target per V+1 §75-82 without taxonomy promotion. |
```

**Expected per V4 F-V4-CH6-1 carry-forward**: Open Questions table at :159-168 carries 4 UNKNOWNs (L03, L16, NEW audit-overlay column gap, NEW fact-stream taxonomy) with executable verify_actions. **Observed**: table present at :159-168 verbatim; all 4 UNKNOWNs carry verify_action prose unchanged from V4. The V5 cosmetic refresh at 1E:35 (now citing `:128-130` for the LAC-1E-12 cross-reference) did NOT alter the Open Questions table line offsets — the verify_action cites at `:161-164` remain valid. **PASS** — V4 sustained-UNKNOWN paragraph + 4 verify_actions continue to hold at V5 HEAD with anti-paper-close discipline preserved.

#### §2.9 V4 §2.7 carry-forward — LOCKS.md CH7-binding existence audit

```
$ grep -n "CH7\|Overfit" /Users/mkbabb/Programming/bbnf-lang/restart/locks/LOCKS.md
(no output; exit 1)
```

**Expected per V2 COH-012 + V3 + V4 carry-forward**: zero hits. **Observed**: zero hits at V5 HEAD. **PASS** — the V2 anti-fabrication phrasing at `1F-coherence-scan.md:74, :93, :110, :127` and the LAC-1E-12 canonical template at `1E-locks-evidence.md:128` (new V5 anchor) remain true at V5 HEAD.

#### §2.10 V4 §2.8 carry-forward — google_sheets file count + runtime dir census

```
$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/google_sheets -type f -name '*.rs' | wc -l
10

$ find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l
9
```

**Expected per V2/V3/V4 carry-forward**: google_sheets=10; runtime dir census=9. **Observed**: exact match at V5 HEAD. **PASS** — V2 COH-011/AP-016/PC-017 fix carries through V3 → V4 → V5 unchanged.

### §3 — Cross-cutting CH7 observations

#### §3.1 V5 fold packet is the smallest possible confirming-cycle fold

The V5 fold packet declares ONE atomic micro-edit at 1E:35 (single-cell trivial cosmetic anchor refresh `:126-128` → `:128-130`). Per V5 dispatch context §1, this is the minimum reasonable fold for a LOCK-trigger cycle: 7 of 8 inventories carry V4-LOCKED or V3-LOCKED status with zero V5 edits, and the single fold target is a 1-line substitution correcting a stale self-cite drift (the LAC-1E-12 promotion candidacy block shifted 2 lines downward when LAC-1E-13 through LAC-1E-16 entries were added in earlier cycles; the V5 fold refreshes the cross-cite anchor to the true HEAD offset).

**No V5 edit introduces a new CH7-class load-bearing claim.** The V5 fold action is purely cosmetic anchor-refresh discipline — the same class as V3's cite-rebind refresh and V4's cosmetic count refresh (AP-009 24→27), now extending to anchor-refresh as a 5th cite-rebind cost-neutrality class per V4 dispatch §2 CH4 narrative.

#### §3.2 Self-test: V5 introduces NO new fabrication of the CH7-classification class

The deepest meta-CH7 risk for V5 was that the single-line cosmetic anchor refresh might point to a stale or wrong target offset at HEAD. Self-test results:

- **The new anchor `:128-130` is HEAD-truthful**: §2.1 `awk 'NR>=128 && NR<=130'` returns the LAC-1E-12 promotion candidacy heading at line 128 + the LAC-1E-12 governance prose at lines 129-130 — exactly the block the V5 cross-reference cites.
- **The old anchor `:126-128` was stale at V4 HEAD**: §2.1 `awk 'NR>=126 && NR<=128'` shows lines 126-127 hold trailing LAC-1E-16 row + blank, and only line 128 is the LAC-1E-12 heading. The V4 cite was off by 2 (pointing to a 1-of-3 fragment instead of the full block); V5 correctly refreshes to the true offset.
- **NEW-CH2-V3-02 orphan-cell propagation guard satisfied**: §2.2 confirms 1:1 substitution at 1E:35 with zero `:126-128` orphan residuals anywhere in 1E.
- **Zero new fabricated cite cluster** introduced in 1E or anywhere else in the T-P1 inventory set (§2.3 + §2.4 confirm 7-of-8 inventories are zero-diff at V5).

**Self-test PASS.** V5 carries no new CH7-class fabrication. The single cosmetic anchor refresh is anchor-truthful at HEAD; the procedural addendums (LAC-1E-12 + NEW-CH2-V3-02) held at the single fold site.

#### §3.3 NEW-CH2-V3-02 orphan-cell propagation guard verification — single fold-site

The V5 dispatch context §2 CH7 focus required that NEW-CH2-V3-02 orphan-cell propagation guard be applied at the single V5 fold site (F-V5-CH6-1). Verification:

- **F-V5-CH6-1 (1E:35 `:126-128` → `:128-130`)**: §2.2 `grep -n ":126-128\|:128-130"` returns exactly ONE hit at 1E:35 carrying `:128-130`; zero `:126-128` orphans across 1E. Propagation guard satisfied via 1:1 token substitution.

**NEW-CH2-V3-02 orphan-cell propagation guard satisfied at the single V5 fold site.** Procedural addendum institutionalization holds at V5 (3rd consecutive cycle: V3 institutionalized at 3 inline sites; V4 added a 4th at 1C :201; V5 carries all 4 forward unchanged + adds the 1E:35 cross-reference refresh as a 5th propagation-guard-verified site).

#### §3.4 Executable-verification mandate institutionalization (LAC-1E-12 carry-forward + V5 carry)

V3 institutionalized the LAC-1E-12 executable-verification mandate at 3 inline rebind sites (1F-anti-pattern :55, :69; 1A :10). V4 added a 4th via F-V4-CH2-1 at 1C :201 (awk+python extraction command inline). V5 carries all 4 sites forward without erasure (per §2.3 + §2.4 zero-diff verification across the 4 host inventories), AND the V5 fold author observed the executable-verification mandate at the single fold site: the new `:128-130` anchor was HEAD-truthful before commit (verifiable via `awk 'NR>=128 && NR<=130'` per §2.1). The mandate now operates at 4 independent inline sites across 3 inventories (1A, 1C, 1F-anti-pattern) + 1 implicit fold-author verification site (1E V5 cosmetic refresh). **ACCEPT** — the mandate is reinforced at V5 via the third consecutive cycle of fold-author observance.

#### §3.5 V3-LOCKED and V4-LOCKED axes carry-forward integrity

V3-LOCKED axes (1B, 1F-coherence-scan, 1F-past-corpora) are zero-edited in both V4 and V5 (§2.4). V4-LOCKED axes (1A, 1C, 1D, 1F-anti-pattern) are zero-edited in V5 (§2.3). The V3 + V4 CH7 ACCEPT verdicts on the locked surfaces carry forward to V5 unchanged:

- 1A-substrate-evidence (V4-LOCKED; V5 zero diff; F-V4-CH5-1 row 100→117 refresh continues to hold per §2.7).
- 1B-codegen-evidence (V3-LOCKED; V5 zero diff; V3 Lock 14 generic-crate compliance prose continues to hold).
- 1C-runtime-evidence (V4-LOCKED; V5 zero diff; F-V4-CH2-1 127-reexport count continues to hold per §2.6 with full 7-site propagation guard satisfied).
- 1D-skinny-lessons (V4-LOCKED; V5 zero diff; F-V4-CH3-1 W13.9 CORRECTNESS-REJECT split + F-V4-CH5-1 row 117 substrate-union row continue to hold).
- 1F-anti-pattern (V4-LOCKED; V5 zero diff; F-V4-CH1-1 AP-009 27-hit count continues to hold per §2.5).
- 1F-coherence-scan (V3-LOCKED; V5 zero diff; COH-012 anti-fabrication phrasing continues to hold per §2.9 LOCKS.md zero CH7/Overfit hits).
- 1F-past-corpora (V3-LOCKED; V5 zero diff; google_sheets=10 propagation continues to hold per §2.10).

**Carry-forward integrity ACCEPT** at V5 for all 7 locked inventories.

#### §3.6 No SCAFFOLD-ONLY admits in any V5 inventory

CH7 prohibits SCAFFOLD-ONLY admits. The 7 V5-zero-diff inventories all carry forward their V3 + V4 CH7 ACCEPT verdicts (§3.5). The single V5-amended inventory (1E) carries a 1-line cosmetic anchor refresh that introduces no new admit, no new wave routing, and no new scope change — AP-017 at `1F-anti-pattern.md:77` continues to name W8/W9 as SCAFFOLD-ONLY footprint at 3 bench files and routes to PRUNE-5 wire (the correct CH7 disposition; unchanged in V5). **ACCEPT.**

#### §3.7 Lock 14 generic-crate compliance in V5 inventory text

V5 amended only 1E:35 (a 1-line cosmetic cross-reference refresh that does NOT touch any grammar-named source citation). The 7 V5-zero-diff inventories carry forward their V3 + V4 Lock 14 inventory-prose ACCEPT verdicts (1C exec summary cites `JsonValue, CssRule, BbnfArena` etc as grammar-named reexports; per CH7 lens definition `:75-77`, the prohibition applies to live source code, not audit text describing the leak surface). V5 inventory prose passes. **ACCEPT.**

#### §3.8 V5 third consecutive ≥95% cycle for CH7 → LOCK-trigger cycle confirms LOCK

V3 closed CH7 at 100% (9/9 ACCEPT) — first ≥95% cycle. V4 closed CH7 at 100% (9/9 ACCEPT) — second consecutive ≥95% cycle, achieving standalone LOCK-eligibility per §3Z. V5 closes CH7 at 100% (8/8 targets ACCEPT: 1 V5-amended inventory (1E) + 7 zero-diff carry-forward inventories) — third consecutive ≥95% cycle, confirming LOCK at V5 close. Per §3Z (≥95% × 2 consecutive cycles; V≤5 ceiling), CH7 had already triggered standalone LOCK at V4; V5 LOCK-trigger cycle confirms cohort §3Z LOCK eligibility at the V≤5 ceiling per V5 dispatch §3.

#### §3.9 Cycle-by-cycle CH7 fold-class taxonomy (LAC-1E-12 cite-rebind cost-neutrality discipline expanded to 5 classes)

V4 CH7 §3.3 named 4 cite-rebind cost-neutrality classes: (1) cosmetic count refresh (V4 AP-009 24→27); (2) self-cite token refresh; (3) cross-cite anchor refresh; (4) label split (V4 W13.5-W13.8/W13.9 separation). V5 institutionalizes the 5th class: **anchor-refresh** (the 1E:35 cite anchor `:126-128` → `:128-130` correction). The anchor-refresh class is distinguished from the cite-rebind class in that the cite target content is unchanged (the same LAC-1E-12 promotion candidacy block) but its physical line offset has drifted due to upstream additions (LAC-1E-13 through LAC-1E-16 entries growing). Anchor-refresh is the lowest-cost fold class: 1-line substitution; 1 grep verification; zero cite-target-content delta; zero downstream propagation. The V5 fold is the canonical example.

**The cite-rebind cost-neutrality discipline now has 5 documented classes** — the V4 packet cited 4 (cosmetic count, self-cite token, cross-cite anchor, label split); the V5 packet introduces and exemplifies the 5th (anchor-refresh). T-P3 §3C carries the 5-class taxonomy forward as part of the LAC-1E-12 procedural addendum institutionalization.

### §4 — V5 Disposition Summary

V5 dispatch focus per `CHALLENGE-CONTEXT.md §2 CH7`:

1. **Verify NO new fabrication introduced by V5** — ACCEPT (§1 (iii), §3.2). The single V5 cosmetic fold (1E:35 anchor refresh) is HEAD-truthful: the new `:128-130` anchor points to the LAC-1E-12 promotion candidacy heading + governance prose verbatim. No new fabricated cluster anywhere.
2. **NEW-CH2-V3-02 + LAC-1E-12 procedural addendums held at V5 fold site (pre/post grep evidence captured)** — ACCEPT (§1 (ii), §3.3). Pre-fold state: one `:126-128` cite at 1E:35. Post-fold state: one `:128-130` cite at 1E:35; zero `:126-128` orphans. 1:1 substitution; total token-class refresh.
3. **Re-execute every cite at HEAD with grep (executable verification mandate)** — ACCEPT (§2.1-§2.10). Ten distinct executable verifications all reproduce the V4-asserted + V5-refreshed cite text verbatim or with bit-for-bit numerical match.

## Cycle Disposition

**ACCEPT.** All three V5 dispatch focus checks land ACCEPT (§1):

1. **F-V5-CH6-1 1E:35 cosmetic anchor refresh `:126-128` → `:128-130`** — anchor-truthful at HEAD; new offset points to LAC-1E-12 heading + governance prose; old offset was stale (off by 2 due to upstream LAC-1E-13 through LAC-1E-16 additions); zero `:126-128` orphans across 1E.
2. **NEW-CH2-V3-02 propagation guard satisfied** — exactly ONE V5 fold site; 1:1 token substitution; pre/post grep evidence captured.
3. **Self-test: V5 introduces NO new fabrication** — every V5-added cite HEAD-verified; LAC-1E-12 + NEW-CH2-V3-02 procedural addendums held at the single V5 fold site.

**Carry-forward integrity**: 4 V4-LOCKED axes (1A, 1C, 1D, 1F-anti-pattern) + 3 V3-LOCKED axes (1B, 1F-coherence-scan, 1F-past-corpora) all zero-diff at V5; V4 + V3 CH7 ACCEPT verdicts on the 7 locked surfaces carry forward to V5 byte-identical (§2.3, §2.4). All 6 V4-validated executable verifications continue to pass at V5 HEAD (§2.5-§2.10).

**ACCEPT-rate (V5 dispatch overlay)**: **3 ACCEPT / 3 dispatch checks = 100%**.

**ACCEPT-rate (full-inventory overlay, 9-target denominator comparable to V2/V3/V4)**: 1A ACCEPT (V4-LOCKED, zero V5 drift), 1B ACCEPT (V3-LOCKED, zero V4+V5 drift), 1C ACCEPT (V4-LOCKED, zero V5 drift; 127 propagation continues to hold at 7 sites), 1D ACCEPT (V4-LOCKED, zero V5 drift), 1E ACCEPT (F-V5-CH6-1 cosmetic anchor refresh anchor-truthful at HEAD), 1F-coherence-scan ACCEPT (V3-LOCKED, zero V4+V5 drift), 1F-anti-pattern ACCEPT (V4-LOCKED, zero V5 drift; F-V4-CH1-1 27-hit count continues to hold), 1F-past-corpora ACCEPT (V3-LOCKED, zero V4+V5 drift), LOCKS.md governance ACCEPT (zero CH7 hits verified §2.9). **9 ACCEPT / 9 targets = 100%**.

**Trajectory**: V1 77.8% (7/9) → V2 66.7% (6/9) → V3 100% (9/9) → V4 100% (9/9) → **V5 100% (9/9)**. The V5 fold sustains the V4 cleanly-converged disposition with zero new fabrication and corrects the single residual stale anchor in 1E (cosmetic anchor refresh).

## LOCK Status

**CH7 LOCK CONFIRMED AT V5** per §3Z (≥95% × 2+ consecutive cycles; V≤5 ceiling):

- **V3 cycle**: CH7 = 100% (≥95% ✓) — first cycle.
- **V4 cycle**: CH7 = 100% (≥95% ✓) — second consecutive cycle. CH7 STANDALONE LOCK ELIGIBLE.
- **V5 cycle**: CH7 = 100% (≥95% ✓) — third consecutive cycle. CH7 LOCK CONFIRMED.

**LOCK status: CONFIRMED — CH7 LOCKED AT V5 CLOSE.**

V5 is the LOCK-TRIGGER cycle per V5 dispatch §3: the cohort §3Z LOCK requires all 7 lenses (CH1-CH7) to each independently reach ≥95% × 2 consecutive cycles. CH7's three-consecutive-cycle LOCK at V5 makes it the leading lens of the cohort and discharges one of the 7 lens conditions definitively. The cohort-wide LOCK at V5 close depends on the parallel CH1-CH6 lens trajectories; the V≤5 ceiling per §3Z is reached at V5 exactly, making V5 the binding LOCK-trigger close for the entire T-P1 cohort if all 7 lenses confirm ≥95% × 2 consecutive cycles at V5.

**V5 is the third consecutive ≥95% cycle for CH7 — CH7 LOCK CONFIRMED. LOCK EXTENSION: V3 → V4 → V5 (3-cycle LOCK).**

## Bibliography

- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87` (CH7 lens definition)
- `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md §0-§5` (V5 dispatch context; LOCK-TRIGGER cycle)
- `restart/audit/totality/p1/hardening/V4/CH7.md` (V4 ACCEPT; 9/9 = 100% — second consecutive ≥95% cycle; standalone LOCK-eligible)
- `restart/audit/totality/p1/hardening/V3/CH7.md` (V3 ACCEPT; 9/9 = 100% — first ≥95% cycle)
- `restart/audit/totality/p1/hardening/V2/CH7.md` (V2 REVISE; 6/9 = 66.7%)
- `restart/audit/totality/p1/hardening/V1/CH7.md` (V1 REVISE; 7/9 = 77.8%)
- `restart/audit/totality/p1/1A-substrate-evidence.md` (V4-LOCKED; zero V5 diff)
- `restart/audit/totality/p1/1B-codegen-evidence.md` (V3-LOCKED; zero V4+V5 diff)
- `restart/audit/totality/p1/1C-runtime-evidence.md:21, 23, 40, 92, 124, 162, 201` (V4-LOCKED; zero V5 diff; 127 propagation at 7 sites continues to hold)
- `restart/audit/totality/p1/1D-skinny-lessons.md:117, 140` (V4-LOCKED; zero V5 diff)
- `restart/audit/totality/p1/1E-locks-evidence.md:35, 128-130, 159-168` (V5 amended; F-V5-CH6-1 cosmetic anchor refresh `:126-128` → `:128-130` at 1E:35)
- `restart/audit/totality/p1/1F-anti-pattern.md:69` (V4-LOCKED; zero V5 diff; 27-hit count continues to hold)
- `restart/audit/totality/p1/1F-coherence-scan.md` (V3-LOCKED; zero V4+V5 diff)
- `restart/audit/totality/p1/1F-past-corpora.md` (V3-LOCKED; zero V4+V5 diff)
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` (HEAD; AP-009 `lightningcss_facts` 27-hit count)
- `crates/core/src/runtime/mod.rs:25-71` (HEAD; mechanical extraction 133 raw - 6 in-window neutrals = 127)
- `restart/locks/LOCKS.md` (HEAD; zero CH7/Overfit hits — V2 COH-012 + V3 + V4 + V5 carry-forward verified §2.9)
- HEAD = 9833295d5 (T-P1 V5 atomic cosmetic fold; 1 inventory file (1E) amended over V4 8f4756113 baseline at a single line)

Executable verification commands (re-run at HEAD, V5 cycle):

```
cd /Users/mkbabb/Programming/bbnf-lang
git rev-parse HEAD                                                          # confirm V5 HEAD (expect: 9833295d5)

# F-V5-CH6-1 1E:35 cosmetic anchor refresh + LAC-1E-12 block at new :128-130
awk 'NR>=128 && NR<=130' restart/audit/totality/p1/1E-locks-evidence.md     # expect: LAC-1E-12 heading + governance prose
awk 'NR>=126 && NR<=128' restart/audit/totality/p1/1E-locks-evidence.md     # expect: trailing LAC-1E-16 row + blank + LAC-1E-12 heading
grep -n ":126-128\|:128-130" restart/audit/totality/p1/1E-locks-evidence.md # expect: exactly 1 hit at 1E:35 carrying :128-130
awk 'NR==35' restart/audit/totality/p1/1E-locks-evidence.md | grep -o ":128-130\|:126-128"   # expect: :128-130 (only)

# V4-LOCKED inventories zero-drift verification
git diff 8f4756113 HEAD --stat -- \
    restart/audit/totality/p1/1A-substrate-evidence.md \
    restart/audit/totality/p1/1C-runtime-evidence.md \
    restart/audit/totality/p1/1D-skinny-lessons.md \
    restart/audit/totality/p1/1F-anti-pattern.md                            # expect: empty output

# V3-LOCKED inventories zero-drift verification
git diff 0a9f1288c HEAD --stat -- \
    restart/audit/totality/p1/1B-codegen-evidence.md \
    restart/audit/totality/p1/1F-coherence-scan.md \
    restart/audit/totality/p1/1F-past-corpora.md                            # expect: empty output

# V4 §2.1 carry-forward — AP-009 27 hits + zero "24 hits" orphans
grep -n "lightningcss_facts" skinny/crates/bbnf-bench/src/nonjson_css_l4.rs | wc -l    # expect: 27
grep -nc "24 hits" restart/audit/totality/p1/1F-anti-pattern.md                          # expect: 0
grep -nc "27 hits" restart/audit/totality/p1/1F-anti-pattern.md                          # expect: 1

# V4 §2.2 carry-forward — 127 reexports + NEW-CH2-V3-02 propagation
awk 'NR>=25 && NR<=71' crates/core/src/runtime/mod.rs \
  | python3 -c "import sys, re; t=sys.stdin.read(); print(sum(len([s for s in (m.group(1) if m.group(1) else m.group(2)).split(',') if s.strip()]) for m in re.finditer(r'pub use\s+[\w_:]+(?:::\{([^}]+)\}|::(\w+))', t)))"    # expect: 133 (raw); minus 6 in-window neutrals = 127
grep -nc "127 grammar-named\|127 distinct grammar-named\|\*\*127 distinct" restart/audit/totality/p1/1C-runtime-evidence.md    # expect: 7
grep -nc "126 grammar-named\|126 distinct" restart/audit/totality/p1/1C-runtime-evidence.md                                    # expect: 0

# V4 §2.4 carry-forward — 1A row 100→117 cross-cite
grep -nE ":100[^0-9]|row 100" restart/audit/totality/p1/1A-substrate-evidence.md    # expect: zero hits
grep -c "1D \`:117\`" restart/audit/totality/p1/1A-substrate-evidence.md            # expect: 2

# V4 §2.5 carry-forward — 1E Open Questions table at :159-168
awk 'NR>=159 && NR<=168' restart/audit/totality/p1/1E-locks-evidence.md             # expect: 4 UNKNOWNs with verify_actions

# V4 §2.7 carry-forward — LOCKS.md CH7-binding existence audit
grep -n "CH7\|Overfit" restart/locks/LOCKS.md                                       # expect: zero hits

# V4 §2.8 carry-forward — google_sheets + runtime dir census
find crates/core/src/runtime/google_sheets -type f -name '*.rs' | wc -l             # expect: 10
find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l                # expect: 9
```

All fourteen verifications executed at V5 HEAD (commit 9833295d5); outputs quoted inline at §2.1-§2.10. The V5 cycle CH7 ACCEPT + 3-cycle LOCK confirmation rest on these reproducible witnesses.
