# P8 — THE BEHAVIOR PROBES (pass-2 lane, spec-augmented)

Status: **all three probes EXECUTED on real engines; F-3 and F-5 close; VR-64/62
corrected.** Traces: `traces/p8/{p8-f3,p8-f3b,p8-f5,p8-search}-results.json`
(committed beside this doc); screenshots + probe sources in the `virt-p2` clone
(`bench/p8-*.mjs`, `demo/public/f3.html`).

Engines: Playwright 1.58 Chromium + **real WebKit 26.5** (the same engine P2's
find-over-CV probe ran on). Apple Silicon; Chromium arms under CDP 4× CPU throttle
where stated; WebKit unthrottled (no CDP throttle exists — noted per arm). Fleet
hygiene per VR-74: every arm in its own `newContext()`; fresh page per cold-jump run.

---

## 1. F-3 — view-timeline reveals × the CV skipped state → **CLOSED (VR-9 SETTLED, one new law)**

**Fixture** (`demo/public/f3.html`): window scroller (the vft/atlas model, A.4;
`overflow-x: clip` on html/body per the SCROLL-ENGINE-BAN), 45 sections ~1.15Kpx each,
`content-visibility: auto; contain-intrinsic-size: auto 1100px` on every non-lead
section, the Beat register verbatim (`entry 5% → entry 22%`, `linear both`, fade +
6vh lift) plus the `@property --scroll-tl` cover-range scalar. Three binding variants
interleaved — the fork the spec names (wrap-inner vs animate-wrapper):

- **`wrapper`** — Beat.vue verbatim: the CV host declares `view-timeline-name:
  --beat-tl` AND carries the reveal + the `--scroll-tl` arm (two animations, one host axis).
- **`named-inner`** — named timeline on the CV host; the reveal bound on an inner
  child via the named lookup.
- **`inner`** — the anti-candidate: anonymous `animation-timeline: view()` on a child
  INSIDE the skipped subtree.

Arms: `cv=on` vs `cv=off` (control — isolates the CV effect from view-timeline
mechanics). Probes: skip-state audit (`contentvisibilityautostatechange` ledger +
`getAnimations()` on skipped targets), a 500-frame slow-sweep progress-vs-geometry
deviation, terminal-state audit, truly-cold instant deep jumps (fresh page per jump),
post-jump backtrack audit, `--scroll-tl` scalar honesty at 6 stops.

### Findings

**1a. Support matrix — both engines green.** `@supports ((animation-timeline: view())
and (animation-range: entry))`, `content-visibility`, `@property` all pass in Chromium
AND WebKit 26.5 (consistent with R11's Safari 26.0 floor). **Detection trap:**
`contentvisibilityautostatechange` EVENTS FIRE in both engines (45/44 initial skip
events logged) but `'oncontentvisibilityautostatechange' in Element.prototype` is
**false in both** — never feature-detect this event via the `on*` property.

**1b. Timeline liveness while skipped — the variant split (the mechanism):**

| Variant | Chromium (skipped) | WebKit 26.5 (skipped) |
|---|---|---|
| wrapper | ViewTimeline ACTIVE (`currentTime` −2183%), from-frame applied | ACTIVE (−2191%), from-frame applied |
| named-inner | ACTIVE (−2233%), from-frame applied | ACTIVE (−2306%), from-frame applied |
| inner (anonymous) | **timeline INACTIVE** — `currentTime: null`, `fill: both` does NOT apply, element reads **base styles** (opacity 1) | nominally active but see 1c — resolves LATE |

**1c. Sweep — do reveals fire on schedule from the skipped state?** Deviation =
|measured opacity − geometric expected| sampled mid-entry (mean / max):

| Variant | Chromium cv=on | Chromium cv=off | WebKit cv=on | WebKit cv=off |
|---|---|---|---|---|
| wrapper | 0.127 / 0.247 | 0.137 / 0.240 | 0.148 / 0.246 | 0.138 / 0.240 |
| named-inner | **0.000 / 0.000** | 0.000 / 0.000 | **0.000 / 0.000** | 0.000 / 0.000 |
| inner | 0.212 / 0.241 | 0.239 / 0.241 | **0.442 / 0.945** | 0.237 / 0.241 |

- **wrapper + named-inner: cv=on ≡ cv=off in both engines → reveals fire ON SCHEDULE
  from the skipped state.** Beat.vue's un-evidenced comment ("a skipped beat un-skips
  before it enters the viewport, so `--beat-tl` resolves and the fade+lift fires on
  schedule") is now MEASURED TRUE — for its own binding shape.
- **inner is the broken shape.** WebKit cv=on mean deviation 0.442, max 0.945 vs its
  own 0.237 control: sections visibly enter at ~opacity 0 and catch up late; the
  backtrack audit still read 0.781 vs expected 1.0 after a 500ms settle. Chromium's
  inactive-while-skipped anonymous timeline re-activates on unskip with only transient
  lag (one cold-jump viewport sample 0.217 vs 0.458), but the `fill: both` guarantee is
  silently void while skipped.
- The wrapper rows' apparent ~0.13 deviation is **probe bias, not engine error**: the
  reveal's own `translate3d` lift shifts `getBoundingClientRect()` while view-progress
  is computed from the un-transformed layout box — identical with CV off, and the
  untransformed-subject variant reads 0.000. Corollary worth keeping: **view()
  progress ignores the subject's own animated transform → no feedback loop when
  animating the timeline subject.** Beat's model is stable by construction.

**1d. Cold deep jumps (the deep-link/search shape; fresh page, target verified
skipped).** All three variants, both engines: correct within **0–1 frames** of the
instant jump; `fill: both` holds fully-entered neighbors at terminal opacity 1; zero
terminal violations after a full sweep; backtrack audits clean except the WebKit
`inner` residual above. The catastrophic "arrive mid-document over invisible
opacity-0 content" failure **does not occur in either engine** for the host-bound
variants.

**1e. `--scroll-tl` (the atlas H11 scalar) is EXACT over CV.** At every stop, both
engines: expected 0.15/0.40/0.50/0.60/0.85 → read 0.1500/0.3997/0.4997/0.5997/0.8495;
clamps to 0/1 at the boundaries; reads the `@property` initial 0 while skipped (the
JS reader treats a skipped host's 0 as "below the fold", which it is).

### Verdict + the vft reveal-motion guidance (the F-3 output)

**VR-9 flips OPEN → SETTLED.** Scroll-driven reveals compose with `content-visibility:
auto` in both engines UNDER ONE LAW — the timeline lives on the CV host:

1. **Declare the ViewTimeline ON the element that carries `content-visibility: auto`**
   (`view-timeline-name` on the section/beat host). The reveal target may be that host
   itself (Beat's shape) or any inner element bound via the NAMED timeline.
2. **Never mint an anonymous `animation-timeline: view()` on a descendant of a CV
   box** — inactive-while-skipped in Chromium (fill silently unbinds; content shows
   base styles), late-firing in WebKit 26.5 (visible opacity-0 entries, deviation to
   0.945). This is the wrap-inner vs animate-wrapper fork's answer: **animate-wrapper
   (or named-inner) wins; anonymous wrap-inner is banned.**
3. Entry-anchored ranges + `fill: both` survive instant deep links from the cold
   skipped state (0–1 frames to correct) — vft deep links need no reveal-side
   special-casing.
4. The `@supports ((animation-timeline: view()) and (animation-range: entry))` +
   PRM outer fence (scroll-driven.css) stand unchanged; floors per R11 (Safari 26.0,
   threaded 26.4).
5. vft's `--scroll-tl`-style scrub scalars are safe over CV (1e) — declare them on the
   same host, same named timeline.

---

## 2. F-5 — the activeId mechanism A/B → **CLOSED (offsetTop layout-read wins)**

**Fixture:** the virt-p2 stress demo `?mode=native` (220 sections, CV/CIS substrate,
element scroller). **Load:** Chromium, CDP 4× throttle, 500 frames down + 250 up,
**remeasure storms** every 40 frames (8 pseudo-random sections ±120px → RO fires,
anchoring responds; 172 invalidations absorbed over the run). **Canon** (VR-49):
deepest-visible-wins = last section in document order with top ≤ the 80px line;
closest-below fallback at the top. **Ground truth:** synchronous full
`getBoundingClientRect` sweep every 5th frame + 6 settle stops. One fresh
context per mechanism.

| Metric | A — offsetTop table + binary search (RO-invalidated) | B — IntersectionObserver-primary (no layout reads) |
|---|---|---|
| transient mismatches (150 samples) | **0 (0.0%)** | 34 (**22.7%**), lag always exactly 1 section |
| steady-state at 6 stops | 6/6 correct | 6/6 correct |
| avg frame ms | 8.84 | 9.01 |
| p95 frame ms | 16.3 | 18.6 |
| frames >33ms / 750 | 2 | 0 |
| longtasks (whole run) | 113, 57ms | 112ms |
| table rebuilds / IO callbacks | 172 rebuilds | 552 callbacks |

**Verdict — the mechanism arm of VR-49 settles on the offsetTop layout-read:** it
reproduces the deepest-visible canon EXACTLY under fast scroll + remeasure storms
(zero transient mismatches — the binary search reads fresh layout every rAF), at
jank statistically indistinguishable from IO at O(200) scale under 4× throttle
(Δavg 0.17ms; the 2 long frames are storm-coincident rebuilds, both <60ms). IO's
one-section stale-rect lag on ~23% of scrolling samples is the visible
TOC-highlight trail R10 worried about, and it buys no measurable frame savings.
**A.7's residual closes: offsetTop-primary is the ONE authority; IO demotes to an
optional in-DOM precision enhancement** (VR-53's "keeping IO as in-DOM precision
enhancement" tightens to "optional, never authoritative").

Residuals (named, low-risk): the A/B ran on the element scroller; the window-scroller
(vft) arm shares the same primitives via the ViewportAdapter seam (VR-40) — untested
here. WebKit ran no A/B (no CDP throttle); the canon is engine-independent and the
mechanism uses only offsetTop/scrollTop.

---

## 3. Search navigation into CV content (R13) → **VR-64 corrected, VR-62 extended**

**Fixture:** stress demo, target `sec-200` (`NEEDLE-S200`), cold from the top.
Arms per engine (fresh context each): (1) native + immediate `scrollIntoView`;
(2) native + settle loop (rAF correction until |err| ≤ 1px); (3) governed
(`idle=off`) + `await revealTo("sec-200")` → `scrollIntoView`. `scroll-margin-top:
80px` injected (the theme.css:197 discipline); landing error = rect.top − 80.

| Arm | Chromium 4× | WebKit 26.5 |
|---|---|---|
| immediate | **0.3px at frame 0**, drifts to 0.6px, 0 corrections; unskip 1.4–1.9ms, ONE cv event | frame 0 −0.1px, **frame 1 +257.4px, rests there FOREVER** |
| settle loop | identical (loop is a no-op) | **1 correction at frame 1 → 0.4px stable at frame 2** |
| governed revealTo | revealTo 387ms (4×), then 0.3→0.6px | revealTo 436ms, then **257.4px permanently** (no loop ran) |

**3a. VR-64 correction #1 — the settle-aware scrollTo is MANDATORY for search nav,
and arrival detection must survive a correct first frame.** WebKit's 257px
displacement (the known VR-23 number, now reproduced in the search shape) arrives
one frame AFTER an apparently-perfect landing: frame 0 reads −0.1px, frame 1 reads
+257.4px (the unskip reflow above the target), and it never self-heals. Any
implementation that samples once and declares success ships a 257px miss on stable
Safari. VR-23's epoch-gated `whenSettled` + unconditional final commit is exactly
the required shape; VR-64's "if eager/fully-revealed: `scrollTo(id)` directly" must
read "`scrollTo(id)` **via the VR-23 settle-aware contract** in ALL cases" — the
plain-`scrollIntoView` shortcut is Chromium-only behavior.

**3b. VR-64 correction #2 — the governed ordering.** Under the mount governor the
match target **does not exist in the DOM pre-reveal** (`getElementById` null): the
match-highlight Range can only be constructed AFTER `revealTo` resolves. Under CV
(native) the Range IS constructible over skipped content pre-navigation. The
`selectResult` contract gains one sentence: *highlight registration follows
`revealTo` (governed) or may precede the scroll (CV); re-setting the Highlight after
`whenSettled` is idempotent and covers both.*

**3c. VR-62 extension — Ranges over skipped content, both engines.** TreeWalker +
`document.createRange` over a CV-SKIPPED section work in Chromium AND WebKit 26.5;
`CSS.highlights.set` accepts the skipped-content Range in both. Engine divergence:
`range.getBoundingClientRect()` while skipped reads the real rect in Chromium
(113×22) but **degenerate 0×0 in WebKit** — never gate on rect queries against
skipped content. After landing, the highlight PAINTS in both engines (in-viewport
rects 113–114×21–22; visual artifact `p8-search-chromium-immediate.png` shows the
painted mark on formerly-skipped content). Zero DOM mutation throughout — the
VR-62 path holds end-to-end.

**3d. Auto-reveal timing (the R13 question).** `scrollIntoView` into skipped content
un-skips the target in **1.4–1.9ms (Chromium 4×) / 5–6ms (WebKit)** — one
`contentvisibilityautostatechange` event, content painted the same frame. The
"reveal" step is free under CV; the cost center is exclusively the WebKit post-unskip
reflow displacement (3a), not the reveal.

---

## 4. THE VR DELTAS (the lane's return)

- **VR-9 → SETTLED** (F-3 closed). Evidence: `traces/p8/p8-f3{,b}-results.json`.
- **NEW VR-75 [SETTLED] — the timeline-host law.** Every scroll-driven animation on
  CV-managed content declares its ViewTimeline ON the CV host (named
  `view-timeline-name`); reveal targets bind via the named timeline (host or
  descendant). Anonymous `view()` on a descendant of a `content-visibility: auto` box
  is BANNED (Chromium: timeline inactive while skipped, `fill` void; WebKit 26.5:
  late-firing, deviation to 0.945). Feature-detect the skip-state event by
  listening, never via `'oncontentvisibilityautostatechange' in Element.prototype`
  (false in both engines while the event fires in both). Owner: glass-ui
  `./virtual` docs + the vft binding doc (G-l); the atlas Beat.vue model is the
  reference implementation and needs NO change.
- **VR-49 mechanism arm → SETTLED** (F-5 closed): offsetTop-table + binary-search,
  rAF-coalesced, RO/width-epoch-invalidated, is the ONE activeId authority; IO is
  optional in-DOM enhancement, never authoritative (VR-53 tightens accordingly).
  Evidence: `traces/p8/p8-f5-results.json` (0% vs 22.7% transient mismatch at
  indistinguishable jank, 4×, storms).
- **VR-64 [corrected]**: all result-nav scrolls go through the VR-23 settle-aware
  contract (WebKit's +257px displacement arrives at frame 1 AFTER a correct frame-0
  read; one correction heals it; without it the miss is permanent). Governed path:
  highlight-Range construction follows `revealTo`; CV path may build pre-scroll;
  idempotent re-set after `whenSettled` covers both.
- **VR-62 [extended]**: skipped-content Ranges constructible + registrable in both
  engines; rect queries on skipped Ranges are engine-divergent (WK 0×0) — no gating
  on them; painted-after-reveal verified visually.
- **F-3, F-5 leave §5; F-1, F-7, F-9 remain the open set.**

## 5. Honesty ledger

- The F-3 sweep's expected-progress model is biased for transformed subjects (~0.13
  mean on the wrapper variant); the named-inner 0.000 control and the cv-on≡cv-off
  identity carry the conclusion, not the wrapper's absolute deviation.
- The first jump probe ran warm (sections unskipped by the preceding sweep);
  `p8-f3b` re-ran every jump cold on fresh pages — cold numbers are the cited ones.
- F-3 ran unthrottled (a correctness probe, not a budget probe); F-5/search Chromium
  arms ran at 4×; WebKit cannot be CPU-throttled via Playwright.
- F-5's A/B is Chromium-only and element-scroller-only; the window-scroller arm and
  a WebKit correctness spot-check are one-day W1 residuals, not blockers.
- `p8-search` measured `revealTo` on the demo's probe governor (batch 4×4/frame),
  not the final VR-13 implementation — the 387–441ms is a shape reference, not a gate.
