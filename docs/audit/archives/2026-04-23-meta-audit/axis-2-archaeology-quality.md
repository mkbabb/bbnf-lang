# Axis 2 — Archaeology Quality

Audit target:

- Primary documents: `docs/tranches/meta-audit/06-commit-archaeology.md` and `docs/tranches/meta-audit/archaeology/era-II-foundations.md` through `era-VI-restart.md`.
- Live repo state checked at `HEAD 5a260f94`.
- Historical archaeology snapshot anchored at `48e6eaa9`, because the audited archaeology was written against that master state.

Method:

- Verified live git counts with `git rev-list`, `git log`, `git for-each-ref`, `git diff --numstat`, and `git show -s`.
- Treated claims that were accurate at `48e6eaa9` but are stale at `5a260f94` as **Refined**, not **Flawed**.
- Treated claims as **Flawed** only where the archaeology states a concrete fact that the cited method does not support, or where the number cannot be reproduced from the named evidence.

## Verified

### 1. The archaeology's master-head counts were correct at its own close

`06-commit-archaeology.md` opens with `1,842` master commits and `945` unpushed commits at lines 3-4 and 28-30. Those numbers are historically correct for the audited snapshot:

```text
$ git rev-list --count 48e6eaa9
1842

$ git rev-list --count origin/master..48e6eaa9
945
```

This means the headline frame in [06-commit-archaeology.md](../../tranches/meta-audit/06-commit-archaeology.md) was not invented; it was simply pinned to the 2026-04-22 close state rather than live `HEAD`.

### 2. The tranche-process milestone SHAs are well grounded

The archaeology's named process milestones check out:

```text
$ git show -s --format='%h %ad %s' --date=short 13411847 536ac07c 5281ec23
13411847 2026-04-10 docs(tranche): add Tranche AA plan + baseline cleanup
536ac07c 2026-04-13 docs: tranche directory structure + crate ownership + AS PROGRESS.md
5281ec23 2026-04-15 docs(AU): FINAL.md + post-AU.json — tranche AU close-out
```

That supports the core tranche-evolution story in [06-commit-archaeology.md](../../tranches/meta-audit/06-commit-archaeology.md): plan-doc surface at AA, `PROGRESS.md` discipline at AS, `FINAL.md` discipline at AU.

### 3. Sample era-boundary SHAs are defensible

Spot-checking the named first or opening SHAs per era:

```text
$ git show -s --format='%h %ad %s' --date=iso-strict-local \
    cc499979 ba07ffc7 a3fadf56 13411847 ca0875eb 4869e715
cc499979 2026-02-26T21:09:35-05:00 refactor: restructure as bbnf-lang monorepo
ba07ffc7 2026-03-15T01:51:35-04:00 docs: update bbnf-lang CLAUDE.md and README.md for IR architecture and new exports
a3fadf56 2026-04-08T21:09:53-04:00 refactor(backend): pre-solve delim_scan + key_dispatch per-grammar (Tranche F)
13411847 2026-04-10T16:25:51-04:00 docs(tranche): add Tranche AA plan + baseline cleanup
ca0875eb 2026-04-15T02:01:06-04:00 docs(AV): add Flattening tranche plan with wave schedule and research artefacts
4869e715 2026-04-20T21:41:14-04:00 build(b0): land ax-iter cargo alias fast-path (B0.W0.a)
```

This supports the broad six-era frame:

- Era II opens on the monorepo/IR foundation.
- Era III's first tranche-tagged commit is `a3fadf56`.
- Era IV's first true tranche-plan doc is `13411847`.
- Era V opens with AV planning.
- Era VI's restart phase is visible by B0 and then AY/B1.

### 4. The AU baseline numbers are correct

The archaeology repeatedly treats AU as the measurement anchor. The key baseline numbers do match the committed benchmark artefact:

```text
$ jq-style extraction from docs/benchmarks/post-AU.json
canada 1231
citm 2438
twitter 1967
normalize 735
tailwind 496
parse_simple 95
```

These numbers match the AU narrative in [era-IV-tape-first.md](../../tranches/meta-audit/archaeology/era-IV-tape-first.md) and the authoritative artefact in [post-AU.json](../../benchmarks/post-AU.json) / [AU/FINAL.md](../../tranches/AU/FINAL.md). On this point the archaeology is solid.

### 5. The unpushed-branch narrative is directionally right

The current repo still shows the same general topology the archaeology describes:

- `master` is far ahead of `origin/master`.
- There are many surviving provenance branches.
- Non-worktree feature branches carry only a modest number of commits not on `master`.

Current live evidence:

```text
$ git log origin/master..HEAD --oneline | wc -l
991

$ python branch audit
non_worktree_unique_branches 22
sum 36
```

That does not exactly reproduce the archaeology's `24 feature branches / ~35 commits not on master`, but it strongly supports the qualitative claim in [06-commit-archaeology.md:489-504](../../tranches/meta-audit/06-commit-archaeology.md) and [era-VI-restart.md:225-229](../../tranches/meta-audit/archaeology/era-VI-restart.md): the side branches are mostly provenance, not a second hidden codebase.

### 6. The top three reversal-commits are defensible as a ranking of architectural reversals

The archaeology's closing judgment in [06-commit-archaeology.md:622-629](../../tranches/meta-audit/06-commit-archaeology.md) is not absurdly cherry-picked. If the criterion is "largest reversals of previously-declared architectural direction," then these three do belong at the top:

- `2f7c1bd4` (AQ.5): deleted the orthogonal `EmissionTier` / structural-dispatch path.
- `a206b962` within AX.W0b: deleted the DTA walker/runtime path.
- AY-I.W1: reverted the seven-column tape split.

Those are the three clearest "we built the wrong substrate, then reversed it" moments in the tranche era.

## Refined

### 1. The headline counts are now stale, not wrong

At live `HEAD 5a260f94`, the repo has moved:

```text
$ git log master --oneline | wc -l
1888

$ git log origin/master..HEAD --oneline | wc -l
991

$ git rev-list --all --count
1965
```

So [06-commit-archaeology.md:28-32](../../tranches/meta-audit/06-commit-archaeology.md) should now be read as a historical snapshot of `48e6eaa9`, not as live truth.

### 2. The era summary table mixes counting methods and should say so explicitly

The era summary in [06-commit-archaeology.md:43-50](../../tranches/meta-audit/06-commit-archaeology.md) presents a single `Commits` column, but the underlying methods are not uniform:

- Era II claims a date-window count.
- Era IV-VI use tranche-ledger counts and tranche-document counts.
- Era III appears to be a conceptual or ledger count, not a raw git count.

That is survivable, but the document should say "date-window raw count" vs "tranche-ledger count" explicitly. Without that distinction, the table reads as if every row came from the same query, which it did not.

### 3. Several Part B dates are right in ordering but off by one in local time

The SHA references are correct, but some dates in [06-commit-archaeology.md:151-155](../../tranches/meta-audit/06-commit-archaeology.md) are one day off in the local timezone:

```text
$ git show -s --format='%h %ad %s' --date=iso-strict-local \
    c1e86ab3 bc550d2c a206b962 0adabb23
c1e86ab3 2026-04-18T06:02:25-04:00 ...
bc550d2c 2026-04-19T11:59:34-04:00 ...
a206b962 2026-04-19T12:00:56-04:00 ...
0adabb23 2026-04-19T12:44:20-04:00 ...
```

The archaeology records these as `2026-04-17` and `2026-04-20`. If the author intended UTC-normalized dating, that needs to be stated; otherwise the doc is locally off by a day.

### 4. The branch analysis should distinguish "non-worktree feature branches" from all local heads

[06-commit-archaeology.md:31](../../tranches/meta-audit/06-commit-archaeology.md) says `Feature branches ahead of master | 24`, while [era-VI-restart.md:225-229](../../tranches/meta-audit/archaeology/era-VI-restart.md) implicitly means non-worktree provenance branches.

Live repo topology now is:

- `54` non-master local heads total.
- `41` heads ahead of `master`.
- `22` non-worktree heads with commits not on `master`.
- `36` non-worktree unique commits not on `master`.

So the archaeology's intent is sensible, but the headline should have named the category precisely.

### 5. The "most valuable commits" conclusion is a reversal ranking, not a total-importance ranking

[06-commit-archaeology.md:622-629](../../tranches/meta-audit/06-commit-archaeology.md) presents AQ.5, AU.4.2, AX.W0b, and AY-I.W1 as the four "most architecturally valuable commits." That is defensible if the ranking means "cleanest high-value reversals."

It is more selection-biased if read as "most important commits overall," because that broader list would also have to consider:

- `536ac07c` for `PROGRESS.md` discipline,
- `5281ec23` for `FINAL.md` / benchmark-baseline canon,
- `4869e715` for the public fast-path B0 surface,
- `3429aaba` for reverting the hand-coded AX.W1 adapters.

The archaeology's conclusion should therefore be read narrowly: it ranked reversals, not overall project leverage.

## Flawed

### 1. Era II's `~264 commits` claim is incorrect by the document's own stated method

The flaw is explicit in [era-II-foundations.md:9-10](../../tranches/meta-audit/archaeology/era-II-foundations.md), which says:

> Commit count: roughly 264 commits across the 18-day span ... per `git log --all --date=short` with the date window filter.

Live git does not support that:

```text
$ git log --all --since='2026-02-26 00:00' --until='2026-03-15 23:59:59' --oneline | wc -l
94
```

This is not a small miss. The document overstates Era II by roughly 2.8× while citing a query that returns a materially smaller number. That makes the Era II commit-volume claim incorrect, not merely approximate.

### 2. AX.W0b's reclaim figure is inconsistent and not reproducible as stated

The archaeology uses several incompatible figures:

- [06-commit-archaeology.md:153](../../tranches/meta-audit/06-commit-archaeology.md) calls `bc550d2c` the first commit of the `~78K-LOC` deletion.
- [06-commit-archaeology.md:329](../../tranches/meta-audit/06-commit-archaeology.md) says `~78,500 LOC reclaim at AX.W0b`.
- [era-V-dta-psi-rut.md:196](../../tranches/meta-audit/archaeology/era-V-dta-psi-rut.md) calls `~78,500 LOC reclaim` the AX proposition.
- [era-V-dta-psi-rut.md:246-247](../../tranches/meta-audit/archaeology/era-V-dta-psi-rut.md) says `~78K LOC reclaim`.
- [AX/FINAL.md:97](../../tranches/AX/FINAL.md) says `~85K LOC interpreter machinery deleted`.

Live diff stats do not resolve to one obvious `~78K` figure:

```text
$ git diff --numstat bc550d2c^..0adabb23
deleted 94566
added   8275
net_deleted 86291

$ git show --numstat a206b962
deleted 5302
added      5
net_deleted 5297

$ git show --numstat bc550d2c
deleted 76442
added   7686
net_deleted 68756
```

The archaeology is therefore mixing at least three different notions:

- gross deletions in `bc550d2c`,
- gross or net deletions across the whole W0b cluster,
- the plan-time target from AX.

Because the metric is never defined, the reclaim number is not reproducible from the evidence it cites.

### 3. The "Era II through Era VI commit counts" are not consistently auditable from the surface presented

This becomes a flaw where the documents imply raw-git precision they do not actually provide. In [06-commit-archaeology.md:43-50](../../tranches/meta-audit/06-commit-archaeology.md), the `Commits` column reads as a uniform, queryable fact. It is not.

Evidence:

```text
$ git log --all --since='2026-03-16 00:00' --until='2026-04-09 23:59:59' --oneline | wc -l
461

$ git log --all --since='2026-04-10 00:00' --until='2026-04-15 23:59:59' --oneline | wc -l
490

$ git log --all --since='2026-04-15 00:00' --until='2026-04-19 23:59:59' --oneline | wc -l
727
```

The archaeology instead reports `~280`, `~185 tranche-tagged`, and `~572 tranche-tagged`. Those may be conceptually valid tranche-ledger counts, but the table does not label them that way. The presentation implies a common measurement surface that is not actually present.

## Open

### 1. The `1,923 commit events across all refs` figure could not be fully reconstructed from live refs

[06-commit-archaeology.md:3-5](../../tranches/meta-audit/06-commit-archaeology.md) reports `1,923 commit events across all refs`. Live `--all` count is now `1965`, but that is after 33 more master commits and at least one new meta-audit branch.

I could verify the master-side historical counts at `48e6eaa9`, but I could not reconstruct the exact historical ref topology needed to reproduce `1,923` precisely from the current repo alone.

### 2. The `24 feature branches / ~35 commits not on master` claim is plausible, but only approximately reproducible now

Current live evidence is close:

- `22` non-worktree heads with unique commits not on `master`,
- `36` non-worktree unique commits not on `master`.

That strongly suggests the archaeology was in the right neighborhood, but I cannot prove the exact historical `24 / ~35` pair without a ref snapshot from 2026-04-22.

### 3. The `~572 tranche-tagged commits` for Era V is internally coherent but not independently reproduced from git alone

The arithmetic inside [era-V-dta-psi-rut.md:12-14](../../tranches/meta-audit/archaeology/era-V-dta-psi-rut.md) is self-consistent:

- AV 53
- AW-I ~45
- AW-II 40
- AW-III 93
- AW-IV 92
- AW-V 80
- AX 169
- total `~572`

What I could not do from the current evidence surface alone was independently rebuild every one of those tranche counts from a single git query, because the commit subjects do not uniformly preserve tranche tags and the doc does not cite per-tranche range manifests.

### 4. The broader brief's `2,787 total distinct events` was not auditable from the assigned archaeology surface

That figure is part of the broader meta-audit framing, not a claim made in `06-commit-archaeology.md` or the era deep dives themselves. I did not find a supporting derivation for `2,787` in the assigned Axis 2 source documents, so I cannot validate or reject it here.

## Axis 2 verdict

The archaeology is strong where it stays close to committed tranche artefacts: named SHAs, AU baseline numbers, tranche-process milestones, and the broad "Era V built substrate faster than it activated consumers" narrative. It is weakest where it presents aggregate counts as if they came from a single precise method. The most important corrections before treating this as canonical are:

1. fix Era II's commit count,
2. define the AX.W0b LOC-reclaim metric once and use it consistently,
3. label every era-count row with its method,
4. make the branch-count category explicit,
5. normalize the off-by-one date labels if local-time dating is intended.

The archaeology is decision-useful after those corrections. It is not yet numerically clean enough to be treated as mechanically authoritative in its current form.
