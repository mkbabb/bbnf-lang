# SK-V15 W4-D Dirty Tree Classification

The W4 owner tree is dirty at entry.

Observed root runtime state:

- `40` tracked files modified under `crates/core/src/runtime`.
- Pattern H still has exactly `67` `.rs` files.
- No adds, deletes, or renames inside the 67.
- Diff stat over root runtime plus root xtask entry files is broad:
  `614 insertions, 743 deletions` at research time.

The sampled runtime diffs look mechanical: import reordering, line wrapping,
enum/struct variant expansion, match-arm wrapping, and indentation changes.
They do not add line-1 generated provenance and they touch only `40/67` Pattern
H files, so they are not W4-complete.

`xtask/src/main.rs` and `xtask/src/regen_simple_runtime.rs` are already dirty
from import reordering only. Treat them as unknown/user baseline until W4
either leaves them alone or explicitly owns subsequent generator/check edits.

## Staging Rule

Do not stage the current dirty root runtime set as W4 output. W4 staging is
allowed only for:

- generator/check source changes in the W4 owner paths;
- generated runtime bytes emitted by those generators;
- W4 research, plan, challenge, and redress artifacts.

Unrelated dirty files outside W4 remain untouched and unstaged.
