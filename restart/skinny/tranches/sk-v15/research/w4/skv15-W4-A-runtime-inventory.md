# SK-V15 W4-A Runtime Inventory

Scope: the Pattern H root runtime surface named by W4:
`crates/core/src/runtime/**` with `find -mindepth 2 -type f -name '*.rs'`.

## Result

The current Pattern H set is exactly 67 files. The full runtime tree has 75
Rust files; the eight shared/root files are outside the W4 Pattern H count.

Line-1 provenance state:

- `0/67` files have a line-1 generated/provenance header.
- `67/67` files start with Rust code.
- `57` files start with `use ...`.
- `10` files start with `pub mod ...`.

Current dirty state inside the 67:

```text
bbnf: 8 files, 6 dirty
bnf: 7 files, 3 dirty
css_l4: 7 files, 7 dirty
css_pretty: 7 files, 4 dirty
csv: 7 files, 3 dirty
ebnf: 7 files, 3 dirty
google_sheets: 10 files, 7 dirty
json: 7 files, 4 dirty
math: 7 files, 3 dirty
```

No Pattern H files are staged. Existing dirty changes are an unknown baseline
until W4 redress proves generator ownership over the final bytes.

## Required Inventory Commands

```sh
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l

find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' -print0 \
  | sort -z \
  | xargs -0 awk 'FNR==1 { print FILENAME ":1:" $0 }'
```

The `-maxdepth 2` form is invalid for this gate because it misses the
`google_sheets/document/*` files.
