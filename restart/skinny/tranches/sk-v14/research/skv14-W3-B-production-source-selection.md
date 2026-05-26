# SK-V14 W3-B: Production Source Selection

Date: 2026-05-26.
Scope: Bootstrap, Tailwind, Material, and Animate source candidates.
Output: this file.

## Section 1 - Findings

The selected corpus files are pinned npm package assets served through immutable
jsDelivr version URLs:

| Family | Version pin | File | Bytes |
|---|---|---|---:|
| Bootstrap | `bootstrap@5.3.3` | `bootstrap.min.css` | 232803 |
| Tailwind CSS | `tailwindcss@0.2.0` | `tailwind.min.css` | 179631 |
| Material Components Web | `material-components-web@14.0.0` | `material-components-web.min.css` | 495454 |
| Animate.css | `animate.css@4.1.1` | `animate.min.css` | 71750 |

Total: 979638 bytes, approximately 957 KiB. This clears the 800 KiB floor and
stays close to the W3 approximate 960 KB target without padding.

Tailwind 2.x assets are multi-megabyte files; Tailwind 0.2.0 is still a real
released production package and keeps W3 near the target. The version pin is
explicit and immutable.

## Section 2 - Recommendations

Stage the four files under `skinny/corpora/css-l4-sk-v14/` with filenames that
include the package version. Record URL, version pin, byte count, SHA-256, HTTP
freshness stamp, and MIT license in `manifest.md`.

## Section 3 - Risks

Using latest or unpinned package URLs would make the corpus unstable. Using
Tailwind 2.x would exceed the approximate target by several megabytes. Using
only tiny research fixtures would reopen P-3.

## Section 4 - Sources

- `https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css`
- `https://cdn.jsdelivr.net/npm/tailwindcss@0.2.0/dist/tailwind.min.css`
- `https://cdn.jsdelivr.net/npm/material-components-web@14.0.0/dist/material-components-web.min.css`
- `https://cdn.jsdelivr.net/npm/animate.css@4.1.1/animate.min.css`
