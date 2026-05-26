# SK-V14 CSS L4 Production Corpus Manifest

Date captured: 2026-05-26.
Freshness source: jsDelivr immutable npm version URLs; each response returned
`x-jsd-version-type: version` and `cache-control: public, max-age=31536000,
s-maxage=31536000, immutable`.

## Files

| Corpus | File | Version pin | Source URL | Bytes | SHA-256 | HTTP freshness stamp | License |
|---|---|---|---|---:|---|---|---|
| Bootstrap | `bootstrap-5.3.3.min.css` | `bootstrap@5.3.3` | `https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css` | 232803 | `3c8f27e6009ccfd710a905e6dcf12d0ee3c6f2ac7da05b0572d3e0d12e736fc8` | `date: Tue, 26 May 2026 06:49:24 GMT`; `etag: W/"38d63-xawd7pYctZoEUlbsID9p4xeHL3w"` | MIT |
| Tailwind CSS | `tailwindcss-0.2.0.min.css` | `tailwindcss@0.2.0` | `https://cdn.jsdelivr.net/npm/tailwindcss@0.2.0/dist/tailwind.min.css` | 179631 | `e463dd783548584666e5e50c47c305def32607a9a2dd64e7593908fc1839ee73` | `date: Tue, 26 May 2026 06:49:24 GMT`; `etag: W/"2bdaf-1Kc07jyGtujvJR0UbujA5wWHHlY"` | MIT |
| Material Components Web | `material-components-web-14.0.0.min.css` | `material-components-web@14.0.0` | `https://cdn.jsdelivr.net/npm/material-components-web@14.0.0/dist/material-components-web.min.css` | 495454 | `60f82e183aa0e791c1f3eb5bac905b5ae885f49f9708aeec8ec71a8b014c4f12` | `date: Tue, 26 May 2026 06:49:24 GMT`; `etag: W/"78f5e-DiLfwq4wpH8+fHZWjhQ3TyuO42w"` | MIT |
| Animate.css | `animate-4.1.1.min.css` | `animate.css@4.1.1` | `https://cdn.jsdelivr.net/npm/animate.css@4.1.1/animate.min.css` | 71750 | `5fbaeb9f8e25d7e0143bae61d4b1802c16ce7390b96ceb2d498b0d96ff4c853f` | `date: Tue, 26 May 2026 06:49:24 GMT`; `etag: W/"11846-uB7xsi3iavinpGVvVl+8kaaddRg"` | MIT |

Total byte count: 979638.

## Verification Commands

```sh
wc -c skinny/corpora/css-l4-sk-v14/*.css
shasum -a 256 skinny/corpora/css-l4-sk-v14/*.css
du -sh skinny/corpora/css-l4-sk-v14
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench css_l4_sk_v14_corpora -- --nocapture
```
