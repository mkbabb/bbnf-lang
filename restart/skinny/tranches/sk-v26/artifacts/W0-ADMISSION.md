# W0 admission

Disposition: `ADMITTED_GIT_DURABLE`.

Selected commits:

- BBNF base `af15f63e0d2d3d719938c13b906a50acbb92ea3b`;
- BBNF W0 payload `47d51a190fda7f51686d5bad1be149191bbfeba6`,
  tree `00af42256deb5a49d5c1ccf1e9cbef3107cc6589`;
- parse-that base `ef10d5b78236c4a30a7bb22a6113b60bdc4bdf42`;
- parse-that W0 payload `627c9b46a773a278ed8c60728c9d335be6747905`,
  tree `b0144769472a972bcd4de8f3e67ffcfe264cef99`;
- baseline-only pprint pin `2b4d2d48657ebc4eb4c23297929ad3532e3eb6b4`,
  tree `93614138642f48b156ba02dbee4673db649632e9`.

The hostile W0 review initially rejected admission. Its blocking findings were
resolved as follows:

1. git-object overlay and receipt are generated and verify;
2. every protected dirty row has explicit owner/provenance, including honest
   per-path owner attribution limits;
3. SK-V25 and parse-that T/U/B receive terminal classifications;
4. semantic planes and exact executable/reference pins are explicit;
5. authority lint scans dispatch-shaped language rather than counting only
   authority rows;
6. both execution payload commits and the pprint dependency pin are clean;
7. the dirty pprint main is replaced by a clean detached dependency worktree;
8. thirteen tracked content-addressed inputs materialize deterministically into
   canonical BBNF and parse-that paths;
9. held-out receipt bytes, ordering, preimage encoding, and selection arithmetic
   are exact;
10. audit replay reconstructs from an immutable row epoch and does not overwrite
    evidence from moving live logs;
11. 3,928 crossing sites across 147 files have mechanical dispositions;
12. 106 session identities and 109 tranche roots remain explicitly qualified
    censuses, not inflated semantic-adjudication claims.

Verification:

- formation receipt: 14/14;
- W0 overlay: 63 paths,
  SHA-256 `51847260d21980af9b04cc2846bf3177bb152a9b9a2edf6c6f651ea069f231c5`;
- W0 receipt SHA-256
  `47422fc0c9dfcf281e4eed2da664899325608235930ae93b4ebee3e59785acb5`;
- TypeScript: 13 files, 122 tests green after materialization;
- Rust library: green; full committed tests remain RED only on the nonexistent
  legacy `parse_that::regex::parser`;
- BBNF library: green with the pre-existing 190 warnings;
- BBNF materialized JSON bench: all four inputs execute; samples are
  non-certifying.

W1 is admitted only for the selected independent runtime kernel and its
born-RED laws. Full BBNF cutover remains blocked until that package is sealed.
