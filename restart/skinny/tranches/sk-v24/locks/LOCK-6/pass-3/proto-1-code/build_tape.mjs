// HARNESS SETUP (untimed, not the decoder-under-test): reconstruct the four SoA
// tape planes from twitter.json bytes, faithful to skinny's Rust offset-tape
// builder (one offset per structural/value token; separators ',' ':' get NO
// offset; strings containing a '\' escape are flagged STRING_NEEDS_DECODE=0x01).
// This is the exact plane contract the Rust `tape_dump.rs` writes; here it is
// rebuilt in JS so the probe needs no skinny build. The DECODER walking these
// planes is the generated artifact under test.
import { readFileSync } from "node:fs";

const WS = new Uint8Array(256);
for (const c of [0x20, 0x09, 0x0a, 0x0d]) WS[c] = 1;
const NUMDELIM = new Uint8Array(256);
for (const c of [0x2c, 0x5d, 0x7d, 0x20, 0x09, 0x0a, 0x0d]) NUMDELIM[c] = 1;

export function buildTape(path) {
  const source = readFileSync(path); // Node Buffer
  const n = source.length;
  const offsets = [];
  const flagCursors = [];
  const flagValues = [];
  let i = 0;
  while (i < n) {
    const c = source[i];
    if (WS[c]) { i++; continue; }
    if (c === 0x2c || c === 0x3a) { i++; continue; } // ',' ':' -> no offset
    if (c === 0x7b || c === 0x7d || c === 0x5b || c === 0x5d) {
      offsets.push(i); i++; continue; // { } [ ]
    }
    if (c === 0x22) { // string: opening quote is the offset
      const cursor = offsets.length;
      offsets.push(i);
      let j = i + 1;
      let hasEscape = false;
      for (;;) {
        const b = source[j];
        if (b === 0x22) break;
        if (b === 0x5c) { hasEscape = true; j += 2; continue; }
        j += 1;
      }
      if (hasEscape) { flagCursors.push(cursor); flagValues.push(0x01); }
      i = j + 1; // past closing quote
      continue;
    }
    if (c === 0x74) { offsets.push(i); i += 4; continue; } // true
    if (c === 0x66) { offsets.push(i); i += 5; continue; } // false
    if (c === 0x6e) { offsets.push(i); i += 4; continue; } // null
    // number: '-' or digit
    offsets.push(i);
    let j = i;
    while (j < n && !NUMDELIM[source[j]]) j += 1;
    i = j;
  }
  return {
    source,
    offsets: Uint32Array.from(offsets),
    flagCursors: Uint32Array.from(flagCursors),
    flagValues: Uint8Array.from(flagValues),
  };
}
