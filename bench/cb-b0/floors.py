#!/usr/bin/env python3
"""Proposed floors from A/A and SLOW cells (sk-v27 LW2.CON-2, S11 BENCH).

For each family that has an A/A cell: A/A noise = the larger magnitude of the A/A interval
ends (99 % BCa of the HL estimate, as % of exp); the proposed floor = max(10 %, 3 x noise).
competition U4 (a): the A/A cell must sit inside that floor; U4 (b): the family's SLOW cell
must be detected. A family whose records are NOISY under the X-7 reading (load, thermal),
or whose A/A or SLOW falsifier fails, gets no proposed floor: NOISY-UNFROZEN.

Usage: floors.py FAMILY AA-DIR SLOW-DIR [--quiet-only]  -> one TSV row on stdout
"""
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import decide  # noqa: E402


def main(argv):
    fam, aa_dir, slow_dir = argv[:3]
    quiet_only = "--quiet-only" in argv
    aa = decide.family([aa_dir], 30, quiet_only)[0]
    slow = decide.family([slow_dir], 30, quiet_only)[0]
    noise = decide.aa_noise_pct(aa)
    floor = decide.proposed_floor_pct(noise)
    u4a = decide.within_floor(aa, floor) and aa["verdict"] not in ("WIN", "LOSS")
    u4b = decide.slowdown_detected(slow)
    # all blocks: NOISY if any record is NOISY; --quiet-only: the NOISY blocks are already dropped,
    # so the read is NOISY only if too few quiet pairs remain (< 30, PREREG §7.2's A/A minimum).
    if quiet_only:
        noisy = aa["n_pairs"] < 30 or slow["n_pairs"] < 30
    else:
        noisy = "NOISY" in aa["statuses"] or "NOISY" in slow["statuses"]
    state = "PROPOSED" if (u4a and u4b and not noisy) else "NOISY-UNFROZEN"
    row = [fam, aa["cell"], str(aa["n_pairs"]), f"{aa['hl_pct']:+.3f}",
           f"[{aa['ci99_pct'][0]:+.3f}, {aa['ci99_pct'][1]:+.3f}]", f"{noise:.3f}", f"{floor:.1f}",
           "PASS" if u4a else "FAIL", slow["cell"], str(slow["n_pairs"]), f"{slow['hl_pct']:+.2f}",
           slow["verdict"], "PASS" if u4b else "FAIL", f"{aa['load_max']:.2f}/{slow['load_max']:.2f}",
           f"{min(aa['qos_ui_fraction_min'], slow['qos_ui_fraction_min']):.2f}",
           "quiet-blocks" if quiet_only else "all-blocks", state]
    print("\t".join(row))
    json.dump({"aa": aa, "slow": slow}, open(os.path.join(os.path.dirname(aa_dir.rstrip("/")),
              f"{fam}-decision{'-quiet' if quiet_only else ''}.json"), "w"), indent=1)


if __name__ == "__main__":
    main(sys.argv[1:])
