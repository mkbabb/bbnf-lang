BEGIN {
    FS = "|"
    ok = 1

    add("citm_catalog/direct_to_struct", 18191, 17431)
    add("apache_builds/direct_to_struct", 11028, 9996)
    add("marine_ik/direct_to_struct", 8759, 9248)
    add("unicode_basic/direct_to_struct", 2253, 2182)

    add("twitter/real_typed_struct", 17385, 15593)
    add("citm_catalog/real_typed_struct", 29928, 17321)
    add("apache_builds/real_typed_struct", 8308, 6754)
    add("github_events/real_typed_struct", 11633, 12029)
    add("update_center/real_typed_struct", 11613, 10150)
    add("mesh/real_typed_struct", 9214, 7739)
    add("marine_ik/real_typed_struct", 11552, 9894)
}

function add(key, track1, track2) {
    expected[key] = 1
    floor_track1[key] = track1
    floor_track2[key] = track2
}

function trim(value) {
    gsub(/^[ \t]+/, "", value)
    gsub(/[ \t]+$/, "", value)
    return value
}

function fail(message) {
    print message > "/dev/stderr"
    ok = 0
}

/^\|/ {
    corpus = trim($2)
    workload = trim($3)
    key = corpus "/" workload

    if (key in expected) {
        track1 = trim($11) + 0
        track2 = trim($12) + 0
        seen[key] = 1

        if (track1 < floor_track1[key]) {
            fail(key " Track 1 " track1 " below floor " floor_track1[key])
        }
        if (track2 < floor_track2[key]) {
            fail(key " Track 2 " track2 " below floor " floor_track2[key])
        }
    }
}

END {
    for (key in expected) {
        if (!(key in seen)) {
            fail("missing required guard row " key)
        }
    }

    if (!ok) {
        exit 1
    }

    print "SK-V12 JSON guard floors PASS"
}
