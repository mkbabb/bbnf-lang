#!/usr/bin/env python3
"""sk-v27 LW3.RX-1a: write parse-that's oracle leaf corpus from the OR-7 records.

One JSON file, no grammar vocabulary, that parse-that-regex's own test reads:
per leaf (the 171 of rx/LOOKAROUND-CENSUS.tsv) its id, pattern, flags and
tier (DFA or LOOKAROUND), and per case the input and the V8+JSC answer:
an integer UTF-8 match end, null for NO_MATCH, "ENC" for a match that ends
inside a surrogate pair (no UTF-8 image), "AMB" for SPEC-AMBIGUITY.

Usage: leaf_corpus.py <census.tsv> <or7-corpus.json.gz> <or7-answers.json.gz> <out.json>
"""
import gzip
import hashlib
import json
import sys


def main():
    census, corpus_gz, answers_gz, out = sys.argv[1:5]
    corpus_raw = gzip.open(corpus_gz).read()
    answers_raw = gzip.open(answers_gz).read()
    corpus = json.loads(corpus_raw)
    answers = json.loads(answers_raw)
    leaves = []
    by_id = {}
    with open(census, encoding="utf-8") as f:
        head = f.readline().rstrip("\n").split("\t")
        for line in f:
            row = dict(zip(head, line.rstrip("\n").split("\t")))
            leaf = {
                "id": f'{row["corpus"]}#{row["n"]}',
                "pattern": row["pattern"],
                "flags": row["flags"],
                "tier": "LOOKAROUND" if row["lookaround"] == "1" else "DFA",
                "cases": [],
            }
            leaves.append(leaf)
            by_id[leaf["id"]] = leaf
    for case in corpus["cases"]:
        lid = case["id"].rsplit("|", 1)[0]
        leaf = by_id[lid]
        assert leaf["pattern"] == case["pattern"] and leaf["flags"] == case["flags"], lid
        ans = answers[case["id"]]
        st = ans["status"]
        if st == "MATCH":
            want = ans.get("end8")
            want = "ENC" if want is None else want
        elif st == "NO_MATCH":
            want = None
        elif st == "SPEC-AMBIGUITY":
            want = "AMB"
        else:
            raise SystemExit(f"{case['id']}: status {st}")
        leaf["cases"].append([case["input"], want])
    doc = {
        "name": "regex-leaf-corpus-v1",
        "note": "Sticky match at offset 0 with each leaf's flags; answers are V8 and JavaScriptCore agreeing (2 reps each). "
        "want: UTF-8 match end, null = no match, ENC = ends inside a surrogate pair, AMB = engines disagree.",
        "source_corpus_sha256": hashlib.sha256(gzip.decompress(open(corpus_gz, "rb").read())).hexdigest(),
        "source_answers_sha256": hashlib.sha256(gzip.decompress(open(answers_gz, "rb").read())).hexdigest(),
        "leaves": leaves,
    }
    text = json.dumps(doc, ensure_ascii=False, separators=(",", ":"))
    text = text.replace(',{"id":', ',\n{"id":')
    open(out, "w", encoding="utf-8").write(text + "\n")
    n = sum(len(l["cases"]) for l in leaves)
    print(f"leaf corpus: {len(leaves)} leaves, {n} cases -> {out}")


if __name__ == "__main__":
    main()
