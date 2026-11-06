#!/usr/bin/env bash
# LW1.TOPO-3 falsifier. GREEN iff
#  (a) origin holds sk-lineage/local-master=af15f63e0, codex/sk-v26-bbnf=b5fd0ca98,
#      codex/sk-v26-parse-that=e31fbfe and pprint sk-lineage/pprint-local=2b4d2d4;
#  (b) `shasum -a 256 -c` over <custody>/MANIFEST.sha256 is rc 0;
#  (c) every preserve commit in <custody>/PRESERVE.tsv is on origin at its SHA;
#  (d) every blob in <custody>/big/ is a release asset of bbnf-lang tag
#      custody-sk-v25 whose GitHub digest is its sha256.
C=${1:-$HOME/.dev-logs/bbnf-lockstep/custody}; P=$HOME/Programming; bad=()
want() { local got; got=$(git -C "$P/$1" ls-remote origin "refs/heads/$2" | cut -c1-${#3})
  [[ $got == "$3" ]] || bad+=("$1 $2: origin has '${got:-nothing}', want $3"); }
want bbnf-lang sk-lineage/local-master af15f63e0
want bbnf-lang codex/sk-v26-bbnf b5fd0ca98
want parse-that codex/sk-v26-parse-that e31fbfe
want pprint sk-lineage/pprint-local 2b4d2d4
if [[ -f $C/MANIFEST.sha256 ]]; then
  (cd "$C" && shasum -a 256 -c --quiet MANIFEST.sha256) || bad+=("shasum -c MANIFEST.sha256 failed")
else bad+=("no custody manifest at $C/MANIFEST.sha256"); fi
if [[ -f $C/PRESERVE.tsv ]]; then
  while IFS=$'\t' read -r repo ref sha _; do want "$repo" "$ref" "$sha"; done < "$C/PRESERVE.tsv"
else bad+=("no PRESERVE.tsv"); fi
if [[ -d $C/big ]]; then
  assets=$(gh release view custody-sk-v25 -R mkbabb/bbnf-lang --json assets -q '.assets[]|.name+" "+.digest' 2>/dev/null)
  for f in "$C"/big/*; do h=$(basename "$f")
    grep -qx "$h sha256:$h" <<<"$assets" || bad+=("big blob $h is not a verified release asset"); done
fi
if ((${#bad[@]})); then printf 'RED\n'; printf '  %s\n' "${bad[@]}"; exit 1; fi
echo "GREEN refs 4/4, manifest $(wc -l < "$C/MANIFEST.sha256" | tr -d ' ') entries, preserve $(wc -l < "$C/PRESERVE.tsv" | tr -d ' '), big $(ls "$C/big" | wc -l | tr -d ' ')"
