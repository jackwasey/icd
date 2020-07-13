#!/usr/bin/env bash
set -euo pipefail
grep \
	--color=always \
	--extended-regexp \
	--only-matching \
	"$@" \
	-e '([[:space:]]|,)const [[:alpha:] ]+[^&*=]+$' \
	"${ICD_HOME:-${HOME}/icd}"/src/*.{c,cpp,h} \
	| 
	grep -v \
	--color=always \
	--extended-regexp \
	-e "RcppExports" \
	-e "const (bool|int|char)" \

	# --only-matching \
#grep \ -e '^.*:.*const [[:alpha:] ]+[^&*]+$' \ |
