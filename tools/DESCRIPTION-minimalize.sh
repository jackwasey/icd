#!/usr/bin/env bash

! [[ -f DESCRIPTION ]] &&
    [[ -d icd ]] &&
    {
        cd icd ||
            {
                echo "DESCRIPTION not in current directory, and no icd DIRECTORY either" >&2;
                exit 1;
            };
    }
awk 'BEGIN {g=0} /^[A-Za-z]+:/ {f=0} /^Suggests:/ {f=1} { if (f) { if (!g){print "Suggests:\n    testthat";g=1;}} else print $0; }' DESCRIPTION |
    awk '/^[A-Za-z]+:/ {f=0} /^VignetteBuilder:/ {f=1} !f'
