#!/usr/bin/env bash

ruby -ryaml -e "p YAML.load(STDIN.read)" <"${ICD_HOME?}/appveyor.yml" >/dev/null
