#!/usr/bin/env bash

ruby -ryaml -e "p YAML.load(STDIN.read)" <"${ICD_HOME:-.}/.travis.yml" >/dev/null
