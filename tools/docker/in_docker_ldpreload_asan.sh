#!/bin/bash
# Copyright (C) 2014 - 2018  Jack O. Wasey
#
# This file is part of icd.
#
# icd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# icd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd. If not, see <http:#www.gnu.org/licenses/>.

set -euo pipefail
IFS=$'\n\t'
set -x

# This is in in_docker_base.sh but apparently the environment is not remembered (or the test didn't work in that script)

## R_CMD_ERR is defined in in_docker_base

# which libasan library?
for tryasanlib in /usr/lib/llvm-7/lib/clang/7.0.0//lib/linux/libclang_rt.asan-x86_64.so /usr/lib/x86_64-linux-gnu/libasan.so.5 /usr/lib/x86_64-linux-gnu/libasan.so.4 /usr/lib/x86_64-linux-gnu/libasan.so.3 /usr/lib/x86_64-linux-gnu/libasan.so.2; do
  if [ -e "$tryasanlib" ]; then
    if LD_PRELOAD="$tryasanlib" ${R_CMD_ERR}; then
      export LD_PRELOAD="$tryasanlib"
      exit 0
    fi
  fi
done
echo "Cannot find libasan."
exit 1
