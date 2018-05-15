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

# which libasan library?
if [ -e /usr/lib/llvm-4.0/lib/clang/4.0.1/lib/linux/libclang_rt.asan-x86_64.so ]; then
  if LD_PRELOAD="/usr/lib/llvm-4.0/lib/clang/4.0.1/lib/linux/libclang_rt.asan-x86_64.so" ${R_CMD_ERR}; then
    export LD_PRELOAD="/usr/lib/llvm-4.0/lib/clang/4.0.1/lib/linux/libclang_rt.asan-x86_64.so"
  fi
elif [ -e /usr/lib/x86_64-linux-gnu/libasan.so.4 ]; then
  if LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libasan.so.4" ${R_CMD_ERR}; then
    export LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libasan.so.4"
  fi
elif [ -e /usr/lib/x86_64-linux-gnu/libasan.so.3 ]; then
  if LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libasan.so.3" ${R_CMD_ERR}; then
    export LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libasan.so.3"
  fi
elif [ -e /usr/lib/x86_64-linux-gnu/libasan.so.2 ]; then
  if LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libasan.so.2" ${R_CMD_ERR}; then
    export LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libasan.so.2"
  fi
else
  echo "Cannot find libasan in /usr/lib/x86_64-linux-gnu"
fi
