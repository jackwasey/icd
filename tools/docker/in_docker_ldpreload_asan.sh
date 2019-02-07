#!/bin/bash
set -euo pipefail
IFS=$'\n\t'
set -x

# This is in in_docker_base.sh but apparently the environment is not remembered (or the test didn't work in that script)

## R_CMD_ERR is defined in in_docker_base 

# which libasan library?
for tryasanlib in \
                  /usr/local/lib/clang/9.0.0/lib/linux/libclang_rt.asan-x86_64.so \
                  /usr/lib/llvm-8/lib/clang/8.0.1/lib/linux/libclang_rt.asan-x86_64.so \
                  /usr/lib/llvm-8/lib/clang/8.0.0/lib/linux/libclang_rt.asan-x86_64.so \
                  /usr/lib/llvm-7/lib/clang/7.0.1/lib/linux/libclang_rt.asan-x86_64.so \
                  /usr/lib/llvm-7/lib/clang/7.0.0/lib/linux/libclang_rt.asan-x86_64.so \
                  /usr/lib/x86_64-linux-gnu/libasan.so.8 \
                  /usr/lib/x86_64-linux-gnu/libasan.so.7 \
                  /usr/lib/x86_64-linux-gnu/libasan.so.8 \
                  /usr/lib/x86_64-linux-gnu/libasan.so.5 \
                  /usr/lib/x86_64-linux-gnu/libasan.so.4 \
                  /usr/lib/x86_64-linux-gnu/libasan.so.3 \
                  /usr/lib/x86_64-linux-gnu/libasan.so.2; do
  if [ -e "$tryasanlib" ]; then
    if LD_PRELOAD="$tryasanlib" ${R_CMD_ERR}; then
      echo "exporting $tryasanlib"
      export LD_PRELOAD="$tryasanlib"
      exit 0
    fi
  fi
done
echo "Cannot find libasan."
exit 1
