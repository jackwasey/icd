#!/bin/bash

function ldpreload_asan {
# This is in in_docker_base.sh but apparently the environment is not remembered
# (or the test didn't work in that script)

## R_CMD_ERR is defined in in_docker_base

# asan_old=$(find -type f -name "libasan.so.*" | sort -r | head -1)
# asan_new=$(find /usr -type f -name "libclang_rt.asan-x86_64.so" | sort -r | head -1)

# if [[ -n "$asan_new" ]]; then
#   export LD_PRELOAD="$asan_new"
#   return 0
# fi
#
# if [[ -n "$asan_old" ]]; then
#   export LD_PRELOAD="$asan_new"
#   return 0
# fi

for tryasanlib in \
  /usr/local/lib/clang/9.0.0/lib/linux/libclang_rt.asan-x86_64.so \
  /usr/lib/llvm-8/lib/clang/8.0.1/lib/linux/libclang_rt.asan-x86_64.so \
  /usr/lib/llvm-8/lib/clang/8.0.0/lib/linux/libclang_rt.asan-x86_64.so \
  /usr/lib/llvm-7/lib/clang/7.0.1/lib/linux/libclang_rt.asan-x86_64.so \
  /usr/lib/llvm-7/lib/clang/7.0.0/lib/linux/libclang_rt.asan-x86_64.so \
  /usr/lib/llvm-4.0/lib/clang/4.0.1/lib/linux/libclang_rt.asan-x86_64.so \
  /usr/lib/x86_64-linux-gnu/libasan.so.8 \
  /usr/lib/x86_64-linux-gnu/libasan.so.7 \
  /usr/lib/x86_64-linux-gnu/libasan.so.8 \
  /usr/lib/x86_64-linux-gnu/libasan.so.5.0.0 \
  /usr/lib/x86_64-linux-gnu/libasan.so.5 \
  /usr/lib/x86_64-linux-gnu/libasan.so.4 \
  /usr/lib/x86_64-linux-gnu/libasan.so.3 \
  /usr/lib/x86_64-linux-gnu/libasan.so.2; do
  if [ -e "$tryasanlib" ]; then
    if LD_PRELOAD="$tryasanlib" \
       ASAN_OPTIONS=abort_on_error=0,detect_leaks=0 \
       echo "cat()" | ASAN_OPTIONS=abort_on_error=0,detect_leaks=0 ${R_CMD} --slave; then
      echo "Exporting: $tryasanlib"
      export LD_PRELOAD="$tryasanlib"
      return 0
    fi
  fi
done
echo "WARNING: Cannot find libasan (only relevant in a clang UBSAN/ASAN build"
return 1
}

