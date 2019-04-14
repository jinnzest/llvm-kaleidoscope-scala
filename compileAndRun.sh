#/bin/bash
export JAVA_OPTS="-Dllc.path=/usr/local/Cellar/llvm/7.0.1/bin/llc"
target/pack/bin/runner "$1" && build/result
