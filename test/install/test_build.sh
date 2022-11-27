#!/bin/bash
set -ex

if [ $# != 1 ]; then
  echo "Invalid nr. of argument (obtained: $#, expected: 1)" >&2
  echo "test_build.sh CMAKE_PREFIX_PATH" >&2
  exit
fi
cmakeprefpath=$1
sourcefolder="$(dirname $0)"

CMAKE_PREFIX_PATH=${cmakeprefpath} cmake ${sourcefolder}
make
./printversion
