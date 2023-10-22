#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

pushd ${SCRIPT_DIR}/.. > /dev/null

pushd duckdb > /dev/null
python3 scripts/amalgamation.py --list-sources | xargs -I{} echo duckdb/{} | sort
popd

popd
