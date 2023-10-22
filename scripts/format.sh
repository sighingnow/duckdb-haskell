#!/bin/bash

set -eu

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

pushd ${SCRIPT_DIR}/.. > /dev/null

echo "Formatting hs files ..."
git ls-files -z '*.hs' | xargs -P 12 -0 fourmolu --mode inplace

echo "Formatting hsc files ..."
git ls-files -z '*.hsc' | xargs -P 12 -0 fourmolu --mode inplace

popd > /dev/null
