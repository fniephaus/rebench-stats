#!/usr/bin/env bash

set -e

if [[ -z "${GRAALVM_HOME}" ]]; then
  echo '$GRAALVM_HOME must be set.'
  exit 1
fi

"${GRAALVM_HOME}"/bin/npm install
"${GRAALVM_HOME}"/bin/R -e 'install.packages(c("ggplot2", "dplyr"))'
