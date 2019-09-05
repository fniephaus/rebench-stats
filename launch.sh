#!/usr/bin/env bash

set -e

if [[ -z "${GRAALVM_HOME}" ]]; then
  echo '$GRAALVM_HOME must be set.'
  exit 1
fi

"${GRAALVM_HOME}"/bin/node --polyglot --jvm src/server.js
