#!/bin/sh

if [ "${GIT_UPDATE_DISABLED}" != "1" ]; then
  echo "Updating submodules..."
  git submodule init && git submodule update
fi
