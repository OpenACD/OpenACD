#!/bin/sh

if [ "${GIT_UPDATE_DISABLED}" != "1" ]; then
  echo "Updating submodules..."
  git submodule init && git submodule update

  # hack for reltool
  if [ ! -d OpenACD ]; then
	mkdir OpenACD
	ln -sf ../ebin OpenACD/ebin
	ln -sf ../src OpenACD/src
	ln -sf ../include OpenACD/include
  fi
fi
