#!/bin/sh

echo "Updating submodules..."
git submodule init && git submodule update

# hack for reltool
if [ ! -d OpenACD ]; then
	mkdir OpenACD
	ln -sf ../ebin OpenACD/ebin
	ln -sf ../src OpenACD/src
	ln -sf ../include OpenACD/include
	ln -sf ../priv OpenACD/priv
fi
