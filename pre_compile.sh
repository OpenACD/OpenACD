#!/bin/sh

cp proto_src/* src/
echo "Updating submodules..."
git submodule init && git submodule update
