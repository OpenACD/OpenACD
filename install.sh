#!/bin/bash

dep="$1"
OALIBDIR="$2"
depname=$( basename $dep )

outdepdir=$2/"$depname"

mkdir -p "$outdepdir"

ebindir="$dep/ebin"
includedir="$dep/include"
privdir="$dep/priv"
echo $privdir
if [ -d "$ebindir" ]; then cp -Rf "$ebindir" "$outdepdir/ebin"; fi
if [ -d "$includedir" ]; then cp -Rf "$includedir" "$outdepdir/include"; fi
if [ -d "$privdir" ]; then cp -Rf "$privdir" "$outdepdir/priv"; fi





