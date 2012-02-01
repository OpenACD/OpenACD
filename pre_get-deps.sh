#!/bin/sh

if [ "${GIT_UPDATE_DISABLED}" != "1" ]; then
	echo "Updating submodules..."
	git submodule init && git submodule update
fi

if [ ! -d ../oacd_freeswitch ]; then
	if [ ! -d ../oacd_dummy ]; then
		cp -R include_apps/ ../
	fi
fi

if [ ! -d ../../proto_src ]; then
	mkdir ../../proto_src
fi

cp proto_src/*.proto ../../proto_src
