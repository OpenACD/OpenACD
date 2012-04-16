#! /usr/bin/env bash

function post_compile {
	cat success_message
}

case $1 in
	"post_compile")
		post_compile;;
esac
