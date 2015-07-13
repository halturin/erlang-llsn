#!/usr/bin/env make -rRf

APP_NAME := llsn

include ./project.mk

run: compile
	@echo "[ Run... ]"
	@$(ERL) -name proto@127.0.0.1\
			-pa ebin deps/*/ebin  ../deps/*/ebin ../*/ebin\
			-s llsn_helper
			-setcookie dev -Ddebug=true
