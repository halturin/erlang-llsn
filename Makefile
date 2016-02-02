#!/usr/bin/env make -rRf

APP_NAME := llsn

include ./Makefile.inc

run: compile
	@echo "[ Run... ]"
	@$(ERL) -name llsn@127.0.0.1 -pa ebin -s llsn_helper -setcookie dev -Ddebug=true
