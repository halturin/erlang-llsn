ERL         ?= erl +A 4 +K true
REBAR       ?= ./rebar
DIALYZER    ?= dialyzer

INCLUDE_DIR := include
SRC_DIR     := src

TEST_APPS   := $(a)
TEST_SUITES := $(s)
TEST_TESTS  := $(t)

.PHONY: all
all: deps compile-all
	@echo "Done."

deps:
	@echo "[ $(APP_NAME): Getting all the dependencies... ]"
	@$(REBAR) get-deps

update-deps:
	@echo "[ $(APP_NAME): Updating all the dependencies... ]"
	@$(REBAR) update-deps

compile-all:
	@echo "[ $(APP_NAME): Compile all the dependencies and project code... ]"
	@$(REBAR) compile

compile:
	@echo "[ $(APP_NAME): Compile only project code... ]"
	@$(REBAR) compile skip_deps=true
	@echo "Done."

debug:
	@echo "[ $(APP_NAME): Compile project code with DEBUG option... ]"
	@$(REBAR) compile skip_deps=true -Ddebug
	@echo "Done."

clean:
	@echo "[ $(APP_NAME): Clean only project bytecode... ]"
	@$(REBAR) clean skip_deps=true
	@echo "Done."

distclean: clean
	@echo "[ $(APP_NAME): Clean deps... ]"
	@$(REBAR) delete-deps
	@echo "Done."

docs:
	@echo "[ $(APP_NAME): Update documentation... ]"
	@$(ERL) -noshell -run edoc_run application '$(APP_NAME)' '"."' '[]'
	@echo "Done."

test: compile
	@echo "[ $(APP_NAME): Test the project... ]"
	@$(REBAR) eunit apps=$(TEST_APPS) suites=$(TEST_SUITES) tests=$(TEST_TESTS) skip_deps=true
	@echo "Done."

dialyze:
	@$(DIALYZER) -n -I $(INCLUDE_DIR) --src $(SRC_DIR)/*.erl
	@echo "Done."
