.PHONY: all compile deps clean test

REBAR_CONFIG = rebar.config
REBAR_TEST_CONFIG = rebar.test.config

all: clean deps test

deps: get-deps update-deps
	@./rebar -C $(REBAR_CONFIG) compile

update-deps:
	@./rebar -C $(REBAR_CONFIG) update-deps

get-deps:
	@./rebar -C $(REBAR_CONFIG) get-deps

compile:
	@./rebar -C $(REBAR_CONFIG) compile skip_deps=true
#	@./rebar -C $(REBAR_CONFIG) xref skip_deps=true

clean:
	@./rebar -C $(REBAR_CONFIG) clean skip_deps=true

test-deps:
	@./rebar -C $(REBAR_TEST_CONFIG) get-deps

test-compile:
	@./rebar -C $(REBAR_TEST_CONFIG) compile

test: compile
	rm -rf .eunit
	@./rebar -C $(REBAR_TEST_CONFIG) eunit skip_deps=true
