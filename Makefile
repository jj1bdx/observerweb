.PHONY: _build

BASE_DIR = $(shell pwd)
REBAR    = rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

rel:
	@$(REBAR) release
