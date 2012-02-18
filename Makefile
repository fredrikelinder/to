#
# - All _DIR variables end each value with a slash
# - All paths are absolute
#

THIS_MAKEFILE		= $(abspath $(lastword $(MAKEFILE_LIST)))
THIS_DIR		= $(dir $(THIS_MAKEFILE))
INIT_GIT_SUBMODULES	= $(shell git submodule update --init)

REBAR_DIR		= $(THIS_DIR)subs/rebar/

REBAR			= $(REBAR_DIR)rebar

.DEFAULT_GOAL: all

PHONY_TARGETS		+= all
PHONY_TARGETS		+= get-deps
PHONY_TARGETS		+= update-dist
PHONY_TARGETS		+= update-deps
PHONY_TARGETS		+= update-subs
PHONY_TARGETS		+= update-rebar
PHONY_TARGETS		+= compile
PHONY_TARGETS		+= release
PHONY_TARGETS		+= upgrade
PHONY_TARGETS		+= xref
PHONY_TARGETS		+= eunit
PHONY_TARGETS		+= clean-dist
PHONY_TARGETS		+= clean
PHONY_TARGETS		+= clean-rebar
PHONY_TARGETS		+= check-rebar

.PHONY: $(PHONY_TARGETS)
$(PHONY_TARGETS): $(THIS_MAKEFILE)

all: get-deps update-deps compile xref eunit

get-deps: check-rebar $(THIS_MAKEFILE)
	$(REBAR) get-deps

update-dist: update-subs update-deps

update-deps: check-rebar
	$(REBAR) update-deps

update-subs: update-rebar

update-rebar:
	( cd $(REBAR_DIR) ; git pull )

compile: check-rebar
	$(REBAR) compile

release: check-rebar
	$(REBAR) generate

upgrade: check-rebar
	$(REBAR) generate-upgrade previous_release=$(RELEASE_DIR)

xref: check-rebar
	$(REBAR) xref

eunit: check-rebar
	$(REBAR) eunit

clean-dist: clean clean-rebar

clean: check-rebar
	$(REBAR) clean delete-deps

clean-rebar:
	( cd $(REBAR_DIR) ; make clean )

#
# subs/rebar/rebar
#

check-rebar: $(REBAR)
ifeq ($(strip $(REBAR)),)
	$(error "Cannot find rebar")
endif

$(REBAR): $(foreach P,bootstrap src/*.erl include/*.hrl,$(wildcard $(REBAR_DIR)$(P)))
	( cd $(REBAR_DIR) ; ./bootstrap )
