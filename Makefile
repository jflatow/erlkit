export ERLANG_MK ?= erlang.mk

PROJECT = erlkit
PROJECT_DESCRIPTION = A somewhat std library for erlang
PROJECT_VERSION = 0.1.0

all:: $(ERLANG_MK)
erlang.mk:
	curl https://erlang.mk/erlang.mk > $@

include $(ERLANG_MK)
