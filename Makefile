export ERLANG_MK ?= $(CURDIR)/erlang.mk

PROJECT = erlkit
PROJECT_DESCRIPTION = A somewhat std library for erlang
PROJECT_VERSION = 0.1.0

all:: $(ERLANG_MK)
$(ERLANG_MK):
	curl https://erlang.mk/erlang.mk | make -f -

include $(ERLANG_MK)
