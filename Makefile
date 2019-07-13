#
# This is the rebar command location, if rebar is in the user path
# then remove './' from every call to escript, because we try call 
# rebar script from this location.
#
REBAR=rebar3

all: compile

compile:
ifndef debug
	$(warning DEBUG logging off)
	@./$(REBAR) compile
else
ifeq ($(debug), on)
	$(info DEBUG logging on)
	@./$(REBAR) -D debug_logging compile
else
	$(error DEBUG logging option with 'on')
endif
endif 

clean:
	@./$(REBAR) clean
	@rm -rf erl_crash*

tests:
	@./$(REBAR) compile eunit 

