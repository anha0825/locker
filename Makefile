.PHONY: compile
compile:
	rebar3 compile

.PHONY: eunit
eunit:
	rebar3 eunit

ct:
	rebar3 ct --sname ct

.PHONY: dialyzer
dialyzer:
	rebar3 dialyzer

.PHONY: xref
xref:
	rebar3 xref

.PHONY: test
test: compile xref eunit ct
