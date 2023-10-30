all: build format test cover doc clean

build:
	rebar3 compile

format:
	efmt -w

test:
	efmt -c
	rebar3 do xref, dialyzer
	rebar3 eunit

cover:
	rebar3 cover

doc:
	rebar3 edoc

clean:
	rebar3 clean

start:
	rebar3 shell
