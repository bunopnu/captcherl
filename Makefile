BUILDTOOL = rebar3

.PHONY: build test doc

build:
	  $(BUILDTOOL) compile

format:
	  $(BUILDTOOL) efmt -w

check:
	  $(BUILDTOOL) efmt -c
	  $(BUILDTOOL) do xref, dialyzer

test:
	  $(BUILDTOOL) eunit

cover:
	  $(BUILDTOOL) cover

doc:
	  $(BUILDTOOL) edoc

clean:
	  $(BUILDTOOL) clean

start:
	  $(BUILDTOOL) shell
