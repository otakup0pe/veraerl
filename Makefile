default: build

build:
	rebar3 escriptize

clean:
	rebar3 clean
	rm -rf _checkouts/*/ebin/ _checkouts/*/_build _build

.PHONY: build clean
