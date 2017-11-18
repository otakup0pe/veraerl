default: build

build:
	rebar3 escriptize

clean:
	rm -f _build veraerl

.PHONY: build clean
