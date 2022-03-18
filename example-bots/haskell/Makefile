.DEFAULT_GOAL := help

compile:
	stack build

run:
	stack run

help:
	@echo "Usage:"
	@echo "  make compile-docker"

compile-docker:
	docker build . --tag bot-match/haskell-example
