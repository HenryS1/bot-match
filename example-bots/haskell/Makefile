.DEFAULT_GOAL := help

compile:
	stack build

run:
	stack run

help:
	@echo "Usage:"
	@echo "  make compile"
	@echo "  make compile-docker"
	@echo "  make run"

compile-docker:
	docker build . --tag build-haskell-bot-for-docker
	docker run -v $(PWD)/build:/build build-haskell-bot-for-docker
	mv build/bot ./
