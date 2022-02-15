.DEFAULT_GOAL := help

compile:
	cargo build

run:
	cargo run

help:
	@echo "Usage:"
	@echo "  make compile"
	@echo "  make compile-docker"
	@echo "  make run"

compile-docker:
	docker build . --tag build-rust-bot-for-docker
	docker run -v $(PWD)/build:/build build-rust-bot-for-docker
	mv build/bot ./
