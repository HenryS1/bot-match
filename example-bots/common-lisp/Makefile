.DEFAULT_GOAL := help

install-deps:
	qlot install

compile:
	ros run --load build.lisp

run:
	./quiescent

help:
	@echo "Usage:"
	@echo "  make install-deps"
	@echo "  make compile"
	@echo "  make run"

compile-docker:
	docker build . --tag build-for-docker
	docker run -v $(PWD)/build:/build build-for-docker
	mv build/quiescent ./
