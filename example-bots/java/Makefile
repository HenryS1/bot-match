.DEFAULT_GOAL := help

compile:
	./gradlew clean build && cp app/build/libs/app-all.jar bot.jar

run:
	java -jar bot.jar

compile-docker:
	docker build . --tag bot-match/java-compiler
	docker run -v $(PWD):/home/compiler bot-match/java-compiler

help:
	@echo "Usage:"
	@echo " make compile"
	@echo " make run"
	@echo " make compile-docker"