.DEFAULT_GOAL := help

compile:
	sbt "clean" "assembly" && cp target/scala-3.1.1/scala-assembly-0.1.0-SNAPSHOT.jar bot.jar

run:
	java -jar bot.jar

compile-docker:
	docker build . --tag bot-match/scala-bot-compiler
	docker run -v $(PWD):/home/compiler bot-match/scala-bot-compiler

help:
	@echo "Usage:"
	@echo " make compile"
	@echo " make run"
	@echo " make compile-docker"
