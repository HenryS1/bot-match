docker:
	docker build -t bot-runner .

run-docker:
	docker run bot-runner --config-file-path ./footsoldiers/game-config.json --map-file-path ./footsoldiers/game-map --bot-dir-1 ./footsoldiers/tests/bot1 --bot-dir-2 ./footsoldiers/tests/bot2

compile:
	ros run --load footsoldiers/build.lisp

run:
	./footsoldiers-runner --config-file-path ./footsoldiers/game-config.json --map-file-path ./footsoldiers/game-map --bot-dir-1 ./footsoldiers/tests/bot1 --bot-dir-2 ./footsoldiers/tests/bot2
