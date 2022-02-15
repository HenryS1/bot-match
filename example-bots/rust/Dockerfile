FROM ubuntu:21.04

RUN useradd -ms /bin/bash bot
ADD . /home/bot/bot
WORKDIR /home/bot/bot

RUN chown -R bot .

RUN apt-get update && \
 apt-get install -y cargo

USER bot

CMD cargo build --release && cp target/release/bot /build/
