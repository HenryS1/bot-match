FROM ubuntu:21.04

RUN useradd -ms /bin/bash bot
ADD . /home/bot/bot
WORKDIR /home/bot/bot

RUN chown -R bot .

RUN apt-get update && \
    apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh

USER bot

CMD stack install --local-bin-path /build/
