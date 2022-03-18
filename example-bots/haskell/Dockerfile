FROM ubuntu:21.04

RUN useradd -ms /bin/bash bot

RUN apt-get update && \
    apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh

ADD ./HaskellBot.cabal /home/bot/HaskellBot.cabal
ADD ./app /home/bot/app
ADD ./src /home/bot/src
ADD ./stack.yaml /home/bot/stack.yaml
ADD ./package.yaml /home/bot/package.yaml

WORKDIR /home/bot

RUN chown -R bot /home/bot

USER bot

RUN mkdir /home/bot/build
RUN stack install --local-bin-path /home/bot/build

ENTRYPOINT ["./build/bot"]