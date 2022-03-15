FROM ubuntu:21.04

RUN useradd -ms /bin/bash bot

RUN apt-get update && \
 apt-get install -y cargo

ADD ./Cargo.toml /home/bot/Cargo.toml
ADD ./src /home/bot/src

RUN chown -R bot /home/bot

USER bot

WORKDIR /home/bot

RUN cargo build --release 

ENTRYPOINT ["./target/release/bot"]
