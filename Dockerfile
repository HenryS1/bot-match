FROM game-runner-base

USER root

ADD . /home/footsoldiers/bot-match
WORKDIR /home/footsoldiers/bot-match

RUN chown -R footsoldiers .

USER footsoldiers

ARG QLHASH=none

RUN /home/footsoldiers/.roswell/bin/qlot install

ARG GITSHA=none
RUN ros run --load footsoldiers/build.lisp
RUN chmod +x /home/footsoldiers/bot-match/footsoldiers

ENTRYPOINT ["/home/footsoldiers/bot-match/footsoldiers-runner"]
