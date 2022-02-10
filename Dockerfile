FROM game-runner-base

USER root

ADD . /home/footsoldiers/bot-match
WORKDIR /home/footsoldiers/bot-match

RUN chown -R footsoldiers .

USER footsoldiers

RUN /home/footsoldiers/.roswell/bin/qlot install && \
    ros run --load footsoldiers/build.lisp && \
    chmod +x /home/footsoldiers/bot-match/footsoldiers

ENTRYPOINT ["/home/footsoldiers/bot-match/footsoldiers-runner"]
