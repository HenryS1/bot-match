FROM ubuntu:21.04

RUN useradd -ms /bin/bash bot

RUN apt-get update 
RUN apt-get install -y wget 
RUN apt-get install tar 
RUN wget https://github.com/roswell/roswell/releases/download/v20.01.14.104/roswell_20.01.14.104-1_amd64.deb 
RUN apt-get install -y libcurl3-gnutls 
RUN apt-get install -y git 
RUN apt-get install -y bzip2 
RUN apt-get install -y make 
RUN dpkg -i roswell_20.01.14.104-1_amd64.deb


ADD ./qlfile /home/bot/bot-match/qlfile
WORKDIR /home/bot/bot-match
RUN chown -R bot .
USER bot
RUN ros install sbcl-bin/2.2.1
RUN ros install qlot 
RUN ros install rove 
RUN mkdir -p /home/bot/.config/common-lisp 
RUN echo "(:source-registry (:tree (:home \"bot-match-bot\")) :inherit-configuration)" > /home/bot/.config/common-lisp/source-registry.conf 
RUN /home/bot/.roswell/bin/qlot install

ADD . /home/bot/bot-match
USER root
RUN chown -R bot /home/bot/bot-match
USER bot
RUN ros use sbcl-bin/2.2.1
RUN ros dynamic-space-size=800 run --load ./build.lisp

ENTRYPOINT ["./bot"]
