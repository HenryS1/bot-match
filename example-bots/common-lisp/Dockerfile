FROM ubuntu:21.04

RUN useradd -ms /bin/bash bot
ADD . /home/bot/bot-match
WORKDIR /home/bot/bot-match

RUN chown -R bot .

RUN apt-get update && \
 apt-get install -y wget && \
 apt-get install tar && \
 wget https://github.com/roswell/roswell/releases/download/v20.01.14.104/roswell_20.01.14.104-1_amd64.deb && \
 apt-get install -y libcurl3-gnutls && \
 apt-get install -y git && \
 apt-get install -y bzip2 && \
 apt-get install -y make && \
 dpkg -i roswell_20.01.14.104-1_amd64.deb

USER bot
RUN ros install sbcl-bin/2.2.0 && \
 ros install qlot && \
 ros install rove && \
 mkdir -p /home/bot/.config/common-lisp && \
 echo "(:source-registry (:tree (:home \"bot-match-bot\")) :inherit-configuration)" > /home/bot/.config/common-lisp/source-registry.conf && \
 /home/bot/.roswell/bin/qlot install

CMD ros dynamic-space-size=800 run --load ./build.lisp && cp quiescent /build/
