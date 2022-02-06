FROM ubuntu:21.04

RUN useradd -ms /bin/bash footsoldiers
ADD . /home/footsoldiers/bot-match
WORKDIR /home/footsoldiers/bot-match

RUN chown -R footsoldiers .

RUN apt-get update && \
 apt-get install -y wget && \
 apt-get install tar && \
 wget https://github.com/roswell/roswell/releases/download/v20.01.14.104/roswell_20.01.14.104-1_amd64.deb && \
 apt-get install -y libcurl3-gnutls && \
 apt-get install -y git && \
 apt-get install -y bzip2 && \
 apt-get install -y make && \
 dpkg -i roswell_20.01.14.104-1_amd64.deb 
 
USER footsoldiers
RUN ros install sbcl-bin/2.2.0 && \
 ros install qlot && \
 /home/footsoldiers/.roswell/bin/qlot install && \
 mkdir -p /home/footsoldiers/.config/common-lisp && \
 ros run --load footsoldiers/build.lisp && \
 chmod +x /home/footsoldiers/bot-match/footsoldiers

ENTRYPOINT ["/home/footsoldiers/bot-match/footsoldiers-runner"]
