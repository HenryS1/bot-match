FROM ubuntu:21.04

RUN apt-get update
RUN apt-get install -y wget
RUN apt-get install tar
RUN wget https://github.com/roswell/roswell/releases/download/v20.01.14.104/roswell_20.01.14.104-1_amd64.deb
RUN apt-get install -y libcurl3-gnutls
RUN apt-get install -y git
RUN apt-get install -y bzip2
RUN apt-get install -y make
RUN dpkg -i roswell_20.01.14.104-1_amd64.deb

RUN apt-get install -y zip
RUN apt-get install -y unzip
RUN apt-get install -y curl
RUN apt-get install -y gnupg

# Add key for Azul java build
RUN apt-key adv \
  --keyserver hkp://keyserver.ubuntu.com:80 \
  --recv-keys 0xB1998361219BD9C9

# Add Azul apt repository
RUN curl -O https://cdn.azul.com/zulu/bin/zulu-repo_1.0.0-3_all.deb
RUN apt-get install ./zulu-repo_1.0.0-3_all.deb
RUN apt-get update

# Install openjre 17
RUN apt-get install -y zulu17-jre

RUN useradd -ms /bin/bash footsoldiers

USER footsoldiers
RUN ros install sbcl-bin/2.2.0
RUN ros install qlot
RUN ros install rove
RUN mkdir -p /home/footsoldiers/.config/common-lisp
RUN echo "(:source-registry (:tree (:home \"bot-match\")) :inherit-configuration)" > /home/footsoldiers/.config/common-lisp/source-registry.conf

USER root
ADD ./qlfile /home/footsoldiers/bot-match/qlfile
WORKDIR /home/footsoldiers/bot-match
RUN chown -R footsoldiers .
USER footsoldiers

RUN /home/footsoldiers/.roswell/bin/qlot install
USER root
ADD ./game-runner /home/footsoldiers/bot-match/game-runner
ADD ./runtime /home/footsoldiers/bot-match/runtime
ADD ./footsoldiers /home/footsoldiers/bot-match/footsoldiers
RUN chown -R footsoldiers .

USER footsoldiers

RUN ros run --load footsoldiers/build.lisp
RUN chmod +x /home/footsoldiers/bot-match/footsoldiers-runner

ENTRYPOINT ["/home/footsoldiers/bot-match/footsoldiers-runner"]
