FROM ubuntu:21.04

RUN apt-get update
RUN apt-get install -y zip
RUN apt-get install -y unzip
RUN apt-get install -y curl

RUN useradd -ms /bin/bash bot

RUN apt-get install -y gnupg

# Add key for Azul java build
RUN apt-key adv \
  --keyserver hkp://keyserver.ubuntu.com:80 \
  --recv-keys 0xB1998361219BD9C9

# Add Azul apt repository
RUN curl -O https://cdn.azul.com/zulu/bin/zulu-repo_1.0.0-3_all.deb
RUN apt-get install ./zulu-repo_1.0.0-3_all.deb
RUN apt-get update

# Install openjdk 17
RUN apt-get install -y zulu17-jdk

# Install sbt
RUN apt-get install apt-transport-https curl gnupg -yqq
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
RUN chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
RUN apt-get update
RUN apt-get install -y sbt

ADD ./project/plugins.sbt /home/bot/project/plugins.sbt
ADD ./project/build.properties /home/bot/project/build.properties
ADD ./src /home/bot/src
ADD ./build.sbt /home/bot/build.sbt

RUN chown -R bot /home/bot

WORKDIR /home/bot

USER bot

RUN sbt "clean" "assembly"

ENTRYPOINT ["java", "-jar", "target/scala-3.1.1/scala-assembly-0.1.0-SNAPSHOT.jar"]
