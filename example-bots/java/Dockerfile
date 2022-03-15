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

ADD ./gradlew /home/bot/gradlew
ADD ./gradle /home/bot/gradle
ADD ./settings.gradle /home/bot/settings.gradle
ADD ./app /home/bot/app
RUN chown -R bot /home/bot/

WORKDIR /home/bot

USER bot

RUN /home/bot/gradlew clean build

ENTRYPOINT ["java", "-jar", "app/build/libs/app-all.jar"]
