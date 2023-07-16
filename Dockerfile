# syntax = docker/dockerfile:1.2
FROM clojure:openjdk-17 AS build

WORKDIR /
COPY . /

#RUN clj -Sforce -T:build all-uber

FROM azul/zulu-openjdk-alpine:17

#COPY --from=build /target/rm-server-standalone.jar rm-server-standalone.jar
#  Use the next and comment out the RUN clj and copy--from=build if you want to reuse iteratively a build you made separately.
ADD target/rm-server-standalone.jar /

# Note that you have a databases directory at the same level as this Dockerfile.
# I cp -r /opt/messaging/databases .
RUN mkdir -p /opt/messaging/databases
RUN mkdir -p /opt/messaging/etc
ADD databases* /opt/messaging/databases
ADD data/key-pairs.edn /opt/messaging/etc/key-pairs.edn

EXPOSE 3000

ENV RM_MESSAGING "/opt/messaging"

ENTRYPOINT exec java $JAVA_OPTS -jar rm-server-standalone.jar
# Things like the following for debugging. Of course, you also login to it.
#ENTRYPOINT printenv ; ls -la /opt/messaging ; ls -la /opt/messaging/etc
