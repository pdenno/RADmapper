# syntax = docker/dockerfile:1.2
FROM clojure:openjdk-17 AS build

WORKDIR /
COPY . /

RUN clj -Sforce -T:build all-uber

FROM azul/zulu-openjdk-alpine:17

COPY --from=build /target/rm-server-standalone.jar rm-server-standalone.jar
#  Use the next and comment out the RUN clj and copy--from=build if you want to reuse iteratively
#  a build you made separately. (Most the the execution time here concern making the uberjar.)
#ADD target/rm-server-standalone.jar /

# Note that you have a databases directory at the same level as this Dockerfile.
# I cp -r /opt/messaging/databases .
RUN mkdir -p /opt/messaging/databases
ADD databases* /opt/messaging/databases

EXPOSE 3000

ENV RM_MESSAGING "/opt/messaging"
# https://vsupalov.com/docker-build-pass-environment-variables/
ARG LLM_KEY "unset"
ENV OPENAI_API_KEY $LLM_KEY

ENTRYPOINT exec java $JAVA_OPTS -jar rm-server-standalone.jar
# Things like the following for debugging:
#ENTRYPOINT printenv ; ls -la ; ls -la /opt/messaging/databases ; echo "LLM_KEY =" $LLM_KEY
