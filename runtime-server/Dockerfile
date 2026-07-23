# Builds and runs the standalone VSM runtime-server (headless, RUNTIME_ONLY mode).
# See doc/runtime-server.md and deploy/README.md for usage.

FROM gradle:8.7-jdk21 AS build
COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle :runtime-server:jar --no-daemon

FROM eclipse-temurin:17-jre
RUN mkdir /app
COPY --from=build /home/gradle/src/runtime-server/build/libs/runtime-server-*.jar /app/runtime-server.jar
COPY runtime-server/docker-entrypoint.sh /app/docker-entrypoint.sh
RUN chmod +x /app/docker-entrypoint.sh

WORKDIR /app
ENTRYPOINT ["/app/docker-entrypoint.sh"]
