FROM gradle:6.3.0-jdk13 AS build
COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle build --no-daemon 

FROM openjdk

EXPOSE 8080

RUN mkdir /app

COPY --from=build /home/gradle/src/build/libs/*.jar /app/vsm.jar

ENTRYPOINT ["java", "-jar","/app/vsm.jar", "runtime", "/app/project"]
