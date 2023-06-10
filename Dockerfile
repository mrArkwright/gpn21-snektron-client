FROM openjdk:17-alpine
RUN apk --no-cache add curl
WORKDIR /root/app
COPY target/scala-3.3.0/gpn21-snektron-client-assembly-0.1.0.jar .
CMD ["java", "-jar", "gpn21-snektron-client-assembly-0.1.0.jar"]
