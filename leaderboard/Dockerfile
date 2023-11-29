# BUILDER
##############################################################################

FROM haskell:9.4.7 AS builder

WORKDIR /build

COPY . /build/

RUN stack build

RUN cp $(stack path --stack-yaml stack-js.yaml --local-install-root)/bin/leaderboard \
    /sbin/


# DEPLOYMENT IMAGE
##############################################################################

FROM alpine:latest

WORKDIR /app

EXPOSE 8080

COPY --from=builder /sbin/leaderboard /app/leaderboard

ENTRYPOINT /app/leaderboard