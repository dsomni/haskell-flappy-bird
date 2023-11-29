# BUILDER
##############################################################################

FROM haskell:9.4.7 AS builder

WORKDIR /build

COPY . /build/

RUN stack build

RUN cp $(stack path --local-install-root)/bin/leaderboard \
    /sbin/


# DEPLOYMENT IMAGE
##############################################################################

FROM ubuntu:latest

WORKDIR /app

EXPOSE 8080

RUN apt update && apt install -y libpq-dev

COPY --from=builder /sbin/leaderboard /app/leaderboard

ENTRYPOINT /app/leaderboard