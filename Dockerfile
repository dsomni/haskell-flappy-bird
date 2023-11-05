# BUILDER
##############################################################################

FROM haskell:9.2.8 AS builder

WORKDIR /build

COPY . /build/

RUN stack build

ENTRYPOINT [ "stack", "run" ]


# TODO:
# # DEPLOYMENT IMAGE
# ##############################################################################

# FROM haskell:9.2.8

# WORKDIR /app

# EXPOSE 3000


# COPY --from=builder /build/artifacts/$EXECUTABLE /app/$EXECUTABLE

# # Set up a default command to run
# ENTRYPOINT /app/${EXECUTABLE_}