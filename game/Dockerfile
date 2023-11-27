FROM ytaka23/stack-ghcjs:lts-7.19 AS builder

WORKDIR /usr/lib/gcc/x86_64-linux-gnu/5.4.0
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o

COPY ./ /work
WORKDIR /work

RUN stack build --stack-yaml stack-js.yaml
RUN cp -r $(stack path --stack-yaml stack-js.yaml --local-install-root)/bin/haskell-flappy-bird.jsexe \
    /sbin/

RUN cp -f web/index.html /sbin/haskell-flappy-bird.jsexe

RUN git clone https://github.com/google/codeworld.git
WORKDIR /work/codeworld
RUN git reset --hard 277d051

RUN cp -r web/js /sbin/haskell-flappy-bird.jsexe
RUN cp -r web/css /sbin/haskell-flappy-bird.jsexe

FROM nginx:latest

COPY --from=builder \
    /sbin/haskell-flappy-bird.jsexe \
    /usr/share/nginx/html/