
FROM alpine:3.14

RUN apk add \
    bash\
    bubblewrap\
    coreutils\
    gcc\
    git\
    m4\
    make\
    musl-dev\
    opam

RUN opam init\
    --disable-sandboxing\
    --auto-setup\
    --compiler ocaml-base-compiler.4.13.1

WORKDIR /build
ADD Makefile /build/
ADD throttle-fstrim.opam /build/

RUN make deps
