FROM ocaml/opam:alpine
USER root
# Install dependencies
RUN apk update && \
    apk add --no-cache libev gmp-dev libev-dev openssl-dev

USER opam
