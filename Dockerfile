FROM erlang:21-alpine as builder

# git for fetching non-hex depenencies
# tar for unpacking the target system
RUN apk add --no-cache tar git

WORKDIR /src

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock /src/
RUN rebar3 compile

# copy in the source and build the release tarball
COPY . /src
RUN rebar3 as prod tar

# unpack tarball to be copied into the image built next
RUN mkdir -p /opt/rel
RUN tar -zxvf /src/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.9

# install openssl, needed by the crypto app
RUN apk add --no-cache openssl ncurses

WORKDIR /opt/service_discovery

ENV NODE 127.0.0.1
ENV COOKIE service_discovery

# COPY --chown=1000:100 --from=builder /opt/rel /opt/service_discovery
COPY --from=builder /opt/rel /opt/service_discovery
# ENV HOME /opt/tmpfs

ENTRYPOINT ["/opt/service_discovery/bin/service_discovery"]

CMD ["foreground"]

# docker run --mount type=tmpfs,dst=/opt/tmpfs,tmpfs-mode=0770 -ti --entrypoint /bin/sh 70424a7870ac
