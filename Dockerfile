FROM erlang:21-alpine as builder

RUN apk add --no-cache --update tar curl git bash make libc-dev gcc g++ vim

WORKDIR /src

# build and cache dependencies
COPY rebar.config rebar.lock /src/
RUN rebar3 compile

# copy in the source and build release
COPY . /src
RUN rebar3 as prod tar

RUN mkdir -p /opt/rel
RUN tar -zxvf /src/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.8

RUN apk add --no-cache openssl-dev ncurses

WORKDIR /opt/service_discovery

ENV NODE 127.0.0.1
ENV COOKIE service_discovery

COPY --chown=1000:100 --from=builder /opt/rel /opt/service_discovery
ENV HOME /opt/tmpfs

EXPOSE 8080 8080

ENTRYPOINT ["/opt/service_discovery/bin/service_discovery"]

CMD ["foreground"]

# docker run --mount type=tmpfs,dst=/opt/tmpfs,tmpfs-mode=0770 -ti --entrypoint /bin/sh 70424a7870ac
