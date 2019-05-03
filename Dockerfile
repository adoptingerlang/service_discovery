# syntax = docker/dockerfile:experimental
FROM erlang:21-alpine as builder

# git for fetching non-hex depenencies
# add any other Alpine libraries needed to compile the project here
RUN apk add --no-cache git

WORKDIR /src

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=type=cache,target=/root/.cache/rebar3 rebar3 compile

# RUN --mount=target=. \
#     --mount=type=cache,target=/tmp/service_discovery/_build \
#     rebar3 compile

FROM builder as releaser

# tar for unpacking the target system
RUN apk add --no-cache tar

WORKDIR /src
RUN mkdir -p /opt/rel

# copy in the source and build the release tarball
COPY . .
# unpack tarball to be copied into the image built next
# RUN --mount=target=. \
#     --mount=type=cache,target=/tmp/service_discovery/_build \
RUN rebar3 as prod tar && \
    tar -zxvf /src/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.9 as runner

# install openssl, needed by the crypto app
RUN apk add --no-cache openssl ncurses

WORKDIR /opt/service_discovery

# RUN addgroup -g 50 -S sd \
#     && adduser -D -S -h /opt/service_discovery -s /sbin/nologin \
#        -u 1000 -G sd sd
# USER sd:sd

COPY --from=releaser /opt/rel /opt/service_discovery

# RUN chown -R 1000:1000 /opt/service_discovery

# ENV COOKIE service_discovery
# write files generated during startup to /tmp
ENV RELX_OUT_FILE_PATH /tmp
# ENV HOME /tmp

ENTRYPOINT ["/opt/service_discovery/bin/service_discovery"]

CMD ["foreground"]

FROM builder as plt

RUN --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 dialyzer --plt-location /root/.cache/rebar3 \
    --plt-prefix deps \
    --base-plt-prefix otp

ENTRYPOINT ["rebar3"]

CMD ["dialyzer", "--plt-location", "/root/.cache/rebar3", "--plt-prefix", "deps", "--base-plt-prefix", "otp"]
