# syntax = docker/dockerfile:experimental
FROM erlang:22-alpine as builder

# git for fetching non-hex depenencies
# add any other Alpine libraries needed to compile the project here
RUN apk add --no-cache git

WORKDIR /app/src

COPY rebar3 /usr/local/bin/

ENV REBAR_BASE_DIR /app/_build

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 compile

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 compile

FROM builder as releaser

# tar for unpacking the target system
RUN apk add --no-cache tar && \
    mkdir -p /opt/rel

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    tar -zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.9 as runner

# install openssl, needed by the crypto app
RUN apk add --no-cache openssl ncurses

WORKDIR /opt/service_discovery

COPY --from=releaser /opt/rel .

ENV COOKIE service_discovery
# write files generated during startup to /tmp
ENV RELX_OUT_FILE_PATH /tmp

ENTRYPOINT ["/opt/service_discovery/bin/service_discovery"]
CMD ["foreground"]

# image for running common test suites
FROM builder as tester

RUN apk add --no-cache py-pip python-dev libffi-dev openssl-dev gcc libc-dev make && \
    pip install docker-compose

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 as test compile

ENTRYPOINT ["rebar3"]
CMD ["ct"]

# image for caching dialyzer plt
FROM builder as plt

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 dialyzer --plt-location /root/.cache/rebar3 \
    --plt-prefix deps \
    --base-plt-prefix otp

ENTRYPOINT ["rebar3"]
CMD ["dialyzer", "--plt-location", "/root/.cache/rebar3", "--plt-prefix", "deps", "--base-plt-prefix", "otp"]

# image to use in tilt when running the release
FROM builder as devrel

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,target=/root/.cache/rebar3 \
    rebar3 release

ENTRYPOINT ["/src/_build/default/rel/service_discovery/bin/service_discovery"]
CMD ["foreground"]
