# syntax = docker/dockerfile:experimental
FROM erlang:22-alpine as builder

# git for fetching non-hex depenencies
# add any other Alpine libraries needed to compile the project here
RUN apk add --no-cache git

WORKDIR /src

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=type=cache,target=/root/.cache/rebar3 rebar3 compile

COPY . .
RUN --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 compile

FROM builder as releaser

# tar for unpacking the target system
RUN apk add --no-cache tar

WORKDIR /src
RUN mkdir -p /opt/rel

RUN --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    tar -zxvf /src/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.9 as runner

# install openssl, needed by the crypto app
RUN apk add --no-cache openssl ncurses

WORKDIR /opt/service_discovery

COPY --from=releaser /opt/rel /opt/service_discovery

ENV COOKIE service_discovery
# write files generated during startup to /tmp
ENV RELX_OUT_FILE_PATH /tmp

ENTRYPOINT ["/opt/service_discovery/bin/service_discovery"]

CMD ["foreground"]

FROM builder as plt

RUN --mount=type=cache,target=/root/.cache/rebar3 \
    rebar3 dialyzer --plt-location /root/.cache/rebar3 \
    --plt-prefix deps \
    --base-plt-prefix otp

ENTRYPOINT ["rebar3"]

CMD ["dialyzer", "--plt-location", "/root/.cache/rebar3", "--plt-prefix", "deps", "--base-plt-prefix", "otp"]
