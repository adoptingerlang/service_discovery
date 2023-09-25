# syntax=docker/dockerfile:1.2
FROM ghcr.io/adoptingerlang/service_discovery/erlang:26.0.2 as builder

WORKDIR /app/src
ENV REBAR_BASE_DIR /app/_build

RUN rm -f /etc/apt/apt.conf.d/docker-clean

# Install git for fetching non-hex depenencies.
# Add any other Debian libraries needed to compile the project here.
RUN --mount=target=/var/lib/apt/lists,id=apt-lists,type=cache,sharing=locked \
    --mount=type=cache,id=apt,target=/var/cache/apt \
    apt update && apt install --no-install-recommends -y git

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 compile

FROM builder as prod_compiled

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as prod compile

FROM prod_compiled as releaser

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    tar -zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel

FROM ghcr.io/adoptingerlang/service_discovery/debian:bullseye as runner

WORKDIR /opt/service_discovery

ENV COOKIE=service_discovery \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # service_discovery specific env variables to act as defaults
    DB_HOST=127.0.0.1 \
    LOGGER_LEVEL=debug

RUN rm -f /etc/apt/apt.conf.d/docker-clean

# openssl needed by the crypto app
RUN --mount=target=/var/lib/apt/lists,id=apt-lists,type=cache,sharing=locked \
    --mount=type=cache,id=apt,sharing=locked,target=/var/cache/apt \
    apt update && apt install --no-install-recommends -y openssl ncurses-bin

COPY --from=releaser /opt/rel .

ENTRYPOINT ["/opt/service_discovery/bin/service_discovery"]
CMD ["foreground"]

