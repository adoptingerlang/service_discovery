# syntax = docker/dockerfile:experimental
FROM us.gcr.io/adoptingerlang/erlang:22.1.1-alpine as builder

WORKDIR /app/src
ENV REBAR_BASE_DIR /app/_build

# Install git for fetching non-hex depenencies.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update git

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

# tar for unpacking the target system
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    apk add --update tar

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    tar -zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel

FROM us.gcr.io/adoptingerlang/alpine:3.10.2 as runner

WORKDIR /opt/service_discovery

ENV COOKIE=service_discovery \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # service_discovery specific env variables to act as defaults
    DB_HOST=127.0.0.1 \
    LOGGER_LEVEL=debug \
    SCHEDULERS=1

# openssl needed by the crypto app
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update openssl ncurses

COPY --from=releaser /opt/rel .

ENTRYPOINT ["/opt/service_discovery/bin/service_discovery"]
CMD ["foreground"]

# image to use in tilt when running the release
FROM builder as dev_release

COPY . .
RUN rebar3 as tilt release

ENTRYPOINT ["/app/_build/tilt/rel/service_discovery/bin/service_discovery"]
CMD ["foreground"]

FROM busybox as dev_sql

COPY apps/service_discovery_postgres/priv/migrations/ /app/sql/
