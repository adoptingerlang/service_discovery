#!/usr/bin/env bash

set -e

# pass false as first argument to disable pushing built images
PUSH=${1:-true}

# must export this so experimental Dockerfile features work
export DOCKER_BUILDKIT=1

IMAGE=tsloughter/service_discovery
BUILD_IMAGE=${IMAGE}_builder
RELEASER_IMAGE=${IMAGE}_releaser
PLT_IMAGE=${IMAGE}_plt

CHKSUM_CMD=${CHKSUM_CMD:-cksum}

CHKSUM=$(cat rebar.config rebar.lock | $CHKSUM_CMD | awk '{print $1}')
GIT_REF=$(git rev-parse HEAD) # or with --short

# takes the name of the image to build and the names of images to use as caches
build_and_push() {
    local target=$1
    shift
    local image=$1
    local cache_images=("$@")

    # prepend --cache-from to each image to use as caches
    cache_images=( "${cache_images[@]/#/--cache-from=}" )

    docker build --target $target --tag $image --cache-from=$image "${cache_images[@]}" \
           --build-arg BUILDKIT_INLINE_CACHE=true -f Dockerfile .

    ! $PUSH || docker push $image
}

build_and_push builder "$BUILD_IMAGE:$CHKSUM"
build_and_push releaser "$RELEASER_IMAGE:$GIT_REF" "$BUILD_IMAGE:$CHKSUM"
build_and_push runner "$IMAGE:$GIT_REF" "$BUILD_IMAGE:$CHKSUM" "$RELEASER_IMAGE:$GIT_REF"
build_and_push plt "$PLT_IMAGE:$CHKSUM" "$BUILD_IMAGE:$CHKSUM"

echo "Finished building ${IMAGE}:${GIT_REF}"
