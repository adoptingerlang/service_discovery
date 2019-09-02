#!/usr/bin/env bash

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPT_DIR/..

push=false
target=all
registry=tsloughter

usage() {
    echo "Usage: $0 [-p] [-t {builder|releaser|runner|plt|all}] [-r <registry>]"
    echo
    echo "  -p  Enable pushing images to registry after build"
    echo "  -t  Target image to build (default: all)"
    echo "  -r  Registry to push images"
}

while getopts ":t:r:p" opt; do
    case ${opt} in
        p )
            push=true
            ;;
        t )
            target=$OPTARG
            ;;
        r )
            registry=$OPTARG
            ;;
        : )
            echo "Invalid Option: -$OPTARG requires an argument" 1>&2
            exit 1
            ;;
        \? )
            usage
            exit 1
            ;;
    esac
done
shift $((OPTIND -1))

# must export this so experimental Dockerfile features work
export DOCKER_BUILDKIT=1

IMAGE=${registry}/service_discovery
BUILD_IMAGE=${IMAGE}:builder
RELEASER_IMAGE=${IMAGE}:releaser
PLT_IMAGE=${IMAGE}:plt

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

    ! $push || docker push $image
}

case ${target} in
    all )
        build_and_push builder "$BUILD_IMAGE-$CHKSUM"
        build_and_push releaser "$RELEASER_IMAGE-$GIT_REF" "$BUILD_IMAGE-$CHKSUM"
        build_and_push runner "$IMAGE:$GIT_REF" "$BUILD_IMAGE-$CHKSUM" "$RELEASER_IMAGE-$GIT_REF"
        build_and_push plt "$PLT_IMAGE-$CHKSUM" "$BUILD_IMAGE-$CHKSUM"
        ;;
    builder )
        build_and_push builder "$BUILD_IMAGE-$CHKSUM"
        ;;
    releaser )
        build_and_push releaser "$RELEASER_IMAGE-$GIT_REF" "$BUILD_IMAGE-$CHKSUM"
        ;;
    runner )
        build_and_push runner "$IMAGE:$GIT_REF" "$BUILD_IMAGE-$CHKSUM" "$RELEASER_IMAGE-$GIT_REF"
        ;;
    plt )
        build_and_push plt "$PLT_IMAGE-$CHKSUM" "$BUILD_IMAGE-$CHKSUM"
        ;;
    * )
        echo "Invalid image target: ${target}" 1>&2
        usage
        exit 1
        ;;
esac

echo "Finished building ${IMAGE}:${GIT_REF}"
