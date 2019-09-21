#!/usr/bin/env bash

set -ex

# change working directory to top level of the project
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPT_DIR/..

type=image
push=
target=runner
registry=

usage() {
    echo "Usage: $0 [-p] [-l] [-t {builder|releaser|runner|plt}] [-r <registry>]"
    echo
    echo "  -p  Enable pushing images to registry after build"
    echo "  -t  Target image to build (default: runner)"
    echo "  -r  Registry to push images"
}

while getopts ":t:r:pl" opt; do
    case ${opt} in
        l )
            type="docker"
            ;;
        p )
            push="--push"
            ;;
        t )
            target=$OPTARG
            ;;
        r )
            registry=${OPTARG}/
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

IMAGE=${registry}service_discovery
BUILD_IMAGE=${IMAGE}:builder
RELEASER_IMAGE=${IMAGE}:releaser
PLT_IMAGE=${IMAGE}:plt

GIT_REF=$(git rev-parse HEAD) # or with --short
GIT_BRANCH=$(git symbolic-ref --short HEAD)

# because of issues with using buildx as a plugin in CircleCI,
# buildx is called directly in this script
~/.docker/cli-plugins/docker-buildx build --target $target \
       $push \
       -o type=$type \
       --tag ${IMAGE}:${GIT_BRANCH} \
       --tag ${IMAGE}:${GIT_REF} \
       --cache-from=${IMAGE}:${GIT_BRANCH} \
       --cache-from=${IMAGE}:master \
       --cache-to=type=inline \
       .

echo "Finished building ${IMAGE}:${GIT_REF}"
