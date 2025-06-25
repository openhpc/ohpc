#!/bin/bash

container=${CONTAINER:-docker}
# shellcheck disable=SC2048,SC2086
exec rsync -e "${container} exec -i --user ${USER}:${USER}" $*
