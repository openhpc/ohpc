#!/bin/bash

container=${CONTAINER:-docker}
# shellcheck disable=SC2048,SC2086
exec "${container}" exec -it openhpc-login sudo -u "${USER}" -i $*
