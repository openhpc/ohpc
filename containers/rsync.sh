#!/bin/bash

container=${CONTAINER:-docker}
exec rsync -e "$container exec -i --user $USER:$USER" $*
