#!/bin/bash

container=${CONTAINER:-docker}
exec $container exec -it openhpc-login sudo -u $USER -i $*
