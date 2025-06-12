#!/bin/bash

container=${CONTAINER:-docker}
user=${USER:-$USER}
arch=$(uname -m)

echo "=== setup $container"
$container volume create openhpc-container-project
$container network create openhpc-container-network

set -e
echo '=== build openhpc'
$container build -t openhpc/openhpc:3 -f openhpc/Containerfile openhpc \
  --build-arg USER=$user \
  --build-arg ARCH=${arch/arm64/aarch64}

for I in container head node ; do
  echo "=== build $I"
  $container build -t openhpc/$I -f $I/Containerfile --build-arg USER=$user $I
done
