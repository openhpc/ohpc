#!/bin/bash

container=${CONTAINER:-docker}
echo "=== delete.sh $container"

$container kill openhpc-login
for I in {0..7} ; do
  $container kill openhpc-node-$I
done
$container kill openhpc-head

$container volume rm openhpc-container-project
$container network rm openhpc-container-network
$container image rm openhpc/node:latest openhpc/head:latest openhpc/openhpc:3
