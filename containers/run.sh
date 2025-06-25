#!/bin/bash

container=${CONTAINER:-docker}
CONTAINER="${container}" ./build.sh
network=--network=openhpc-container-network
volume=--volume=openhpc-container-project:/project

echo "=== Start cluster ${container}"
"${container}" run -d --rm "${network}" "${volume}" --name=openhpc-head --hostname=head openhpc/head
"${container}" run -d --rm "${network}" "${volume}" --name=openhpc-login --hostname=login openhpc/node
for I in {0..7}; do
	"${container}" run -d --rm "${network}" "${volume}" --name=openhpc-node-"${I}" --hostname=node-"${I}" openhpc/node
done

echo '=== Login Node (exit to shutdown cluster)'
./ssh.sh

echo '=== Stop cluster'
for I in {0..7}; do
	"${container}" kill openhpc-node-"${I}"
done
"${container}" kill openhpc-login
"${container}" kill openhpc-head
