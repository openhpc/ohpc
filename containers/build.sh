#!/bin/bash

container=${CONTAINER:-docker}
user=${USER:-${USER}}
arch=$(uname -m)
modules_package=${MODULES_PACKAGE:-lmod-ohpc}

echo "=== setup ${container}"
"${container}" volume create openhpc-container-project
"${container}" network create openhpc-container-network

set -e
echo '=== build openhpc'
"${container}" build -t openhpc/openhpc:4 -f openhpc/Containerfile openhpc \
	--build-arg USER="${user}" \
	--build-arg ARCH="${arch/arm64/aarch64}" \
	--build-arg MODULES_PACKAGE="${modules_package}"

for I in container head node; do
	echo "=== build ${I}"
	build_args=(--build-arg USER="${user}" --build-arg ARCH="${arch/arm64/aarch64}")
	if [ "${I}" != head ]; then
		build_args+=(--build-arg MODULES_PACKAGE="${modules_package}")
	fi
	"${container}" build -t openhpc/"${I}" -f "${I}"/Containerfile "${build_args[@]}" "${I}"
done
