#!/bin/bash
# Integration test for OpenHPC Warewulf/Slurm recipe on EL-based distros.
#
# Runs the full recipe inside a VM (or container with KVM access),
# provisioning QEMU-based compute nodes via PXE and verifying they boot.
#
# Requirements:
#   - /dev/kvm access (nested virt or bare-metal)
#   - ~8 GB RAM, 4+ CPUs recommended
#
# Usage:
#   sudo bash tests/ci/integration-test-el.sh

# shellcheck disable=SC2034,SC2154

set -ex

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SHUTDOWN_VMS="${SHUTDOWN_VMS:-1}"

while getopts "k" opt; do
	case "${opt}" in
	k) SHUTDOWN_VMS=0 ;;
	*)
		echo "Usage: $0 [-k]" >&2
		exit 1
		;;
	esac
done

# ---------------------------------------------------------------------------
# System info
# ---------------------------------------------------------------------------
ls -la /dev/kvm 2>/dev/null || echo "WARNING: /dev/kvm not found"
lscpu

# ---------------------------------------------------------------------------
# Speed up dnf and reduce log noise
# ---------------------------------------------------------------------------
echo "max_parallel_downloads=10" >>/etc/dnf/dnf.conf
echo "debuglevel=1" >>/etc/dnf/dnf.conf

# ---------------------------------------------------------------------------
# Enable EPEL (not enabled by default on AlmaLinux Lima images)
# ---------------------------------------------------------------------------
dnf -y install epel-release libselinux-utils 'dnf-command(config-manager)'
dnf config-manager --set-enabled epel
dnf config-manager --set-enabled crb

# ---------------------------------------------------------------------------
# Prepare CI environment (installs OpenHPC repos, base packages)
# ---------------------------------------------------------------------------
bash "${SCRIPT_DIR}/prepare-ci-environment.sh"

# ---------------------------------------------------------------------------
# Add local RPM repository (if /tmp/RPMS exists)
# ---------------------------------------------------------------------------
if [ -d /tmp/RPMS ]; then
	createrepo_c /tmp/RPMS
	cat >/etc/yum.repos.d/local-rpms.repo <<-'EOF'
		[local-rpms]
		name=Local RPMs
		baseurl=file:///tmp/RPMS
		enabled=1
		gpgcheck=0
		priority=1
	EOF
fi

# ---------------------------------------------------------------------------
# Install docs-ohpc (provides recipe.sh + input.local template)
# ---------------------------------------------------------------------------
dnf -y install docs-ohpc

# ---------------------------------------------------------------------------
# Install VM and networking dependencies
# ---------------------------------------------------------------------------
dnf -y install qemu-kvm ipxe-roms-qemu iproute initscripts-service

# ---------------------------------------------------------------------------
# Compute node definitions
# ---------------------------------------------------------------------------
num_computes=2
num_cpus=6

c_name[0]=c1
c_name[1]=c2

c_ip[0]=192.168.100.100
c_ip[1]=192.168.100.101

c_mac[0]=52:54:00:00:01:00
c_mac[1]=52:54:00:00:01:01

c_bmc[0]=10.16.1.1
c_bmc[1]=10.16.1.2

# ---------------------------------------------------------------------------
# Network setup: bridge + TAP interfaces
# ---------------------------------------------------------------------------
ip link add br0 type bridge
ip addr add 192.168.100.1/24 dev br0
ip link set br0 up

for ((i = 0; i < num_computes; i++)); do
	ip tuntap add "tap${i}" mode tap
	ip link set "tap${i}" up
	ip link set "tap${i}" master br0
done

echo "192.168.100.1 sms" >>/etc/hosts
hostnamectl hostname sms

# ---------------------------------------------------------------------------
# Generate input.local
# ---------------------------------------------------------------------------
RECIPE_DIR="/opt/ohpc/pub/doc/recipes/almalinux10"
INPUT_LOCAL="${RECIPE_DIR}/input.local"

cat >"${INPUT_LOCAL}" <<'INPUTEOF'
# -*-sh-*-
# Auto-generated input.local for integration testing

sms_name="${sms_name:-sms}"
sms_ip="${sms_ip:-192.168.100.1}"
sms_eth_internal="${sms_eth_internal:-br0}"

internal_netmask="${internal_netmask:-255.255.255.0}"
internal_prefix_length="${internal_prefix_length:-24}"
internal_network="${internal_network:-192.168.100.0}"

ipv4_gateway="${ipv4_gateway:-192.168.100.1}"
ntp_server="${ntp_server:-0.centos.pool.ntp.org}"

dns_servers="${dns_servers:-1.1.1.1}"
domain_name="${domain_name:-local}"

has_ipmi="${has_ipmi:-1}"
bmc_username="${bmc_username:-admin}"
bmc_password="${bmc_password:-password}"

provision_wait="${provision_wait:-0}"

eth_provision="${eth_provision:-eth0}"

enable_ib="${enable_ib:-0}"
enable_opa="${enable_opa:-0}"
enable_opafm="${enable_opafm:-0}"
enable_ipoib="${enable_ipoib:-0}"
enable_clustershell="${enable_clustershell:-0}"
enable_ipmisol="${enable_ipmisol:-0}"
enable_opensm="${enable_opensm:-0}"
enable_genders="${enable_genders:-0}"
enable_magpie="${enable_magpie:-0}"
enable_kargs="${enable_kargs:-0}"
enable_lustre_client="${enable_lustre_client:-0}"
enable_pmix="${enable_pmix:-0}"
enable_nhc="${enable_nhc:-0}"
enable_todisk="${enable_todisk:-0}"
enable_dracut="${enable_dracut:-0}"
enable_mpi_defaults="${enable_mpi_defaults:-1}"
enable_mpich_ucx="${enable_mpich_ucx:-1}"
enable_intel_packages="${enable_intel_packages:-0}"
update_slurm_nodeconfig="${update_slurm_nodeconfig:-1}"

num_computes="${num_computes:-2}"
compute_regex="${compute_regex:-c*}"
compute_prefix="${compute_prefix:-c}"

c_name[0]=${c_name[0]:-c1}
c_name[1]=${c_name[1]:-c2}

c_ip[0]=${c_ip[0]:-192.168.100.100}
c_ip[1]=${c_ip[1]:-192.168.100.101}

c_mac[0]=${c_mac[0]:-52:54:00:00:01:00}
c_mac[1]=${c_mac[1]:-52:54:00:00:01:01}

c_bmc[0]=${c_bmc[0]:-10.16.1.1}
c_bmc[1]=${c_bmc[1]:-10.16.1.2}

num_cpus="${num_cpus:-6}"

slurm_node_config="${slurm_node_config:-NodeName=c[1-2] Sockets=1 CoresPerSocket=${num_cpus} ThreadsPerCore=1 State=UNKNOWN}"

kargs="${kargs:-acpi_pad.disable=1}"

ipoib_netmask="${ipoib_netmask:-255.255.0.0}"
sms_ipoib="${sms_ipoib:-192.168.0.1}"
c_ipoib[0]=${c_ipoib[0]:-192.168.1.1}
c_ipoib[1]=${c_ipoib[1]:-192.168.1.2}
INPUTEOF

# ---------------------------------------------------------------------------
# Install fake ipmitool
#
# The recipe calls ipmitool to power-reset compute nodes. We intercept
# "chassis power reset" and launch a QEMU VM on the corresponding TAP
# interface instead.
# ---------------------------------------------------------------------------
cat >/usr/local/bin/ipmitool <<'IPMIEOF'
#!/bin/bash
# Fake ipmitool: intercepts "chassis power reset" to launch QEMU VMs.

# BMC address -> compute index mapping
declare -A BMC_TO_IDX=(
	[10.16.1.1]=0
	[10.16.1.2]=1
)

# MAC addresses per compute index
declare -a MACS=(
	"52:54:00:00:01:00"
	"52:54:00:00:01:01"
)

# Compute node names (for log prefixing)
declare -a NAMES=(
	"c1"
	"c2"
)

# Parse the BMC host from arguments (-H <host>)
bmc_host=""
args=("$@")
for ((j = 0; j < ${#args[@]}; j++)); do
	if [[ "${args[$j]}" == "-H" ]]; then
		bmc_host="${args[$((j + 1))]}"
		break
	fi
done

# Check if this is a "chassis power reset" command
is_power_reset=0
for ((j = 0; j < ${#args[@]} - 2; j++)); do
	if [[ "${args[$j]}" == "chassis" && \
		"${args[$((j + 1))]}" == "power" && \
		"${args[$((j + 2))]}" == "reset" ]]; then
		is_power_reset=1
		break
	fi
done

if [[ "${is_power_reset}" -ne 1 ]] || [[ -z "${bmc_host}" ]]; then
	# Not a power reset or no host specified — try real ipmitool if available
	if command -v /usr/sbin/ipmitool > /dev/null 2>&1; then
		exec /usr/sbin/ipmitool "$@"
	fi
	echo "fake ipmitool: ignoring: $*" >&2
	exit 0
fi

idx="${BMC_TO_IDX[${bmc_host}]}"
if [[ -z "${idx}" ]]; then
	echo "fake ipmitool: unknown BMC host ${bmc_host}" >&2
	exit 1
fi

mac="${MACS[${idx}]}"
name="${NAMES[${idx}]}"
tap="tap${idx}"

echo "fake ipmitool: launching QEMU VM ${name} on ${tap} (mac=${mac})"

# Find the iPXE ROM
ROMFILE=""
for candidate in \
	/usr/share/ipxe/qemu/pxe-virtio.rom \
	/usr/share/ipxe/virtio-net.rom \
	/usr/share/qemu/pxe-virtio.rom; do
	if [[ -f "${candidate}" ]]; then
		ROMFILE="${candidate}"
		break
	fi
done

# Architecture-specific QEMU flags
ARCH_FLAGS=()
case "$(uname -m)" in
aarch64)
	ARCH_FLAGS=(
		-M virt,gic-version=max
		-drive "if=pflash,format=raw,readonly=on,file=/usr/share/AAVMF/AAVMF_CODE.fd"
	)
	;;
esac

NETDEV_OPTS="tap,id=net0,ifname=${tap},script=no,downscript=no"
DEVICE_OPTS="virtio-net-pci,netdev=net0,mac=${mac}"
if [[ -n "${ROMFILE}" ]]; then
	DEVICE_OPTS+=",romfile=${ROMFILE}"
fi

/usr/libexec/qemu-kvm \
	"${ARCH_FLAGS[@]}" \
	-pidfile "/tmp/vm-${idx}.pid" \
	-m 4096 -smp "${num_cpus}" \
	-accel kvm -cpu host \
	-boot n \
	-netdev "${NETDEV_OPTS}" \
	-device "${DEVICE_OPTS}" \
	-device virtio-balloon,deflate-on-oom=on,free-page-reporting=on \
	-device virtio-rng-pci,rng=rng0 \
	-object rng-random,id=rng0,filename=/dev/urandom \
	-display none -vga none -machine graphics=off \
	-chardev "stdio,id=char0,mux=on" \
	-serial chardev:char0 \
	-device virtio-serial-pci \
	-device virtconsole,chardev=char0 \
	-mon chardev=char0 \
	< /dev/null 2>&1 |
	sed -u "s/\x1b\[[0-9;]*[a-zA-Z]//g; s/\r//g" |
	awk '{ printf "[%s %s]: %s\n", "'"${name}"'", strftime("%H:%M:%S"), $0; fflush() }' &

echo "fake ipmitool: QEMU VM ${name} launched (pid=$!)"
IPMIEOF

sed -i "s/\${num_cpus}/${num_cpus}/" /usr/local/bin/ipmitool
chmod +x /usr/local/bin/ipmitool

# ---------------------------------------------------------------------------
# Run the recipe
# ---------------------------------------------------------------------------
export PATH=/usr/local/bin:${PATH}
export OHPC_INPUT_LOCAL="${INPUT_LOCAL}"
# shellcheck disable=SC2016
sed -i \
	-e 's/\(\["rd.shell"\]\)/\1 + ["console=hvc0", "loglevel=5"]/' \
	-e 's/\(systemctl restart rsyslog\)/\1 || true/' \
	-e 's|#<<< ohpc_proxy:compute >>>#|echo "max_parallel_downloads=10" >> $CHROOT/etc/dnf/dnf.conf\necho "debuglevel=1" >> $CHROOT/etc/dnf/dnf.conf|' \
	"${RECIPE_DIR}/x86_64/warewulf/slurm/recipe.sh"
if ! bash -x "${RECIPE_DIR}/x86_64/warewulf/slurm/recipe.sh"; then
	echo "Recipe failed – dumping journal since boot:"
	journalctl -b --no-pager
	exit 1
fi

# ---------------------------------------------------------------------------
# Wait for compute nodes to boot and become reachable via SSH
# ---------------------------------------------------------------------------
MAX_RETRIES=60
SLEEP_INTERVAL=10

all_up=1
for ((i = 0; i < num_computes; i++)); do
	echo "Waiting for ${c_name[${i}]} (${c_ip[${i}]}) to become reachable..."
	node_up=0
	for ((try = 1; try <= MAX_RETRIES; try++)); do
		if ssh -o StrictHostKeyChecking=no \
			-o UserKnownHostsFile=/dev/null \
			-o ConnectTimeout=5 \
			"${c_ip[${i}]}" hostname 2>/dev/null; then
			echo "${c_name[${i}]} is up after ${try} attempts"
			node_up=1
			break
		fi
		echo "  attempt ${try}/${MAX_RETRIES}..."
		sleep "${SLEEP_INTERVAL}"
	done
	if [[ "${node_up}" -ne 1 ]]; then
		echo "ERROR: ${c_name[${i}]} did not come up" >&2
		all_up=0
	fi
done

# ---------------------------------------------------------------------------
# Mark compute nodes as idle in Slurm
# ---------------------------------------------------------------------------
# shellcheck source=/dev/null
source "${INPUT_LOCAL}"
sinfo

retry_counter=0
max_retries=5

while true; do
	((retry_counter += 1))
	if [ "${retry_counter}" -gt "${max_retries}" ]; then
		exit 1
	fi
	scontrol update nodename=c[1-2] state=idle && break
	sinfo

	# In case it is a network error let's wait a bit.
	echo "Retrying scontrol attempt ${retry_counter}"
	sleep "${retry_counter}"
done

srun -N2 hostname

# ---------------------------------------------------------------------------
# Verify compute nodes
# ---------------------------------------------------------------------------
for ((i = 0; i < num_computes; i++)); do
	echo "--- Verification for ${c_name[${i}]} ---"
	ssh -o StrictHostKeyChecking=no \
		-o UserKnownHostsFile=/dev/null \
		"${c_ip[${i}]}" \
		'hostname; cat /etc/os-release; uname -a' 2>/dev/null || true
done

# ---------------------------------------------------------------------------
# Install and run OpenHPC test suite
# ---------------------------------------------------------------------------
dnf -y install test-suite-ohpc autoconf automake libtool

TESTS_FAILED=0

set +e

# Run user-level tests as ohpc-test
if runuser -l ohpc-test -c "\
		cd /home/ohpc-test/tests; \
		./bootstrap; \
		./configure \
			--disable-all \
			--enable-modules \
			--enable-rms-harness \
			--enable-compilers \
			--with-mpi-families='openmpi5 mpich'; \
		make -k check"; then
	echo "User-level tests passed"
else
	echo "User-level tests failed" >&2
	TESTS_FAILED=1
fi

# Run admin-level tests as root
export USER=root
cd /home/ohpc-test/tests/
./bootstrap
./configure --disable-all --disable-bos --disable-oob --disable-spack
if ! make -k check; then
	echo "Admin-level tests failed" >&2
	TESTS_FAILED=1
fi

set -e

if [ "${TESTS_FAILED}" -ne 0 ]; then
	echo -e "\nTest execution failed. Printing logs.\n"
	find /home/ohpc-test/tests/ -name "*.log" -print0 |
		while IFS= read -r -d '' log_file; do
			echo "================================================"
			echo "Log file: ${log_file}"
			cat "${log_file}"
		done
fi

# ---------------------------------------------------------------------------
# Cleanup: kill QEMU VMs
# ---------------------------------------------------------------------------
for ((i = 0; i < num_computes; i++)); do
	if [[ -f "/tmp/vm-${i}.pid" ]]; then
		pid=$(cat "/tmp/vm-${i}.pid")
		if [[ "${SHUTDOWN_VMS}" -eq 1 ]]; then
			echo "Stopping VM ${i} (pid=${pid})"
			kill "${pid}" 2>/dev/null || true
		else
			echo "VM ${i} still running: kill ${pid}"
		fi
	fi
done

if [[ "${all_up}" -ne 1 ]]; then
	echo "FAIL: Not all compute nodes came up" >&2
	exit 1
fi

if [ "${TESTS_FAILED}" -ne 0 ]; then
	echo "FAIL: Test suite had failures" >&2
	exit 1
fi

echo "SUCCESS: All compute nodes booted and tests passed"
