#!/bin/bash

set -x
set -e

USER=$1
shift

COMPILER_FAMILY=$1
shift

PKG=("dnf" "-y")

if hash zypper > /dev/null 2>&1; then
	PKG=("zypper" "-n" "--no-gpg-checks")
fi

# First install slurm and needed packages
"${PKG[@]}" install \
	hostname \
	make \
	openssh-clients \
	which \
	sudo \
	ohpc-autotools \
	prun-ohpc \
	openmpi5-"${COMPILER_FAMILY}"-ohpc \
	mpich-"${COMPILER_FAMILY}"-ohpc \
	lmod-defaults-"${COMPILER_FAMILY}"-openmpi5-ohpc \
	slurm-slurmd-ohpc \
	slurm-slurmctld-ohpc \
	slurm-example-configs-ohpc \
	slurm-ohpc

# Install rebuilt packages (if any)
# shellcheck disable=SC2046 # (we want the words to be split)
"${PKG[@]}" install $(find /home/"${USER}"/rpmbuild/RPMS/ -name "*rpm") || true

# Remove OpenPBS, if it is installed as a dependency. The tests use Slurm Resource Manager
"${PKG[@]}" remove openpbs-*-ohpc || true
rm -f /etc/pbs.conf

# Setup slurm
echo "127.0.0.1 node0 node1" >> /etc/hosts

cp /etc/slurm/slurm.conf.example /etc/slurm/slurm.conf

sed -i -e "
	s,SlurmdLogFile=.*$,SlurmdLogFile=/var/log/slurmd.%n.log,g; \
	s,SlurmdSpoolDir=.*$,SlurmdSpoolDir=/var/spool/slurmd.%n,g; \
	s,SlurmdPidFile=.*$,SlurmdPidFile=/var/run/slurmd.%n.pid,g; \
	s,JobCompType=jobcomp/none,,g; \
	s,ProctrackType=.*,ProctrackType=proctrack/linuxproc,g; \
	s,TaskPlugin=.*,TaskPlugin=task/none,g; \
	s,NodeName=.*$,,g; \
	s,PartitionName.*$,,g; \
	s,ReturnToService.*$,ReturnToService=2,g; \
	s,SlurmctldHost=.*$,SlurmctldHost=${HOSTNAME},g;" /etc/slurm/slurm.conf

{
	echo "NodeName=c0 NodeHostname=node0 Port=17004 CPUs=2"
	echo "NodeName=c1 NodeHostname=node1 Port=17005 CPUs=2"
	echo "PartitionName=normal Nodes=c0,c1 Default=YES MaxTime=24:00:00 State=UP"
} >> /etc/slurm/slurm.conf

# cgroupv2 support does not yet work in containers.
# Force cgroupv1 even on hosts with v2.
echo "CgroupPlugin=cgroup/v1" >  /etc/slurm/cgroup.conf

chown root.root /var/log/munge

mkdir -p /run/munge

/usr/sbin/munged -f
/usr/sbin/slurmctld
slurmd -N c0 || cat /var/log/slurm*
slurmd -N c1 || cat /var/log/slurm*

sinfo

retry_counter=0
max_retries=5

while true; do
	(( retry_counter+=1 ))
	if [ "${retry_counter}" -gt "${max_retries}" ]; then
		exit 1
	fi
	scontrol update nodename=c[0-1] state=idle && break
	sinfo

	# In case it is a network error let's wait a bit.
	echo "Retrying scontrol attempt ${retry_counter}"
	sleep "${retry_counter}"
done

srun -N2 hostname

# Figure out which tests we need to run.
# This script returns three array variables:
#  PKGS and TESTS and ADMIN_TESTS
# shellcheck disable=SC2068 # (we want individual elements)
eval "$(tests/ci/spec_to_test_mapping.py $@)"

if [ "${#PKGS[@]}" -gt 0 ]; then
	"${PKG[@]}" install "${PKGS[@]}"
fi

export SIMPLE_CI=1
TESTS_FAILED=1

set +e

sudo --user="${USER}" --login bash -c "cd ${PWD}/tests; find ./ -name '*.log' -delete"

# Always running at least with '--enable-modules'. No need to check for
# an empty TESTS array.
if sudo \
	--user="${USER}" \
	--preserve-env=SIMPLE_CI \
	--login \
	bash -c "\
		cd ${PWD}/tests; \
		./bootstrap; \
		./configure \
			--disable-all \
			--enable-modules \
			--enable-rms-harness \
			--enable-compilers \
			--with-compiler-families='${COMPILER_FAMILY}' \
			--with-mpi-families='openmpi5 mpich' \
			${TESTS[*]}; \
		make check";
then
    TESTS_FAILED=0
fi


if [ "${#ADMIN_TESTS[@]}" -gt 0 ]; then
	# The configure script uses the variable $USER to decide if root or not
	export USER=root
	cd tests
	./bootstrap
	./configure --disable-all --disable-bos --disable-oob --disable-spack "${ADMIN_TESTS[*]}"
	if ! make check; then
		TESTS_FAILED=1
	fi
fi

if [ "${TESTS_FAILED}" -eq 0 ]; then
	exit 0
fi

set -e

# If we are here, the tests failed. Print the logs and exit with an error code.
echo -e "\nThe tests execution failed. Printing the logs.\n"
find ./ -name "*.log" -print0 | while IFS= read -r -d '' log_file
do
	echo "================================================"
	echo "Log file: ${log_file}";
	cat "${log_file}";
done
exit 1
