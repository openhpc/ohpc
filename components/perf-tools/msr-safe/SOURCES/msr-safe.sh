#!/bin/sh

set -o nounset
set -o pipefail

. /etc/sysconfig/msr-safe

al_cpu() {
  printf 'al_%.2x%x\n' \
  $(grep -m1 'cpu family' /proc/cpuinfo | cut -f2 -d: | tr -d ' ') \
  $(grep -m1 'model' /proc/cpuinfo | cut -f2 -d: | tr -d ' ')
}

start() {
  if [ -z "${AL_CPU:-}" ]; then
    AL_CPU=$(al_cpu)
  fi

  if [ -z "${ALLOWLIST:-}" ]; then
    ALLOWLIST="/usr/share/msr-safe/allowlists/${AL_CPU}"
  fi

  if [ -f "${ALLOWLIST}" ]; then
    /sbin/modprobe msr-safe && \
    cat "${ALLOWLIST}" > /dev/cpu/msr_allowlist

    return $?
  else
    return 1
  fi
}

stop() {
    echo > /dev/cpu/msr_allowlist && \
    /sbin/rmmod msr-safe

    return $?
}

rc=0

case "${1:-}" in
  start)
      start
      rc=$?
      ;;
  stop)
      stop
      rc=$?
      ;;
  *)
      echo $"Usage: $0 {start|stop}"
      exit 2
esac

exit $rc
