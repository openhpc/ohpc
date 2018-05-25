#!/bin/sh

set -o nounset
set -o pipefail

. /etc/sysconfig/msr-safe

wl_cpu() {
  printf 'wl_%.2x%x\n' \
  $( tr -d "\t " < /proc/cpuinfo | grep -m1 'cpufamily:' | cut -f2 -d:) \
  $( tr -d "\t " < /proc/cpuinfo | grep -m1 'model:' | cut -f2 -d:)
}

start() {
  if [ -z "${WL_CPU:-}" ]; then
    WL_CPU=$(wl_cpu)
  fi

  if [ -z "${WHITELIST:-}" ]; then
    WHITELIST="/usr/share/msr-safe/whitelists/${WL_CPU}" 
  fi

  if [ -f "/usr/share/msr-safe/whitelists/${WL_CPU}" ]; then
    /sbin/modprobe msr-safe && \
    cat "${WHITELIST}" > /dev/cpu/msr_whitelist

    return $?
  else
    return 1
  fi
}

stop() {
    echo > /dev/cpu/msr_whitelist && \
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
