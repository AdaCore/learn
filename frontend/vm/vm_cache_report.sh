#!/bin/bash
#
# Report on the VM download caches.
#
# Prints one summary line per cache, followed by the detailed report of each
# cache that provides one.
#
# The caches are handled by one script each: vm_cache_gnat.sh and
# vm_cache_apt.sh. Either may also be invoked directly.
#
# Usage:
#   vm_cache_report.sh            # all caches
#   vm_cache_report.sh --orphans  # just the orphan paths, one per line
#
# Environment:
#   LEARN_VM_CACHE_GNAT / _APT   override the cache locations
#   (defaults: <repo>/.toolchains/{gnat,apt})

set -eu
set -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

gnat="${here}/vm_cache_gnat.sh"
apt="${here}/vm_cache_apt.sh"

case "${1:-}" in
  --orphans)
    "${gnat}" orphans
    "${apt}" orphans
    exit 0
    ;;
  -h|--help)
    sed -n '3,17p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
    exit 0
    ;;
  "") ;;
  *)  echo "error: unknown option '$1'" >&2; exit 1 ;;
esac

printf '%-8s %-8s %s\n' "CACHE" "SIZE" "LOCATION"
"${gnat}" summary
"${apt}" summary

echo
echo "GNAT toolchain cache"
echo "--------------------"
"${gnat}" report

echo
echo "apt package cache"
echo "-----------------"
"${apt}" report
