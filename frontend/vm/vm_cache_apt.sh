#!/bin/bash
#
# Operate on the cache of apt package files.
#
# Reports the location and size of the cache.
#
# The cache holds the .deb files apt downloaded during provisioning, so that
# rebuilding a VM does not fetch them again. Both VMs use the same cache.
#
# Invoked by vm_cache_report.sh. May also be invoked directly to operate on
# this cache alone.
#
# Usage:
#   vm_cache_apt.sh summary   # one line for the caches table
#
# Environment:
#   LEARN_VM_CACHE_APT   cache directory (default: <repo>/.toolchains/apt)

set -eu
set -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "${here}/../.." && pwd)

# Expand a cache location against the repository root, matching how the
# Vagrantfile expands it against its own directory. An absolute value is used
# as-is; a relative one must not depend on the caller's working directory.
abspath () {
  case "$1" in
    /*) echo "$1" ;;
    *)  echo "${repo}/$1" ;;
  esac
}

cache=$(abspath "${LEARN_VM_CACHE_APT:-.toolchains/apt}")

size_of () {
  if [ -d "$1" ]; then du -sh "$1" 2>/dev/null | cut -f1; else echo "-"; fi
}

do_summary () {
  printf '%-8s %-8s %s\n' "apt" "$(size_of "${cache}")" "${cache}"
}

case "${1:-}" in
  summary)  do_summary ;;
  -h|--help|"")
    sed -n '3,17p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *)
    echo "error: unknown command '$1'" >&2; exit 1 ;;
esac
