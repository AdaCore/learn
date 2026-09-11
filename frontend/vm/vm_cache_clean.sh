#!/bin/bash
#
# Remove entries that are no longer required from the VM download caches.
#
# The caches are handled by one script each: vm_cache_gnat.sh and
# vm_cache_apt.sh, which define what makes an entry no longer required and
# may also be invoked directly.
#
# Defaults to a dry run. The cache directories are configurable and may be
# located anywhere on the host, so removal must be requested explicitly.
#
# Usage:
#   vm_cache_clean.sh            # dry run -- show what would be removed
#   vm_cache_clean.sh --delete   # actually remove it
#
# Environment:
#   LEARN_VM_CACHE_GNAT / _APT   override the cache locations
#   (defaults: <repo>/.toolchains/{gnat,apt})

set -eu
set -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
gnat="${here}/vm_cache_gnat.sh"
apt="${here}/vm_cache_apt.sh"

args=()
case "${1:-}" in
  --delete)        args=(clean --delete) ;;
  -n|--dry-run|"") args=(clean) ;;
  -h|--help)       sed -n '3,18p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *)               echo "error: unknown option '$1'" >&2; exit 1 ;;
esac

# Each cache is processed even if another fails, so that one broken cache
# does not leave the rest uncleaned. The worst status is returned.
rc=0
"${gnat}" "${args[@]}" || rc=$?
echo
"${apt}" "${args[@]}" || rc=$?
exit "${rc}"
