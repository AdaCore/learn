#!/bin/bash
#
# Remove entries that are no longer required from the VM download caches.
#
# The caches are handled by one script each. Removal is currently implemented
# for the GNAT toolchain cache only; see vm_cache_gnat.sh for the definition
# of an entry that is no longer required.
#
# Defaults to a dry run. The cache directories are configurable and may be
# located anywhere on the host, so removal must be requested explicitly.
#
# Usage:
#   vm_cache_clean.sh            # dry run -- show what would be removed
#   vm_cache_clean.sh --delete   # actually remove it
#
# Environment:
#   LEARN_VM_CACHE_GNAT   cache directory (default: <repo>/.toolchains/gnat)

set -eu
set -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
gnat="${here}/vm_cache_gnat.sh"

case "${1:-}" in
  --delete)        exec "${gnat}" clean --delete ;;
  -n|--dry-run|"") exec "${gnat}" clean ;;
  -h|--help)       sed -n '3,17p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *)               echo "error: unknown option '$1'" >&2; exit 1 ;;
esac
