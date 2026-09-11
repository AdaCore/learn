#!/bin/bash
#
# Remove orphaned entries from the toolchain download cache -- tarballs whose
# version is no longer listed in toolchain.ini, and .part files left behind by
# an interrupted download.
#
# Defaults to a dry run. The cache directory is user-configurable and may sit
# anywhere on the host, so deleting requires saying so explicitly.
#
# The orphan list comes from vm_cache_report.sh, which derives it from the
# same toolchain.ini the provisioner reads.
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
report="${here}/vm_cache_report.sh"

delete=false
case "${1:-}" in
  --delete)      delete=true ;;
  -n|--dry-run|"") ;;
  -h|--help)     sed -n '3,18p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *)             echo "error: unknown option '$1'" >&2; exit 1 ;;
esac

orphans=$("${report}" --orphans)

if [ -z "${orphans}" ]; then
  echo "No orphaned cache entries."
  exit 0
fi

count=$(echo "${orphans}" | wc -l)

if [ "${delete}" = true ]; then
  echo "${orphans}" | while read -r f; do
    echo "removing ${f}"
    rm -f "${f}"
  done
  echo "Removed ${count} entr$([ "${count}" -eq 1 ] && echo y || echo ies)."
else
  echo "Would remove ${count} orphaned entr$([ "${count}" -eq 1 ] && echo y || echo ies):"
  echo "${orphans}" | while read -r f; do
    printf '  %-8s %s\n' "$(du -h "${f}" 2>/dev/null | cut -f1)" "${f}"
  done
  echo
  echo "This was a dry run. Re-run with --delete to remove them."
fi
