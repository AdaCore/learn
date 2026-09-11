#!/bin/bash
#
# Report on the VM download caches: size, contents, and which toolchain
# entries no longer correspond to a version listed in toolchain.ini.
#
# Dropping a version from toolchain.ini leaves its tarball behind and nothing
# prunes it, so orphans accumulate silently. This reports them;
# vm_cache_clean.sh removes them.
#
# The set of wanted versions is read from the same toolchain.ini the
# provisioner reads, so "orphan" cannot drift from "needed".
#
# Usage:
#   vm_cache_report.sh            # all caches
#   vm_cache_report.sh --orphans  # just the orphan paths, one per line
#
# Environment:
#   LEARN_VM_CACHE_GNAT / _NODE / _APT   override the cache locations
#   (defaults: <repo>/.toolchains/{gnat,node,apt})

set -eu
set -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "${here}/../.." && pwd)

toolchain_ini="${repo}/frontend/python/rst_code_example_pipeline/src/rst_code_example_pipeline/data/toolchain.ini"

cache_gnat="${LEARN_VM_CACHE_GNAT:-${repo}/.toolchains/gnat}"
cache_node="${LEARN_VM_CACHE_NODE:-${repo}/.toolchains/node}"
cache_apt="${LEARN_VM_CACHE_APT:-${repo}/.toolchains/apt}"

versions_of () {
  sed -n '/^\[toolchains\]/,/^\[/p' "${toolchain_ini}" \
    | sed -n "s/^$1[[:space:]]*=[[:space:]]*//p"
}

# Print the basenames the GNAT cache is supposed to contain.
wanted_names () {
  local tool ver
  for tool in gnat gnatprove gprbuild; do
    for ver in $(versions_of "${tool}"); do
      echo "${tool}-x86_64-linux-${ver}.tar.gz"
      echo "${tool}-x86_64-linux-${ver}.tar.gz.sha256"
    done
  done
}

# Anything in the GNAT cache that is not wanted. Leftover .part files count as
# orphans too: a complete download is always renamed into place, so one that
# survives is debris from an interrupted run.
orphans () {
  [ -d "${cache_gnat}" ] || return 0
  local wanted
  # LC_ALL=C throughout: comm compares byte-wise, so both inputs must be
  # sorted that way too. Locale collation ignores the '-', which puts
  # gnat-... and gnatprove-... in an order comm rejects.
  wanted=$(wanted_names | LC_ALL=C sort)
  find "${cache_gnat}" -maxdepth 1 -type f -printf '%f\n' 2>/dev/null \
    | LC_ALL=C sort \
    | LC_ALL=C comm -23 - <(echo "${wanted}") \
    | while read -r f; do echo "${cache_gnat}/${f}"; done
}

size_of () {
  if [ -d "$1" ]; then du -sh "$1" 2>/dev/null | cut -f1; else echo "-"; fi
}

if [ "${1:-}" = "--orphans" ]; then
  orphans
  exit 0
fi

if [ ! -f "${toolchain_ini}" ]; then
  echo "error: ${toolchain_ini} not found" >&2
  exit 1
fi

printf '%-8s %-8s %s\n' "CACHE" "SIZE" "LOCATION"
printf '%-8s %-8s %s\n' "gnat" "$(size_of "${cache_gnat}")" "${cache_gnat}"
printf '%-8s %-8s %s\n' "node" "$(size_of "${cache_node}")" "${cache_node}"
printf '%-8s %-8s %s\n' "apt"  "$(size_of "${cache_apt}")"  "${cache_apt}"

echo
echo "Toolchain versions wanted by toolchain.ini:"
for tool in gnat gnatprove gprbuild; do
  printf '  %-10s %s\n' "${tool}" "$(versions_of "${tool}" | tr -s ' ')"
done

echo
echo "Toolchain tarballs present:"
if [ -d "${cache_gnat}" ]; then
  find "${cache_gnat}" -maxdepth 1 -type f -name '*.tar.gz' -printf '  %f\n' \
    2>/dev/null | sort || true
else
  echo "  (cache directory does not exist yet)"
fi

echo
orphan_list=$(orphans)
if [ -z "${orphan_list}" ]; then
  echo "No orphaned entries."
else
  echo "Orphaned entries (not wanted by toolchain.ini):"
  echo "${orphan_list}" | while read -r f; do
    printf '  %-8s %s\n' "$(du -h "${f}" 2>/dev/null | cut -f1)" "${f##*/}"
  done
  echo
  echo "Remove them with: frontend/vm/vm_cache_clean.sh --delete"
fi
