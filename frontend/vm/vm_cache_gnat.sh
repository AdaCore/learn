#!/bin/bash
#
# Operate on the cache of GNAT toolchain tarballs.
#
# Downloads toolchain tarballs into the cache, reports its contents,
# identifies entries that are no longer required, and removes them.
#
# An entry is no longer required when its version is not listed in
# toolchain.ini, or when it is a .part file left behind by an interrupted
# download. The list of required versions is read from the same toolchain.ini
# the provisioner reads, so the two cannot diverge.
#
# Invoked by vm_cache_report.sh and vm_cache_clean.sh. May also be invoked
# directly to operate on this cache alone.
#
# Usage:
#   vm_cache_gnat.sh fetch <tool> <version>
#                                     # download one toolchain into the cache
#   vm_cache_gnat.sh fetch --all      # download every version in toolchain.ini
#   vm_cache_gnat.sh summary          # one line for the caches table
#   vm_cache_gnat.sh report           # versions, contents and orphans
#   vm_cache_gnat.sh orphans          # orphan paths, one per line
#   vm_cache_gnat.sh clean            # dry run -- what would be removed
#   vm_cache_gnat.sh clean --delete   # remove it
#
# Environment:
#   LEARN_VM_CACHE_GNAT   cache directory (default: <repo>/.toolchains/gnat)

set -eu
set -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "${here}/../.." && pwd)

toolchain_ini="${repo}/frontend/python/rst_code_example_pipeline/src/rst_code_example_pipeline/data/toolchain.ini"

# Expand a cache location against the repository root, matching how the
# Vagrantfile expands it against its own directory. An absolute value is used
# as-is; a relative one must not depend on the caller's working directory.
abspath () {
  case "$1" in
    /*) echo "$1" ;;
    *)  echo "${repo}/$1" ;;
  esac
}

cache=$(abspath "${LEARN_VM_CACHE_GNAT:-.toolchains/gnat}")

# Downloading is large enough to keep in its own file.
fetch_impl="${here}/vm_cache_gnat_fetch.sh"

size_of () {
  if [ -d "$1" ]; then du -sh "$1" 2>/dev/null | cut -f1; else echo "-"; fi
}

versions_of () {
  sed -n '/^\[toolchains\]/,/^\[/p' "${toolchain_ini}" \
    | sed -n "s/^$1[[:space:]]*=[[:space:]]*//p"
}

# The basenames this cache is required to contain.
wanted_names () {
  local tool ver
  for tool in gnat gnatprove gprbuild; do
    for ver in $(versions_of "${tool}"); do
      echo "${tool}-x86_64-linux-${ver}.tar.gz"
      echo "${tool}-x86_64-linux-${ver}.tar.gz.sha256"
    done
  done
}

# Entries present in the cache that are not required. A .part file is also
# treated as such: a completed download is always renamed into place, so one
# that remains is the residue of an interrupted run.
orphans () {
  [ -d "${cache}" ] || return 0
  local wanted
  # LC_ALL=C throughout: comm compares byte-wise, so both inputs must be
  # sorted that way too. Locale collation ignores the '-', which puts
  # gnat-... and gnatprove-... in an order comm rejects.
  wanted=$(wanted_names | LC_ALL=C sort)
  find "${cache}" -maxdepth 1 -type f -printf '%f\n' 2>/dev/null \
    | LC_ALL=C sort \
    | LC_ALL=C comm -23 - <(echo "${wanted}") \
    | while read -r f; do echo "${cache}/${f}"; done
}

require_ini () {
  if [ ! -f "${toolchain_ini}" ]; then
    echo "error: ${toolchain_ini} not found" >&2
    exit 1
  fi
}

do_summary () {
  printf '%-8s %-8s %s\n' "gnat" "$(size_of "${cache}")" "${cache}"
}

do_report () {
  require_ini
  local tool orphan_list f

  echo "Toolchain versions wanted by toolchain.ini:"
  for tool in gnat gnatprove gprbuild; do
    printf '  %-10s %s\n' "${tool}" "$(versions_of "${tool}" | tr -s ' ')"
  done

  echo
  echo "Toolchain tarballs present:"
  if [ -d "${cache}" ]; then
    find "${cache}" -maxdepth 1 -type f -name '*.tar.gz' -printf '  %f\n' \
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
}

do_clean () {
  local delete=$1
  local list count f

  list=$(orphans)
  if [ -z "${list}" ]; then
    echo "No orphaned cache entries."
    return 0
  fi
  count=$(echo "${list}" | wc -l)

  if [ "${delete}" = true ]; then
    echo "${list}" | while read -r f; do
      echo "removing ${f}"
      rm -f "${f}"
    done
    echo "Removed ${count} entr$([ "${count}" -eq 1 ] && echo y || echo ies)."
  else
    echo "Would remove ${count} orphaned entr$([ "${count}" -eq 1 ] && echo y || echo ies):"
    echo "${list}" | while read -r f; do
      printf '  %-8s %s\n' "$(du -h "${f}" 2>/dev/null | cut -f1)" "${f}"
    done
    echo
    echo "This was a dry run. Re-run with --delete to remove them."
  fi
}

case "${1:-}" in
  fetch)    shift; exec bash "${fetch_impl}" "$@" ;;
  summary)  do_summary ;;
  report)   do_report ;;
  orphans)  orphans ;;
  clean)    do_clean "$([ "${2:-}" = "--delete" ] && echo true || echo false)" ;;
  -h|--help|"")
    sed -n '3,27p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *)
    echo "error: unknown command '$1'" >&2; exit 1 ;;
esac
