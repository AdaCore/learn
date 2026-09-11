#!/bin/bash
#
# Operate on the cache of apt package files.
#
# Reports the contents of the cache, identifies entries that are no longer
# required, and removes them.
#
# The cache holds the .deb files apt downloaded during provisioning, so that
# rebuilding a VM does not fetch them again. Both VMs use the same cache.
#
# An entry is no longer required when its package and version appear in
# neither vm_apt_web.txt nor vm_apt_epub.txt. Those are the same lists the
# provisioner installs from, so the two cannot diverge. A .deb file is named
# <package>_<version>_<architecture>.deb, with the epoch separator of a
# version encoded as %3a.
#
# Note that the lists only describe a VM provisioned with package pinning
# enabled. After a bootstrap with VM_APT_PIN=0, or after upgrading a VM in
# place, the cache legitimately holds versions no list mentions yet; capture
# the lists first, otherwise those entries are reported as no longer required.
#
# Invoked by vm_cache_report.sh and vm_cache_clean.sh. May also be invoked
# directly to operate on this cache alone.
#
# Usage:
#   vm_cache_apt.sh summary          # one line for the caches table
#   vm_cache_apt.sh report           # contents and entries to remove
#   vm_cache_apt.sh orphans          # their paths, one per line
#   vm_cache_apt.sh clean            # dry run -- what would be removed
#   vm_cache_apt.sh clean --delete   # remove it
#
# Environment:
#   LEARN_VM_CACHE_APT   cache directory (default: <repo>/.toolchains/apt)

set -eu
set -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "${here}/../.." && pwd)

pin_lists=("${here}/vm_apt_web.txt" "${here}/vm_apt_epub.txt")

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

require_lists () {
  local f
  for f in "${pin_lists[@]}"; do
    if [ ! -f "${f}" ]; then
      echo "error: ${f} not found" >&2
      exit 1
    fi
  done
}

# "<package> <version>" for every entry of both pin lists. The architecture
# qualifier a multi-arch entry carries (bind9-libs:amd64=...) is dropped: the
# same package and version may be cached as _amd64.deb or _all.deb.
wanted_pairs () {
  cat "${pin_lists[@]}" \
    | sed -e 's/:[a-z0-9][a-z0-9-]*=/=/' -e 's/=/ /' \
    | LC_ALL=C sort -u
}

# "<package> <version>" for every .deb in the cache. Only regular files
# directly in the directory are considered: apt also keeps `lock`, `partial/`
# and `apt/` there, which are not ours to touch.
present_pairs () {
  [ -d "${cache}" ] || return 0
  find "${cache}" -maxdepth 1 -type f -name '*.deb' -printf '%f\n' 2>/dev/null \
    | sed 's/\.deb$//' \
    | awk -F_ '{ gsub(/%3a/, ":", $2); print $1, $2 }' \
    | LC_ALL=C sort -u
}

# Paths of the .deb files whose package and version are in neither list.
orphans () {
  [ -d "${cache}" ] || return 0
  local stale
  # LC_ALL=C throughout: comm compares byte-wise, so both inputs must be
  # sorted the same way.
  stale=$(LC_ALL=C comm -13 <(wanted_pairs) <(present_pairs))
  [ -n "${stale}" ] || return 0
  # Map each stale pair back to the file carrying it. A version in a filename
  # has its epoch separator encoded, so encode before matching.
  echo "${stale}" | while read -r pkg ver; do
    find "${cache}" -maxdepth 1 -type f \
         -name "${pkg}_${ver//:/%3a}_*.deb" -print 2>/dev/null
  done
}

do_summary () {
  printf '%-8s %-8s %s\n' "apt" "$(size_of "${cache}")" "${cache}"
}

do_report () {
  require_lists
  local total stale_list stale_count f

  if [ ! -d "${cache}" ]; then
    echo "Package files present: (cache directory does not exist yet)"
    return 0
  fi

  total=$(find "${cache}" -maxdepth 1 -type f -name '*.deb' 2>/dev/null | wc -l)
  stale_list=$(orphans)
  stale_count=$([ -z "${stale_list}" ] && echo 0 || echo "${stale_list}" | wc -l)

  echo "Package files present: ${total}"
  echo "Required by the pinned lists: $((total - stale_count))"

  echo
  if [ "${stale_count}" -eq 0 ]; then
    echo "No entries to remove."
    return 0
  fi

  echo "Entries no longer required (in neither pinned list):"
  echo "${stale_list}" | while read -r f; do
    printf '  %-8s %s\n' "$(du -h "${f}" 2>/dev/null | cut -f1)" "${f##*/}"
  done

  if [ "${stale_count}" -gt $((total - stale_count)) ]; then
    echo
    echo "Most of the cache is reported as no longer required, which suggests"
    echo "the pinned lists do not describe the VMs the cache was filled from."
    echo "Capture the lists before removing anything."
  fi

  echo
  echo "Remove them with: frontend/vm/vm_cache_clean.sh --delete"
}

do_clean () {
  require_lists
  local delete=$1
  local list count f

  list=$(orphans)
  if [ -z "${list}" ]; then
    echo "No apt cache entries to remove."
    return 0
  fi
  count=$(echo "${list}" | wc -l)

  if [ "${delete}" = true ]; then
    echo "${list}" | while read -r f; do
      echo "removing ${f}"
      rm -f "${f}"
    done
    echo "Removed ${count} package file$([ "${count}" -eq 1 ] || echo s)."
  else
    echo "Would remove ${count} package file$([ "${count}" -eq 1 ] || echo s):"
    echo "${list}" | while read -r f; do
      printf '  %-8s %s\n' "$(du -h "${f}" 2>/dev/null | cut -f1)" "${f}"
    done
    echo
    echo "This was a dry run. Re-run with --delete to remove them."
  fi
}

case "${1:-}" in
  summary)  do_summary ;;
  report)   do_report ;;
  orphans)  orphans ;;
  clean)    do_clean "$([ "${2:-}" = "--delete" ] && echo true || echo false)" ;;
  -h|--help|"")
    sed -n '3,34p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *)
    echo "error: unknown command '$1'" >&2; exit 1 ;;
esac
