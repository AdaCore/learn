#!/bin/bash
#
# Fetch a GNAT-FSF-builds toolchain tarball into the download cache and print
# its path.
#
# The cache exists so that destroying a VM does not throw the toolchains away:
# a full reprovision otherwise re-downloads several GB. Upstream publishes a
# .sha256 sidecar for every asset, and it is verified on every use -- not only
# after downloading -- so a file corrupted later is re-fetched rather than
# failing extraction with a confusing tar error.
#
# Runs on the host as well as inside a VM. On the host it can be used to warm
# the cache before `vagrant up`, so that provisioning downloads nothing.
#
# Usage:
#   vm_toolchain_fetch.sh <tool> <version>   # print the verified cached path
#   vm_toolchain_fetch.sh --all              # every version in toolchain.ini
#
# Environment:
#   LEARN_VM_CACHE_GNAT   cache directory
#                         (default: <repo>/.toolchains/gnat; the Vagrantfile
#                          passes /vagrant_cache/gnat inside the VMs)
#   LEARN_VM_NAME         suffix for temporary files, so that two VMs sharing
#                         one cache cannot collide (default: the hostname)

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
tag="${LEARN_VM_NAME:-$(hostname)}"

base_url=https://github.com/alire-project/GNAT-FSF-builds/releases/download

usage () {
  sed -n '3,25p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
  exit "${1:-1}"
}

# Fetch one tarball, verified against its upstream .sha256 sidecar.
# Prints the cached path on stdout; progress goes to stderr so the path can be
# captured with $(...).
fetch () {
  local tool=$1
  local ver=$2
  local name="${tool}-x86_64-linux-${ver}.tar.gz"
  local url="${base_url}/${tool}-${ver}/${name}"
  local tarball="${cache}/${name}"
  local sum="${cache}/${name}.sha256"
  # Two VMs share one cache and fetch the same gnat tarballs, so their
  # temporaries must not collide.
  local part="${tarball}.${tag}.part"
  local sum_part="${sum}.${tag}.part"
  local rc=0

  mkdir -p "${cache}"

  if [ ! -s "${sum}" ]; then
    echo "Fetching checksum for ${name}" >&2
    wget -q -O "${sum_part}" "${url}.sha256" || rc=$?
    if [ "${rc}" -ne 0 ] || [ ! -s "${sum_part}" ]; then
      rm -f "${sum_part}"
      echo "error: could not fetch ${url}.sha256 (exit ${rc})" >&2
      return 1
    fi
    mv "${sum_part}" "${sum}"
  fi

  if [ -f "${tarball}" ] && verify "${sum}" "${tarball}"; then
    echo "Using cached ${name}" >&2
  else
    [ -f "${tarball}" ] && echo "Cached ${name} failed verification; re-fetching" >&2
    rm -f "${tarball}"
    echo "Downloading ${name}" >&2
    # The redirection creates ${part} before wget runs, so a failure must be
    # caught rather than left to `set -e` -- otherwise an empty file survives.
    wget -O "${part}" "${url}" || rc=$?
    if [ "${rc}" -ne 0 ]; then
      rm -f "${part}"
      echo "error: could not download ${url} (exit ${rc})" >&2
      return 1
    fi
    if ! verify "${sum}" "${part}"; then
      rm -f "${part}"
      echo "error: ${name} failed checksum verification after download" >&2
      return 1
    fi
    mv "${part}" "${tarball}"
  fi

  echo "${tarball}"
}

verify () {
  local sum=$1
  local file=$2
  # The sidecar holds a bare hash with no filename, so pair it up here.
  echo "$(cat "${sum}")  ${file}" | sha256sum -c - > /dev/null 2>&1
}

# Read a whitespace-separated list of versions out of toolchain.ini.
versions_of () {
  local tool=$1
  if [ ! -f "${toolchain_ini}" ]; then
    echo "error: ${toolchain_ini} not found" >&2
    return 1
  fi
  sed -n '/^\[toolchains\]/,/^\[/p' "${toolchain_ini}" \
    | sed -n "s/^${tool}[[:space:]]*=[[:space:]]*//p"
}

fetch_all () {
  local tool ver
  for tool in gnat gnatprove gprbuild; do
    for ver in $(versions_of "${tool}"); do
      fetch "${tool}" "${ver}" > /dev/null
    done
  done
  echo "Cache ready: ${cache}" >&2
  du -sh "${cache}" 2>/dev/null >&2 || true
}

case "${1:-}" in
  --all)          fetch_all ;;
  -h|--help|"")   usage 0 ;;
  -*)             echo "error: unknown option '$1'" >&2; usage 1 ;;
  *)
    [ $# -eq 2 ] || { echo "error: expected <tool> <version>" >&2; usage 1; }
    fetch "$1" "$2"
    ;;
esac
