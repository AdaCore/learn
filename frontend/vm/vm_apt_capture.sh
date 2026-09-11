#!/bin/bash -eu
#
# Capture the apt package set of a provisioned VM into its pin list.
#
# The pin lists (vm_apt_web.txt, vm_apt_epub.txt) record the exact package
# versions of a VM whose output has been verified, so that a later
# `vagrant up` reproduces that machine rather than whatever the archive
# serves on the day. This script is how a new snapshot is taken.
#
# Run it on the host, from the working copy the VMs were created in.
# Only snapshot a VM that has been provisioned and whose build output has
# been checked: whatever is installed at that moment is frozen into the
# list and demanded of every future provision.
#
# Usage:
#   frontend/vm/vm_apt_capture.sh web
#   frontend/vm/vm_apt_capture.sh epub
#   frontend/vm/vm_apt_capture.sh            # both

set -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "${here}/../.." && pwd)

# The one thing this script exists to record. `${binary:Package}` keeps the
# `:arch` suffix that multi-arch packages carry (bind9-libs:amd64) and omits
# it elsewhere, which is the format `apt-get install` expects back.
dpkg_format='${binary:Package}=${Version}\n'

capture () {
  local vm=$1
  local out="${repo}/frontend/vm/vm_apt_${vm}.txt"
  local tmp="${out}.new"
  local rc=0

  echo "Capturing package set of the '${vm}' VM ..."

  # Go through `vagrant ssh` rather than a hardcoded port/key, so the script
  # keeps working if the forwarded ports move.
  #
  # `|| rc=$?` matters: the redirection below creates ${tmp} before the
  # command runs, so letting `set -e` abort here would leave an empty file
  # behind. Capture the status instead and clean up explicitly.
  ( cd "${repo}" \
      && vagrant ssh "${vm}" -c "dpkg-query -W -f='${dpkg_format}'" -- -T ) \
    > "${tmp}" || rc=$?

  if [ "${rc}" -ne 0 ] || [ ! -s "${tmp}" ]; then
    rm -f "${tmp}"
    echo "error: could not capture the package set of '${vm}'" \
         "(exit ${rc}); '${out}' left unchanged" >&2
    return 1
  fi

  mv "${tmp}" "${out}"
  echo "  -> frontend/vm/vm_apt_${vm}.txt ($(wc -l < "${out}") packages)"
}

vms=("$@")
if [ ${#vms[@]} -eq 0 ]; then
  vms=(web epub)
fi

for vm in "${vms[@]}"; do
  capture "${vm}"
done

cat <<'EOF'

Review the diff before committing it. The snapshot freezes whatever is
installed right now, including anything installed by hand while debugging.
EOF
