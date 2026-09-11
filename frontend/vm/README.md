# Vagrant VM: How to

This file provides more details about setting up the Vagrant VMs for this
repository. It covers:

- what `vagrant up` actually installs, and how to connect to each VM
- how to run the VMs of several checkouts side by side
- how to avoid re-downloading several GB every time a VM is rebuilt
- what the pinned package lists are, and how to update them
- how to bring up a VM on a new Ubuntu base box

Everything here is optional, though. A simple `vagrant up` should be
sufficient.

## Bringing up the VMs

To create both VMs and install everything they need, run:

```
$ vagrant up
```

This creates two VMs and provisions each of them. Provisioning installs the
apt packages, the Node.js release from NodeSource, the Ada toolchains listed
in `toolchain.ini` (into `/opt/ada`), a Python virtual environment at
`/vagrant/venv`, and the frontend's npm dependencies.

The two VMs get different toolchains: `web` installs every GNAT version,
since it only needs `gnatchop`, while `epub` also installs GNATprove and
GPRbuild because it builds and runs the course examples.

Expect the first run to take a while: the toolchains alone are over 2 GB.
The section on caching below removes that cost from every later run.

To bring up just one of them:

```
$ vagrant up web
$ vagrant up epub
```

## Connecting

Once the VMs are up, open a shell on either of them with:

```
$ vagrant ssh web
$ vagrant ssh epub
```

Inside either VM, the checkout is mounted at `/vagrant`: `frontend/` and
`content/` are the live directories from your host, so edits on either side
are visible immediately.

The Ada toolchain is on the `PATH` of a login shell. A command run
non-interactively does not read `~/.profile`, so use a login shell when you
need the toolchain:

```
$ vagrant ssh epub -c "bash -lc 'gnatchop --version'"
```

## Running several checkouts at the same time

If you have more than one checkout of this repository, each can have its own
pair of VMs. VirtualBox names each VM after the directory it was created in,
so they do not clash — but the host ports do, and `vagrant up` fails with
"Vagrant cannot forward the specified ports on this VM".

Give the second checkout its own ports:

```
$ export LEARN_WEB_PORT=8081
$ export LEARN_WEB_SSH_PORT=2232
$ export LEARN_EPUB_SSH_PORT=2230
$ vagrant up
```

| Variable | Default | Effect |
|---|---|---|
| `LEARN_WEB_PORT` | `8080` | Host port for the `web` development server |
| `LEARN_WEB_SSH_PORT` | `2222` | Host port for SSH to `web` |
| `LEARN_EPUB_SSH_PORT` | `2200` | Host port for SSH to `epub` |

Set them in the shell you use for that checkout, so that every later
`vagrant` command in it agrees.

These are a preference, not a guarantee: if a port you ask for is also taken,
Vagrant moves it and warns rather than failing. To see what a machine ended
up with:

```
$ vagrant port web
$ vagrant ssh-config web
```

`vagrant ssh` always connects correctly, so prefer it over a hard-coded port.

## Avoiding repeated downloads

Destroying a VM throws away everything provisioning downloaded. To avoid
paying for that twice, the Ada toolchain tarballs and the `.deb` files are
kept on the host instead, in directories mounted into both VMs.

Nothing needs to be configured for this — the caches fill themselves on the
first `vagrant up` and are reused from then on. The defaults live in
`.toolchains/` inside the checkout and are gitignored:

| Variable | Default | Holds |
|---|---|---|
| `LEARN_VM_CACHE_GNAT` | `.toolchains/gnat` | Ada toolchain tarballs and their checksums |
| `LEARN_VM_CACHE_APT` | `.toolchains/apt` | Downloaded `.deb` files |

Because the default is inside the checkout, a second checkout starts with an
empty cache. To share one set of downloads between them, point both variables
somewhere outside. For example:

```
$ export LEARN_VM_CACHE_GNAT=~/vm-cache/gnat
$ export LEARN_VM_CACHE_APT=~/vm-cache/apt
```

Use absolute paths. A relative one is resolved against the repository root,
not your current directory.

### Filling the cache in advance

You can download the toolchains before creating any VM, so that provisioning
fetches nothing. This runs on the host:

```
$ frontend/vm/vm_toolchain_fetch.sh --all
```

This is useful when you expect to rebuild a VM several times, or want the
download out of the way before going offline. To fetch a single version:

```
$ frontend/vm/vm_toolchain_fetch.sh gnat 15.1.0-2
```

Each tarball is checked against the SHA-256 published alongside it upstream,
every time it is used rather than only when downloaded, so a truncated or
damaged file is replaced automatically instead of breaking a later build.

### Checking and cleaning up

To see what the caches currently hold, run:

```
$ frontend/vm/vm_cache_report.sh
```

This shows where each cache is, how large it is, and which toolchain
tarballs are no longer listed in `toolchain.ini` — those accumulate whenever
a version is dropped, and nothing removes them on its own.

To get rid of those leftovers, run:

```
$ frontend/vm/vm_cache_clean.sh             # shows what it would remove
$ frontend/vm/vm_cache_clean.sh --delete    # removes it
```

The first form changes nothing, so it is safe to run to see the list.

Both commands are entry points covering every cache. The work is done by one
script per cache — `vm_cache_gnat.sh` and `vm_cache_apt.sh` — which take the
same verbs and can be run directly when you only care about one of them:

```
$ frontend/vm/vm_cache_gnat.sh report
$ frontend/vm/vm_cache_gnat.sh clean --delete
```

## The pinned package lists

Both VMs run Ubuntu, so their system software is installed with `apt`, the
package manager Debian and Ubuntu share. Left to itself, `apt` installs
whichever version of a package the Ubuntu archive happens to offer on the day
you ask for it, which means two people running `vagrant up` a month apart get
two different machines.

To avoid that, this directory keeps a record of the apt packages each VM
should have: `vm_apt_web.txt` and `vm_apt_epub.txt`, one file per VM, listing
one `package=version` per line. Provisioning installs exactly those versions,
so a `vagrant up` reproduces a known machine rather than today's archive.

That matters because these VMs produce published artifacts. A new texlive or
font package can change the PDFs without anything failing, and the difference
would only show up when someone compared a course against an earlier release.

Provisioning passes `--allow-downgrades` deliberately: if a package on the VM
is newer than the pinned version, it is moved *back*. The pinned versions are
expected to lag the archive, and that is the point.

### Updating the packages in a VM

Upgrading is a deliberate step, not something that happens on its own. Bring
the VMs up, upgrade in place, and check the result before recording it:

```
$ vagrant ssh epub
$ sudo apt update && sudo apt full-upgrade
```

Then build the content and confirm the output is still correct — at minimum a
successful `make site`, and a generated course PDF compared against the
published one. Only then record the new state, from the host:

```
$ frontend/vm/vm_apt_capture.sh epub
$ frontend/vm/vm_apt_capture.sh          # or both VMs at once
```

This rewrites the lists in place. Read the diff before committing it: the
snapshot freezes whatever is installed at that moment — including anything
you installed by hand while debugging — and every later `vagrant up` will
then demand it.

## Bringing up a new Ubuntu base box

Moving the VMs to a newer Ubuntu release has one complication: the pinned
lists name versions that the new release's archive has never carried, so
provisioning aborts before it can get far enough to record new ones. The
first run therefore has to be made without pinning:

| Variable | Default | Effect |
|---|---|---|
| `VM_APT_PIN` | `1` | Set to `0` to skip installing the pinned versions |

The full sequence is:

1. Set the new box and version in the `Vagrantfile`. Both the `web` and the
   `epub` block need it:

   ```ruby
   web.vm.box = "bento/ubuntu-<release>"
   web.vm.box_version = "<version>"
   ```

2. Rebuild the VMs with pinning switched off:

   ```
   $ vagrant destroy -f
   $ VM_APT_PIN=0 vagrant up
   ```

   Expect to iterate here. A new Ubuntu release is where renamed or dropped
   packages turn up, and the failure will be in the package list in the
   `Vagrantfile` rather than in the pinned lists, which are switched off.

3. Verify the result, as when updating packages: a successful build on both
   VMs, and a generated course PDF compared against the published one.

4. Record the new package set:

   ```
   $ frontend/vm/vm_apt_capture.sh
   ```

5. Rebuild once more, this time *with* pinning, to confirm the new lists
   install cleanly from scratch:

   ```
   $ vagrant destroy -f
   $ vagrant up
   ```

   This step is not optional: a list that cannot be replayed is worse than no
   list, because it will only fail for the next person.

6. Commit the captured lists together with the `box_version` change. On their
   own, neither half describes a working configuration.
