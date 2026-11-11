# Bootstrap a Linux account

`bootstrap.yml` installs upstream Nix, builds an existing Home Manager profile,
checks out the Emacs configuration, imports a dedicated GPG signing subkey,
activates the profile and sets the account's login shell in one run. Run Ansible
on your controller (for example, your Mac); the target needs SSH access, Python 3
and sudo. The supported targets are x86-64 Debian/Ubuntu hosts running systemd.

Home Manager owns dotfiles and personal packages. This playbook owns the initial
Nix installation, Emacs configuration checkout, signing-key import and login-shell
setting. It does not run the older `homedir.yml` or install accelerator software.

Before activation, `bootstrap_emacs_repo` is cloned into `~/.emacs.d` as the login
user. This is a writable Git checkout. Existing checkouts are left at their
current revision, including any local edits; update them separately with Git.
A nonempty directory that is not a Git checkout causes the clone to fail without
overwriting it.

Ubuntu's `zsh` package is installed with the prerequisites. After successful
activation, the existing account's login shell is set to `/bin/zsh`; the next
login uses it. Home Manager supplies `.zshrc`, `.zshenv` and the rest of the shell
configuration. The login shell is always zsh, using a stable system path
independent of Nix generations.

## Private configuration

For a new checkout, run these commands from the repository's root. Skip the copy
if you already have a private inventory:

```sh
mkdir -p playbook/private
chmod 700 playbook/private
cp playbook/bootstrap.inventory.example.yml playbook/private/inventory.yml
chmod 600 playbook/private/inventory.yml
```

Edit the private inventory. The example defines two hosts, `server_a` and
`server_b`, with shared settings under `bootstrap.vars`. Both inherit those
settings; a value under an individual host overrides the shared value. For
example, one host can select a different `bootstrap_revision` for a staged update.

| Variable | Meaning | Usual scope |
| --- | --- | --- |
| `ansible_host` | SSH alias, DNS name or IP address to connect to | Per host |
| `ansible_user` | Optional remote login override; otherwise SSH selects the user | Usually omitted; shared or per host if needed |
| `ansible_python_interpreter` | Python executable on the remote host | Shared (`/usr/bin/python3`) |
| `bootstrap_repo` | Dotfiles Git repository URL | Shared |
| `bootstrap_emacs_repo` | Emacs configuration Git URL, cloned into `~/.emacs.d` before activation | Shared |
| `bootstrap_revision` | Optional branch, tag or commit to check out; defaults to `HEAD` | Shared, with overrides for staged updates |
| `bootstrap_profile` | Exact key under the flake's `homeConfigurations` | Per host |
| `bootstrap_primary_fingerprint` | Full fingerprint of the primary GPG key | Shared |
| `bootstrap_subkey_fingerprint` | Full fingerprint of this host's dedicated signing subkey | Per host |
| `bootstrap_vault_file` | Encrypted bundle's path on the controller | Shared template, separate file per host |

Leave `ansible_user` unset to use OpenSSH's normal login selection: `User` from
the matching SSH configuration, or your local username if none is configured.
An explicit Ansible user setting (such as `-u`, `remote_user`, or `ansible_user`)
overrides that choice. Use the SSH alias in `ansible_host` so the intended `Host`
section matches. The playbook uses gathered facts to identify the actual remote
account and home directory; it does not require `ansible_user` to be defined or
create a new account.

A full commit hash is optional. You can use a branch or tag, or omit
`bootstrap_revision` for `HEAD` (the repository's default branch on a fresh
clone). Each run reports the resolved commit alongside the profile size. Set a
commit hash when you want repeat runs to use exactly the same dotfiles revision.
The flake lock file still pins Nix dependencies.

The inventory labels are yours to choose; they do not need to match the machines'
OS hostnames, SSH aliases or Home Manager profile names. `inventory_hostname`
means that label, so the shared Vault path template resolves to
`playbook/private/server_a.vault.yml` and `playbook/private/server_b.vault.yml`.
Renaming a label therefore changes the expected Vault filename. Fingerprints in
the inventory use uppercase hexadecimal without spaces or a trailing `!`.
Use a separate signing subkey and Vault bundle for each host so a single host's
subkey can be revoked independently. The primary fingerprint can remain shared.

Both repository URLs must be readable before the signing key is installed; HTTPS
works for public repositories. SSH authentication and GPG commit signing are
separate concerns.

For two or three servers, one inventory file is enough. If it grows, shared
variables can move to `playbook/private/group_vars/bootstrap.yml` and individual
variables to `playbook/private/host_vars/server_a.yml`, alongside the inventory.
Ansible loads these automatically. These are ordinary variable mappings, without
the inventory's `bootstrap:`, `vars:` or `hosts:` wrappers.

Settings declared in `bootstrap.yml` itself, such as the pinned Nix version and
checksum, take precedence over inventory variables. Override those with explicit
`-e` arguments.

`playbook/private/` is ignored, including its inventory and encrypted Vault files.
Only examples and automation belong in Git. Keep an independent backup of the
private directory and the Vault password. Never put actual credentials in the
example files. The former tracked `playbook/inventory.yml` has been removed;
its entries were preserved locally as comments in the private inventory. They
are inactive until explicitly configured. Earlier Git history still contains
the old inventory.

The profile must already exist in the selected revision, match the remote username
and home directory, and have `nix.gc.automatic = false`. Its
`programs.git.settings.user.signingKey` should be the dedicated signing subkey's
full fingerprint followed by `!`, which forces Git to select that subkey.
The primary fingerprint is also accepted for existing configurations, but then
GPG selects an available signing subkey. Keep `commit.gpgSign = true` and use
OpenPGP signing. The profile should not manage `~/.config/nix/nix.conf`, which
contains this bootstrap's per-user flake and build-concurrency settings.

## Prepare a signing subkey and Vault bundle

Do this on the machine that holds your primary key. No key generation or export
is performed automatically by the playbook.

Create a dedicated signing subkey once per host. Skip this command if you have
already created that host's subkey:

```sh
gpg --quick-add-key PRIMARY_FINGERPRINT ed25519 sign 1y
```

List the primary key and all its subkeys, explicitly requesting both kinds of
fingerprint. Omit `PRIMARY_FINGERPRINT` to list all your secret-key identities if
you have not yet identified the primary key:

```sh
gpg --list-secret-keys --keyid-format long \
  --with-fingerprint --with-subkey-fingerprint PRIMARY_FINGERPRINT
```

`--with-subkey-fingerprint` alone only adds subkey fingerprints when the primary
fingerprint is being printed. The explicit `--with-fingerprint` makes this work
regardless of the usual key-ID display format. The listing contains metadata,
not exported secret-key material. It has this shape (placeholders shown):

```text
sec   rsa4096/PRIMARY_KEY_ID ... [SC]
      PRIMARY_FINGERPRINT
uid   ...
ssb   cv25519/ENCRYPTION_KEY_ID ... [E]
      ENCRYPTION_SUBKEY_FINGERPRINT
ssb   ed25519/SIGNING_KEY_ID ... [S]
      SIGNING_SUBKEY_FINGERPRINT
```

Copy the full fingerprint on the line following the newly created `ssb` entry
marked `[S]`. `[E]` is an encryption subkey, and `sec` is the primary key. If
there are several signing subkeys, select the one you created for this host;
check its creation date and expiry. The shorter ID after `ed25519/` is not the
full fingerprint, and neither is any separately displayed `Keygrip`.

Set `bootstrap_primary_fingerprint` to the fingerprint under `sec`, and
`bootstrap_subkey_fingerprint` to the fingerprint under that `ssb [S]` entry.
Use uppercase hexadecimal without spaces or a trailing `!` in the inventory.
For the Ed25519 subkey created above, the fingerprint is 40 hexadecimal
characters. The fingerprint is a public identifier; exporting the secret
subkey is a separate step:

```sh
gpg --armor --export-secret-subkeys 'SIGNING_SUBKEY_FINGERPRINT!'
```

Replace the uppercase placeholders with the actual fingerprints. Keep the `!`
inside the quotes on the export command: it selects that exact subkey. This
command prints the secret-key block to the terminal; it does not create the
Vault file. Copy everything from `-----BEGIN PGP PRIVATE KEY BLOCK-----` through
`-----END PGP PRIVATE KEY BLOCK-----`, including those lines and any blank lines
inside the block. Avoid recording this sensitive output in a terminal log.
Preserve the blank line separating the armor header from the encoded key data:
without it, GPG can import the key but still return an `invalid armor header`
error, causing Ansible to stop.

Now create the encrypted bundle, using the filename matching your inventory
label (`server_a` in this example):

```sh
ansible-vault create --vault-id signing@prompt playbook/private/server_a.vault.yml
```

`--vault-id` takes `label@password-source`. Here, `signing` is an arbitrary label
for the Vault encryption password, recorded in the encrypted file's header;
`prompt` asks for that password interactively. The label does not identify the
host or GPG key, and can be shared across the hosts' Vault files. By default,
Ansible uses labels as hints when selecting which supplied password to try.

Choose and confirm a Vault password when prompted. This encrypts the bundle;
it is separate from your existing GPG key passphrase. Ansible opens your editor.
Enter this YAML, replacing the placeholder block with the complete export and
indenting every line of that block by two spaces:

```yaml
secret_subkey: |
  -----BEGIN PGP PRIVATE KEY BLOCK-----

  PASTE_THE_EXPORTED_CONTENT_HERE
  -----END PGP PRIVATE KEY BLOCK-----
# Optional when sudo requires a password:
ansible_become_password: 'YOUR_REMOTE_SUDO_PASSWORD'
```

The GPG passphrase is not needed for import and does not belong in this bundle.
An existing `passphrase` field is ignored and can be removed. If the sudo password
contains a single quote, double that quote inside the YAML single-quoted string.
Save and finish the editor session; Ansible encrypts the file. To reopen an
existing bundle, use `ansible-vault edit` with the same `--vault-id` and filename.

`ansible_become_password` is the remote login account's sudo password. It is
separate from the GPG passphrase and from the Vault password that encrypts this
file. The playbook decrypts the host's bundle on the controller before its first
sudo task. It supplies the sudo password to Ansible with sensitive output
suppressed. Only `secret_subkey` is sent to GPG's standard input.
Omit `ansible_become_password` for passwordless sudo, or supply it interactively
with `--ask-become-pass` instead.

Do not use `--export-secret-keys` or export the entire keyring. Use a private
editor session without backups or swap files. The decrypted bundle's template
is also in `bootstrap.vault.example.yml`.

Ansible imports the export directly on the host with `gpg --batch --no-tty
--homedir ~/.gnupg --import`, passing the key through standard input. It creates
`~/.gnupg` with mode `0700` and suppresses secret-bearing task output. SSH
pipelining avoids staging secret-bearing Ansible modules; keep it enabled and do
not enable Ansible debug logging when using real credentials. No temporary key
export is written to the host or the Nix store. The bootstrap does not inspect
the export's key selection or create test signatures, so use the exact subkey
export command above.

Import preserves the key's existing passphrase protection. When you first sign
on the host, unlock the key through GPG's pinentry prompt (Emacs in this profile).
Signing works with the controller disconnected. Your GPG agent's normal cache
policy determines when another prompt is needed.

## Validate without connecting

Run commands from the repository root so Ansible picks up `ansible.cfg`. It
formats results as readable YAML, preserving multiline build output and omitting
duplicate `stdout_lines`/`stderr_lines` arrays. Task headings remain visible.
Secret-bearing tasks still use `no_log`; this does not expose Vault contents.
The command module still buffers Nix output until the build finishes.

```sh
ansible-playbook -i playbook/bootstrap.inventory.example.yml \
  playbook/bootstrap.yml --syntax-check
ansible-playbook -i playbook/private/inventory.yml \
  playbook/bootstrap.yml --list-hosts
```

These commands do not connect to hosts. Ordinary `--check` is deliberately
unsupported: it cannot predict Nix build outputs or perform a secret-key import.

## Bootstrap the host

The command prompts for the Vault password; the sudo password is read from
`ansible_become_password` in that host's encrypted bundle when present.
It installs prerequisites/Nix, checks out the selected dotfiles revision, builds
and reports the profile's size, checks out Emacs configuration, imports the
signing subkey, activates Home Manager and sets zsh as the login shell. The
`result` symlink retains the built generation. The reported runtime closure
excludes source caches, old generations and temporary build space.

```sh
ansible-playbook -i playbook/private/inventory.yml playbook/bootstrap.yml \
  --limit server_a --vault-id signing@prompt
```

If sudo needs a password and it is not stored in the Vault, add
`--ask-become-pass` (or `-K`) to the command. Ansible prompts for the remote
account's sudo password separately from the Vault password. No sudo password
is needed for passwordless sudo. The playbook already selects sudo to root for
the tasks that need it, using Ansible's defaults; do not set global
`ansible_become: true` or use global `--become`.
The playbook elevates package installation, Nix daemon and login-shell tasks. Activation
refuses conflicting existing dotfiles rather than overwriting them; resolve the
conflict and rerun. Dirty dotfiles checkouts are not forcibly reset. Use
`--limit server_b` for the other example host, or omit `--limit` to process every
host in the `bootstrap` group, one at a time.

Nix 2.35.6 and the x86-64 archive's SHA-256 are pinned in `bootstrap.yml`; existing
upstream multi-user installations are reused without automatic upgrades. Partial
or Determinate installations require separate handling. Fresh installs use
`--daemon --no-channel-add`, so no extra Nixpkgs channel is fetched. There is no
minimum-free-space check. Run builds and activation between timed calculations,
and prune old generations during maintenance.

References: [upstream Nix installation](https://nix.dev/install-nix),
[Ansible inventory variables](https://docs.ansible.com/projects/ansible/latest/inventory_guide/intro_inventory.html#adding-variables-to-inventory),
[Ansible variable precedence](https://docs.ansible.com/projects/ansible/latest/playbook_guide/playbooks_variables.html#understanding-variable-precedence),
[Ansible account management](https://docs.ansible.com/projects/ansible/latest/collections/ansible/builtin/user_module.html),
[Ansible SSH user selection](https://docs.ansible.com/projects/ansible/latest/collections/ansible/builtin/ssh_connection.html#parameter-remote_user),
[Ansible Git revisions](https://docs.ansible.com/projects/ansible/latest/collections/ansible/builtin/git_module.html#parameter-version),
[Ansible sudo passwords](https://docs.ansible.com/projects/ansible/latest/playbook_guide/playbooks_privilege_escalation.html#become-connection-variables),
[Ansible Vault](https://docs.ansible.com/projects/ansible/latest/vault_guide/vault.html),
[GnuPG fingerprint display options](https://gnupg.org/documentation/manuals/gnupg/GPG-Input-and-Output.html),
[GnuPG export commands](https://gnupg.org/documentation/manuals/gnupg/Operational-GPG-Commands.html).
