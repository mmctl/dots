# openSUSE Tumbleweed setup

This guide documents the setup flow for a personal openSUSE Tumbleweed machine.
It starts from a minimal desktop-oriented installation, fetches the dotfiles
repository, and then applies the setup scripts in the intended order. The
scripts do most of the system configuration, but a few manual steps remain for
network profiles, browser extensions, SSH keys, and optional fingerprint
authentication.

The setup is intended to be run as the target regular user, not as `root`. The
scripts will request `sudo` only for the system-level changes that require it.

## Starting point

During installation, use the following baseline:

- Installer image: DVD / offline image.
- Online repositories: Main Repository, Main Update Repository, Non-OSS Repository, and Non-OSS Update Repository.
- System role: Generic Desktop.
- Boot loader: GRUB2 EFI.
- Software patterns:
  - Graphical Environments: Fonts.
  - Documentation: Help and Support Documentation, Documentation.
  - Base Technologies: Kernel dump tooling, Base System, Enhanced Base System, SELinux Support, x86-64-v3 optimized packages, YaST Base Utilities, YaST Desktop Utilities, and Minimal Appliance Base.

After the first boot, continue with the steps below.

## 1. Connect to the network

Use NetworkManager from the terminal to connect to a temporary Wi-Fi network.

List available WiFi networks:

```sh
nmcli device wifi list
```

Connect to a network interactively:

```sh
nmcli device wifi connect "WiFiNetworkID" --ask
```

The later manual networking section explains how to store guest and private
Wi-Fi profiles with the desired autoconnect priorities and route metrics.

## 2. Fetch the dotfiles and setup scripts

Create the expected XDG data and state directories, install the bootstrap tools
needed to clone the repository and verify repository keys, and clone the
dotfiles repository.

```sh
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
mkdir -p -- "$XDG_DATA_HOME" "$XDG_STATE_HOME"

sudo zypper install git stow curl gpg2

export DOTS_DIR="$XDG_DATA_HOME/dots"
git clone --branch personal-opensuse-tumbleweed --single-branch https://github.com/mmctl/dots.git "$DOTS_DIR"
```

The setup scripts are expected under:

```text
$DOTS_DIR/setup/scripts/
```

The dotfiles repository itself is stored at:

```text
$XDG_DATA_HOME/dots
```

With the default values above, that resolves to:

```text
~/.local/share/dots
```

## 3. Understand the setup runner and logs

Run each setup script through the wrapper:

```sh
"$DOTS_DIR/setup/scripts/run-setup-script" "$DOTS_DIR/setup/scripts/setup-name"
```

The wrapper refuses to run as `root`, checks that the target script is
executable, creates a private log directory, and tees all output to both the
terminal and a timestamped log file.

Logs are written to:

```text
${XDG_STATE_HOME:-$HOME/.local/state}/setup/
```

With the exported `XDG_STATE_HOME`, this becomes:

```text
~/.local/state/setup/
```

Each log file is named after the script and the time of execution:

```text
setup-base-YYYYMMDD-HHMMSS.log
setup-devel-YYYYMMDD-HHMMSS.log
```

To reduce repetition during the setup session, define a small helper:

```sh
run_setup() {
  "$DOTS_DIR/setup/scripts/run-setup-script" "$DOTS_DIR/setup/scripts/setup-$1"
}
```

The examples below use this helper.

After each reboot, open a new terminal and re-create the environment variables
and helper if they are no longer defined:

```sh
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
export DOTS_DIR="${DOTS_DIR:-$XDG_DATA_HOME/dots}"

run_setup() {
  "$DOTS_DIR/setup/scripts/run-setup-script" "$DOTS_DIR/setup/scripts/setup-$1"
}
```


## 4. Run the automated setup steps

Run the scripts in the order shown here. Several scripts print `ACTION` lines at
the end; treat those as manual checkpoints before relying on the affected
subsystem.

### 4.1 Configure Snapper for `/home`

```sh
run_setup snapper
```

This script checks whether `/home` is a Btrfs subvolume. If it is, the script
creates or reuses the Snapper configuration named `home`, allows the current
user to access it, enables number and timeline cleanup, and sets snapshot
retention limits. If `/home` is not a Btrfs subvolume, it logs that Snapper
setup for `/home` is skipped.

Main effect:

```text
Snapper config: home
Target path:    /home
```

### 4.2 Install the base profile and system structure

```sh
run_setup base
```

This script establishes the base user profile and repository state. It stows the
base profile from the dotfiles repository, creates the standard XDG directories,
stows the XDG user-dirs configuration, adds Packman Essentials with GPG key
verification, refreshes the repositories, installs multimedia codecs from
Packman, and performs a full Tumbleweed distribution upgrade.

Reboot before continuing so that the upgraded base system and profile changes
are active.

### 4.3 Install development and build essentials

```sh
run_setup devel-base
```

This script installs the basic development toolchain required by later scripts.
It also installs or updates the stable Rust toolchain through `rustup`.

Manual checkpoint:

```text
Ensure ~/.cargo/bin is on PATH.
```

The base profile normally handles this after reloading the shell or rebooting.
Confirm with:

```sh
command -v cargo
command -v rustup
```

### 4.4 Configure security, SSH, GPG, and secrets

```sh
run_setup security
```

This script prepares SSH, GPG, the password-manager command-line client, and the
encrypted Authinfo file used by mail and forge tooling. For SSH, the script generates
two key pairs, and will prompt for passwords on the secret keys.

Manual checkpoint: add these public keys to the relevant accounts, such as
forges and web servers:

```text
~/.ssh/${USER}_ed25519_personal.pub
~/.ssh/${USER}_ed25519_work.pub
```

### 4.5 Install the desktop environment

```sh
run_setup de
```

This script installs the Wayland desktop stack. It adds the DankLinux and DMS
repositories, installs Niri, DMS (Quickshell), greetd, Kitty graphical
file-management tools, fonts, desktop integration tools, and related utilities.
It also enables the DMS user service and the greetd system service.

Manual checkpoint: reboot after this step so that the greeter, user services,
shell configuration, and desktop stack are started cleanly.

### 4.6 Install productivity tools

```sh
run_setup productivity
```

This script installs and prepares the main productivity applications. It sets up
Emacs and its user service, installs mail tooling such as isync and mu, and
Myspell dictionaries, prepares the Maildir layout, configures desktop entries
and MIME handlers for Emacsclient, installs Flatpak and Flathub, and installs
Zen Browser, Signal, and Zotero.

Manual checkpoint: open Zen Browser and install the following extensions:

- Bitwarden.
- uBlock Origin, including the desired filter lists.
- Zotero Connector.

Reboot after this step.

### 4.7 Install the full development environment

```sh
run_setup devel
```

This script installs the larger development environment for writing,
programming, formal methods, proof engineering, and container work. In
particular, it installs TeX Live, Rust components, Go, C/C++, Python tooling,
opam and OCaml tooling, EasyCrypt, cvc5, Z3, Lean, Docker, and
QMK-related tools.

Manual checkpoint: reboot after this step so that group membership changes,
especially Docker group membership, take effect.

### 4.8 Create the PARA workspace and project checkouts

```sh
run_setup para
```

This script creates the personal PARA directory structure and checks out project repositories.

It creates private directories with mode `700`:

```text
~/projects
~/areas
~/resources
~/archives
```

The paths can be overridden with:

```sh
PROJECTS_DIR=...
AREAS_DIR=...
RESOURCES_DIR=...
ARCHIVES_DIR=...
```

The script then clones the configured project/area repositories and creates
selected symlinks.

This step assumes that the SSH keys created by `setup-security` have already
been added to the relevant forge accounts (e.g., GitHub) and that the SSH host
aliases from the stowed SSH configuration are available.

## 5. Manual network configuration

After the main setup, configure persistent network profiles.

### Guest WiFi

Use this for a shared or lower-priority WiFi network where the password may be stored system-wide and the connection is available to everyone.

```sh
nmcli device wifi connect "GuestWiFiID" --ask
nmcli connection modify "GuestWiFiID" \
  connection.autoconnect yes \
  connection.autoconnect-priority 50 \
  ipv4.route-metric 500 ipv6.route-metric 500
```

This makes the connection automatic, but lower priority than private WiFi and Ethernet.

### Private WiFi

Use this for the main private WiFi network. The password is stored in the user
keyring and the connection is restricted to the current user.

```sh
nmcli device wifi connect "PrivateWiFiID" --ask
nmcli connection modify "PrivateWiFiID" \
  connection.autoconnect yes \
  connection.autoconnect-priority 100 \
  ipv4.route-metric 300 ipv6.route-metric 300 \
  connection.permissions "user:$USER" \
  802-11-wireless-security.psk-flags 1
```

This gives the private WiFi a higher autoconnect priority and better route
metric than the guest WiFi.

### Ethernet priority

List configured Ethernet connections:

```sh
nmcli connection show
```

Prefer Ethernet over WiFi by giving the Ethernet connection a lower route metric:

```sh
nmcli connection modify "EthernetID" ipv4.route-metric 100 ipv6.route-metric 100
```

The intended priority order is therefore:

1. Ethernet: route metric `100`.
2. Private WiFi: route metric `300`.
3. Guest WiFi: route metric `500`.

## 6. Optional fingerprint authentication

For machines with a supported fingerprint reader, install the fingerprint
packages and enroll the current user.

```sh
sudo zypper install fprintd fprintd-pam
sudo fprintd-enroll "$USER"
```

If fingerprint authentication remains buggy in the DMS lock screen or greeter,
disable the general fingerprint feature and enable fingerprint authentication
only for `sudo`.

## 7. Important generated files and directories

The setup creates and uses the following locations:

| Location                                               | Purpose                                                                             |
|--------------------------------------------------------|-------------------------------------------------------------------------------------|
| `~/.local/share/dots`                                  | Dotfiles repository and setup scripts.                                              |
| `~/.local/state/setup`                                 | Timestamped setup logs created by `run-setup-script`.                               |
| `~/.cache`                                             | XDG cache directory.                                                                |
| `~/.config`                                            | XDG configuration directory.                                                        |
| `~/.local/share`                                       | XDG data directory.                                                                 |
| `~/.local/state`                                       | XDG state directory.                                                                |
| `~/.local/bin`                                         | User-local executables and symlinks.                                                |
| `~/.ssh/${USER}_ed25519_personal`                      | Personal SSH private key.                                                           |
| `~/.ssh/${USER}_ed25519_work`                          | Work SSH private key.                                                               |
| `~/.config/systemd/user/ssh-agent.service`             | User-level SSH agent service.                                                       |
| `~/.local/share/.authinfo.gpg`                         | Encrypted Authinfo file for mail and forge credentials.                             |
| `~/.local/share/mail`                                  | Maildir root used by isync, mu, and mu4e.                                           |
| `~/.config/systemd/user/emacs.service`                 | User-level Emacs daemon service.                                                    |
| `~/.local/share/applications/emacsclient.desktop`      | Desktop entry for opening files in Emacsclient.                                     |
| `~/.local/share/applications/emacsclient-mail.desktop` | Desktop entry for composing `mailto:` URLs in Emacsclient.                          |
| `~/.local/share/srcs`                                  | Source checkouts and downloaded tool trees such as EasyCrypt, cvc5, Z3, and ble.sh. |
| `~/.local/share/qmk_firmware`                          | QMK firmware checkout.                                                              |
| `~/projects`                                           | PARA projects directory.                                                            |
| `~/areas`                                              | PARA areas directory.                                                               |
| `~/resources`                                          | PARA resources directory.                                                           |
| `~/archives`                                           | PARA archives directory.                                                            |

## 8. Troubleshooting

### Find the latest setup log

```sh
latest_log=$(ls -t "${XDG_STATE_HOME:-$HOME/.local/state}/setup"/*.log | head -n 1)
printf '%s\n' "$latest_log"
tail -n 100 "$latest_log"
```

### Re-run an idempotent setup script

Most scripts are written to skip existing repositories, keys, and configurations where possible. Re-run through the wrapper:

```sh
run_setup name
```

Check the corresponding log after re-running.

### Confirm user services

```sh
systemctl --user status ssh-agent.service
systemctl --user status emacs.service
systemctl --user status dms.service
```

### Confirm Docker access after reboot

```sh
groups
systemctl status docker
```

The current user should be listed in the `docker` group after logging in again.

### Confirm key development tools

```sh
command -v cargo
command -v rustup
command -v easycrypt
command -v cvc5
command -v z3
command -v elan
command -v docker
```
