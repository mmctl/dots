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
- Disk:
  - Guided Setup ->
        - Choose what to do with Linux/other partitions:
          [IF WANT SINGLE FRESH INSTALL] Remove even if not needed
        - Enable Disk Encryption:
          - Authentication:
            Password only (for now, will set up after first boot)
        - Settings for the root partition:
          - File system type: Btrfs
            Enable snapshots
          (No "Propose separate home partition")
          - Propose separate swap partition
            (No "Enlarge to RAM size for suspend", unless need/want hibernation)
- Installation Settings/Overview:
  - Booting ->
    - Boot loader type:
      - [If disk encryption] Systemd boot
      - [Else] Anything (GRUB2 for EFI gives nicer menu)
  - Software ->
    - Patterns (select manually):
      - Graphical Environments:
        - Fonts
      - Base Technologies:
        - Kernel dump tooling
        - Base System
        - Enhanced Base System
        - SELinux Support
        - x86-64-v3 optimized packages
        - [If laptop] Mobile
        - YaST Base Utilities
        - YaST Desktop Utilities
        - Minimal Appliance Base
      - Documentation:
        - Help and Support Documentation
        - Documentation

After the first boot, continue with the steps below.

## 0. [If disk encryption] Set up recovery and authentication methods for disk encryption

Before proceeding, ensure that the LUKS password or recovery key is available.
This credential remains the fallback if TPM unlocking fails.

Inspect the encrypted devices and their existing enrollment slots:

```bash
sudo sdbootutil list-devices
sudo systemd-cryptenroll /dev/nvme0n1p2 # Replace with actual name of device
```

Each device should have an independent `password` and/or `recovery` enrollment.
If it does not have a `recovery` enrollment, create one:

```sh
sudo sdbootutil enroll --method=recovery-key
```

*Write the printed recovery key down on a piece of paper, and store it securely.*

Then, depending on your preferences, you can create a (1) TPM2 enrollment, preferably with PIN, and/or
(2) FIDO2 enrollment, preferably two (one for a primary key, and one for a backup key).

For a TPM2 enrollment with PIN (remove the `+pin` if you prefer without PIN):

```sh
sudo sdbootutil enroll --method=tpm2+pin
```

For a FIDO2 enrollment with PIN and touch, ensure a PIN has been set on the key (for its FIDO2 application) before enrolling;
otherwise, a touch-only enrollment is created, and re-enrollment is necessary to utilize a PIN (if set at a later point).

For both types of FIDO2 enrollment, however, the enrollment process is the same.
First insert your hardware key to check whether it is detected:

```sh
sudo systemd-cryptenroll --fido2-device=list
```

If your key is detected, create the enrollment:

```sh
sudo sdbootutil enroll --method=fido2
```

Repeat the FIDO2 enrollment process for each (backup) key, if you have any.

After completing your enrollments, regenerate/update the PCR 15 predictions:

```sh
sudo sdbootutil update-predictions --measure-pcr
```

If no error occurs during any of these commands, reboot and test out every enrollment at least once, including the recovery key.
Do not reboot if any of these commands report an error.

If everything works as expected, you may remove the password if you wish:

```sh
sudo sdbootutil unenroll --method=password
```

Last, but definitely not least, backup the LUKS header of each encrypted device:

```sh
LUKS_DEVICE=/dev/nvme0n1p2 # Replace with actual name of encrypted device
LUKS_UUID="$(sudo cryptsetup luksUUID "$LUKS_DEVICE")"
LUKS_BACKUP_NAME="luks-header-${LUKS_UUID}.img"
sudo cryptsetup luksHeaderBackup --header-backup-file="$LUKS_BACKUP_NAME" "$LUKS_DEVICE"
```

Including the LUKS UUID in the filename makes it clear which encrypted volume
the backup belongs to. During recovery, you are responsible for pairing the
header backup with the correct encrypted device.

Store the header backup somewhere safe and externally, that is, not on the same
physical drive as the encrypted volume. Preferably, also test that each
backed-up header can successfully unlock its corresponding volume. This process
is described [at the end of these setup instructions](#testing-recovery-of-luks-headers).

## 1. [If not automatically connected to network] Connect to the network

Ensure that NetworkManager is enabled.

```sh
sudo systemctl enable --now NetworkManager.service
```

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

```sh
$DOTS_DIR/setup/scripts/
```

The dotfiles repository itself is stored at:

```sh
$XDG_DATA_HOME/dots
```

With the default values above, that resolves to:

```sh
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

This script prepares SSH, GPG, age, the password-manager command-line client,
and the VPN client.

Manual checkpoint: enroll fingerprints and extract any relevant SSH/age identities
from hardware keys. Reboot.

### 4.5 Install the desktop environment

```sh
run_setup de-base-dms  # DankMaterialShell
run_setup de-base-noct # Noctalia
```

This script installs the Wayland desktop stack. Specifically, it installs Niri,
DMS or Noctalia (Quickshell), greetd, Kitty, fonts, desktop integration tools,
and related utilities. It also enables the DMS user service and the greetd
system service.

Manual checkpoint: reboot after this step so that the greeter, user services,
shell configuration, and desktop stack are started cleanly. Login with your password
to initialize the keyring.

After reboot, install additional graphical applications/extensions for the
desktop environment:

```sh
run_setup de-ext
```

This installs Nautilus (graphical file management), Flatpak (with
Flathub), Zen browser, Signal, and Zotero.

Manual checkpoint: Open Zen browser and log in to Proton, checking "Keep me
signed in" on a trusted device. Then, install the following extensions (still in
Zen browser):

- Proton Pass.
- uBlock Origin, including the desired filter lists.
- Zotero Connector.

Also, connect Signal, and log in to Zotero. Reboot after this step.


### 4.6 Install productivity tools

Before running the productivity script, a few manual steps are required (which
we opt for due to current lack of upstream programmatic/unattended installation
options).

First, install [Proton Drive
CLI](https://proton.me/download/drive/cli/index.html) and [Proton
Bridge](https://proton.me/support/protonmail-bridge-install).

Log in to Proton Pass CLI:

```sh
pass-cli login
```

Log in to Proton Drive:

```sh
proton-drive auth login
```

Launch Proton Bridge, log in, and [enable "Split
addresses"](https://proton.me/support/difference-combined-addresses-mode-split-addresses-mode).
Then, export the Bridge-local TLS certificate (from Advanced Settings), and store the certificate at
`$XDG_CONFIG_HOME/proton/bridge/cert.pem` (delete the private key).

Store the Bridge-local IMAP/SMTP credentials in the keyring, once for each Proton account (this is for retrieval through `mbsync`):

```sh
# Enter password reported in Bridge when asked
secret-tool store \
    --label='Proton Bridge (personal)' \
    service proton-bridge \
    account personal
```

And once per email address associated with a Proton account (this is for sending and receiving through Emacs):

```sh
# Replace values with actual host, port, and user reported in Bridge
# Enter password reported in Bridge when asked
secret-tool store \
    --label='Proton Bridge IMAP (example@proton.me)' \
    service proton-bridge \
    host 127.0.0.1 \
    port 1143 \
    user example@proton.me

secret-tool store \
    --label='Proton Bridge SMTP (example@proton.me)' \
    service proton-bridge \
    host 127.0.0.1 \
    port 1025 \
    user example@proton.me

# Replace values with actual host, port, and user reported in Bridge
# Enter password reported in Bridge when asked
secret-tool store \
    --label='Proton Bridge IMAP (example@mmeijers.com)' \
    service proton-bridge
    host 127.0.0.1 \
    port 1143 \
    user example@mmeijers.com

secret-tool store \
    --label='Proton Bridge SMTP (example@mmeijers.com)' \
    service proton-bridge
    host 127.0.0.1 \
    port 1025 \
    user example@mmeijers.com
```

At this point, run the actual productivity setup:

```sh
run_setup productivity
```

This script installs and prepares the main productivity applications. It sets up
Emacs and its user service, installs mail tooling such as isync and mu, and
Myspell dictionaries, prepares the Maildir layout, and configures desktop entries
and MIME handlers for Emacsclient.


### 4.7 Install the full development environment

```sh
run_setup devel
```

This script installs the larger development environment for writing,
programming, formal methods, proof engineering, and container work. In
particular, it installs TeX Live, Rust components, Go, C/C++, Python tooling,
opam and OCaml tooling, EasyCrypt, Lean, Docker, and
QMK-related tools.

Manual checkpoint: reboot after this step so that group membership changes,
especially Docker group membership, take effect.

### 4.8 Create the PARA workspace and project checkouts

```sh
run_setup para
```

This script creates the personal PARA directory structure and checks out project
repositories.

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
sudo fprintd-enroll -f right-index-finger "$USER"
sudo fprintd-enroll -f left-index-finger "$USER"
```

If fingerprint authentication remains buggy in the lock screen or greeter,
try disabling the general fingerprint feature and enable fingerprint authentication
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
| `~/.ssh/${USER}_ed25519_professional`                          | Professional SSH private key.                                                               |
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

## 8. Testing and Troubleshooting

### Testing recovery of LUKS headers

Boot from a Linux live or rescue medium and set the encrypted device and corresponding
header-backup file:

```sh
LUKS_DEVICE=/dev/nvme0n1p2 # Replace with actual name of encrypted device
LUKS_BACKUP_FILE=/path/to/backup-header-file-<luksUUID>.img # Replace with actual path to backup header file for LUKS_DEVICE
LUKS_MAPPING_NAME=luks-recovery # Arbitrary temporary mapping name
```

Test the backup by using it as a detached header:

```sh
sudo cryptsetup open \
    --readonly \
    --header "$LUKS_BACKUP_FILE" \
    "$LUKS_DEVICE" \
    "$LUKS_MAPPING_NAME"

sudo blkid "/dev/mapper/$LUKS_MAPPING_NAME"
```

Enter a recovery key or other credential that was valid when the header backup was
created. The test succeeds if the mapping opens and `blkid` recognizes the expected
filesystem or storage layout.

Close the temporary LUKS mapping:
```
sudo cryptsetup close "$LUKS_MAPPING_NAME"
```

If the recovery test succeeds, you can perform the actual recovery if needed:

```sh
sudo cryptsetup luksHeaderRestore \
    --header-backup-file "$LUKS_BACKUP_FILE" \
    "$LUKS_DEVICE"
```

This replaces the current LUKS header, keyslots, and token enrollments with those
contained in the backup. After restoring the header, review the enrollments and create
a new header backup.

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
