# openSUSE Tumbleweed setup

This guide documents the setup flow for a personal openSUSE Tumbleweed machine.
It starts from a minimal desktop-oriented installation, fetches the dotfiles
repository, and then applies the setup scripts in the intended order. The
scripts do most of the system configuration, but a few manual steps remain.

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
      - Systemd boot
  - Software ->
    - Patterns (select manually):
      - Graphical Environments:
        - Fonts
      - Base Technologies:
        - Base System
        - Enhanced Base System
        - SELinux Support
        - x86-64-v3 optimized packages
        - [If laptop] Mobile
        - YaST Base Utilities
        - YaST Desktop Utilities
        - Minimal Applicance Base
      - Documentation:
        - Help and Support Documentation
        - Documentation

After the first boot, continue with the steps below.


## 0. Fundamental system setup and security

## 0.1 Configure access control for random-seed file

The random-seed file on the EFI System Partition should not be accessible to
unprivileged users. Check whether it is:

```sh
sudo bootctl random-seed
```

If this reports that the file is world-accessible,
add `fmask=0177,dmask=0077` to the existing `/boot/efi` VFAT mount
options in `/etc/fstab`. Then, remount the file system:

```sh
sudo umount /boot/efi
sudo mount /boot/efi
```

Recheck:

```sh
bootctl random-seed
```

## 0.2 [If disk encryption] Set up recovery and authentication methods for disk encryption

Before proceeding, ensure that the LUKS password or recovery key is available.
This credential remains the fallback if TPM2 or FIDO2 unlocking fails.

Inspect the encrypted devices tracked by `sdbootutil` and their existing enrollment slots:

```bash
sudo sdbootutil list-devices
sudo systemd-cryptenroll /dev/nvme0n1p2 # Replace with actual name of device
```

Each device should have at least one `password` and/or `recovery` enrollment.
If there is no `recovery` enrollment, create one:

```sh
sudo sdbootutil enroll --method=recovery-key
```

*Write the printed recovery key down and store it securely, either physically on
a piece of paper, digitally in a password manager, or both.*

Then, depending on your preferences, you can create a (1) TPM2 enrollment, preferably with PIN, and/or
(2) FIDO2 enrollment, preferably two (one for a primary key, and one for a backup key).

For a TPM2 enrollment with PIN (remove the `+pin` if you prefer without PIN):

```sh
sudo sdbootutil enroll --method=tpm2+pin
```

For a FIDO2 enrollment with PIN and touch, ensure a PIN has been set on the key
(for its FIDO2 application) before enrolling; otherwise, a touch-only enrollment
is created, and re-enrollment is necessary to utilize a PIN (if set at a later
point). For YubiKeys, setup instructions can be found in
[SETUP-YUBIKEY.md](./SETUP-YUBIKEY.md) (but goes beyond what is strictly needed
here, also covering what is expected in the remainder of the setup).

For both types of FIDO2 enrollment, however, the enrollment process is the same.
First insert your hardware key to check whether it is detected:

```sh
sudo systemd-cryptenroll --fido2-device=list
```

If your key is detected, create the enrollment for each LUKS device tracked by
`sdbootutil` (i.e., each device shown by `sudo sdbootutil list-devices`).
Ensure that only the hardware key currently being enrolled is inserted:

```sh
sudo systemd-cryptenroll \
    --fido2-device=auto \
    --fido2-with-client-pin=yes \
    --fido2-with-user-presence=yes \
    /dev/nvme0n1p2 # Replace with actual name of device
```

Repeat this command for each tracked LUKS device. Then repeat the complete
FIDO2 enrollment process for each backup key, if you have any.

After all FIDO2 enrollments, ensure that every applicable `/etc/crypttab` entry
contains `fido2-device=auto`, e.g.:

```sh
cr_swap UUID=X none fido2-device=auto
cr_root UUID=Y none x-initrd.attach,fido2-device=auto
```

After validating `/etc/crypttab`, regenerate the initrd and boot entries:

```
sudo sdbootutil mkinitrd
```

If TPM2 unlocking was enrolled, regenerate/update the PCR 15 predictions:

```sh
sudo sdbootutil update-predictions --measure-pcr
```

If no error occurs during any of these commands, reboot and test out every enrollment at least once, including the recovery key.
Do not reboot if any of these commands report an error.

If all tracked devices have a tested recovery enrollment, you may remove all password enrollments if you wish:

```sh
sudo sdbootutil unenroll --method=password
```

Last, but definitely not least, backup the LUKS header of each tracked device:

```sh
LUKS_DEVICE=/dev/nvme0n1p2 # Replace with actual name of device
LUKS_UUID="$(sudo cryptsetup luksUUID "$LUKS_DEVICE")"
LUKS_BACKUP_DIR=/path/to/secure/external/location # Replace with actual path to external location
LUKS_BACKUP_NAME="luks-header-${LUKS_UUID}.img"
sudo cryptsetup luksHeaderBackup --header-backup-file="$LUKS_BACKUP_DIR/$LUKS_BACKUP_NAME" "$LUKS_DEVICE"
```

Including the LUKS UUID in the filename makes it clear which encrypted volume
the backup belongs to. During recovery, you are responsible for pairing the
header backup with the correct encrypted device.

Store the header backup somewhere safe and externally, that is, not on the same
physical drive as the encrypted volume. An automated backup-setup script is
included that can do this for you, alongside setting up general backups; this is
covered [in a later section](#3.9-set-up-automated-backups) Preferably, also
test that each backed-up header can successfully unlock its corresponding
volume. This process is described [at the end of these setup
instructions](#testing-recovery-of-luks-headers).

Create a new header backup whenever enrollment slots are changed, and securely
remove obsolete backups that should no longer remain usable.

## 0.2. [If not automatically connected to network] Connect to the network

Enable and start NetworkManager:

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
nmcli  --ask device wifi connect "WiFiSSID"
```

This creates a persistent NetworkManager connection profile. The later manual
networking section adjusts or removes this profile as appropriate.

## 1. Fetch the dotfiles and setup scripts

Create the expected XDG data and state directories, install the bootstrap tools
needed to clone the repository and run the initial setup scripts, and clone the
dotfiles repository.

```sh
sudo zypper dup

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

## 2. Understand the setup runner and logs

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


## 3. Run the automated setup steps

Run the scripts in the order shown here. Several scripts print `ACTION` lines at
the end; treat those as manual checkpoints before relying on the affected
subsystem.

### 3.1 Configure Snapper for `/home`

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

### 3.2 Install the base profile and system structure

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

### 3.3 Install development and build essentials

```sh
run_setup devel-base
```

This script installs the basic development toolchain required by later scripts.
It also installs or updates the stable Rust toolchain and its standard
development components through `rustup`.

Manual checkpoint:

```text
Ensure $CARGO_HOME/bin is on PATH.
```

The base profile should already place this directory on `PATH`. Confirm with:

```sh
echo $PATH
```

### 3.4 Configure security, SSH, GPG, and secrets

```sh
run_setup security
```

This script configures the wheel-based sudo policy and, when available,
fingerprint authentication for sudo. It installs and configures SSH, GPG,
age, and YubiKey support; sets up the systemd SSH agent; imports the Proton
Bridge package-signing key; and installs the Proton Pass and Proton VPN
command-line clients.

Manual checkpoint: if fingerprint authentication was enabled, enroll and verify
the desired fingerprints. Extract and store the relevant SSH and age identities
from each hardware key using the names expected by the stowed configuration.

Reboot so that the new wheel-group membership and user-session environment
configuration take effect.

### 3.5 Install the desktop environment

```sh
run_setup de-base
```

This script installs and configures the Wayland desktop stack. Specifically, it
installs Niri, Noctalia Shell, greetd, Kitty, graphical toolkits and fonts,
desktop integration tools, and shell enhancements. It configures greetd to
start a Niri session and enables the greetd system service.

Manual checkpoint: reboot after this step so that greetd and the newly configured
desktop environment start cleanly. Log in with your password to initialize and
unlock the login keyring.

After reboot, install additional graphical applications/extensions for the
desktop environment:

```sh
run_setup de-ext
```

This installs Nautilus for graphical file management, Flatpak with the Flathub
user remote, Zen Browser, Signal, and Zotero.

Manual checkpoint: open Zen browser and complete its setup to
preference, e.g.:
- Set it as default browser
- Add exceptions for frequently used services to remember logins
  - Log in to these frequently used services (e.g., GitHub, Proton).
- Install extensions:
  - Proton Pass
  - uBlock Origin, including the desired filter lists
  - Zotero Connector

Also, link Signal, and log in to Zotero.


### 3.6 Install productivity tools

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

Log in to Proton Drive (and make sure the relevant data is available at the
expected paths therein, see script):

```sh
proton-drive auth login
```

Launch Proton Bridge, log in, and [enable "Split
addresses"](https://proton.me/support/difference-combined-addresses-mode-split-addresses-mode).
Then export the Bridge-local TLS certificate from Advanced Settings and store
the certificate at `$XDG_CONFIG_HOME/proton/bridge/cert.pem`. Delete the private
key from the exported file, if it is included. Make the Bridge directory
accessible only to the current user:

```sh
install -d -m 700 -- $XDG_CONFIG_HOME/proton/bridge
```

Store the Bridge-local IMAP/SMTP credentials in the keyring, once for each
Proton account (this is for retrieval through `mbsync`):

```sh
# Enter password reported in Bridge when asked
secret-tool store \
    --label='Proton Bridge (personal)' \
    service proton-bridge \
    account personal
```

Also store address-specific IMAP and SMTP credentials once per email address
(for use by the Emacs mail configuration):

```sh
# Replace values with actual host, port, and user reported in Bridge
# Enter password reported in Bridge when asked
secret-tool store \
    --label='Proton Bridge IMAP (example@proton.me)' \
    host 127.0.0.1 \
    port 1143 \
    user example@proton.me

secret-tool store \
    --label='Proton Bridge SMTP (example@proton.me)' \
    host 127.0.0.1 \
    port 1025 \
    user example@proton.me

# Replace values with actual host, port, and user reported in Bridge
# Enter password reported in Bridge when asked
secret-tool store \
    --label='Proton Bridge IMAP (example@mmeijers.com)' \
    host 127.0.0.1 \
    port 1143 \
    user example@mmeijers.com

secret-tool store \
    --label='Proton Bridge SMTP (example@mmeijers.com)' \
    host 127.0.0.1 \
    port 1025 \
    user example@mmeijers.com
```

At this point, run the actual productivity setup:

```sh
run_setup productivity
```

This script creates the encrypted authinfo file using credentials retrieved from
Proton Pass and the age recipients created during the security setup. It creates
and synchronizes the Maildir layout, configures headless Proton Bridge autostart,
initializes the mu database, and installs the spellchecker and dictionaries.

It also installs and configures Emacs, its graphical user service and primer
service, downloads data from Proton Drive, and configures desktop
entries and MIME handlers for Emacsclient.

Manual checkpoint: start a non-daemon Emacs instance:

```sh
emacs
```

Load the installation entry point and wait for native compilation to finish:

```elisp
(load (expand-file-name "emacs/install/install-entry.el"
                        (getenv "XDG_DATA_HOME")))
```

Add any additional required credential entries to `$AUTHINFO_FILE`, such as
access tokens for other forge accounts. Reboot after completing these steps.

### 3.7 Install the full development environment

```sh
run_setup devel
```

This script installs the larger development environment for writing,
programming, formal methods, proof engineering, and container work. In
particular, it installs TeX Live, Rust components, Go, C/C++, Python tooling,
opam and OCaml tooling, EasyCrypt without SMT provers, Lean, and Docker.

Manual checkpoint: install compatible SMT provers (e.g., Z3 and CVC5), and
configure EasyCrypt/Why3:

```sh
easycrypt why3config
```

Then reboot so that the development environment variables and group membership
changes, especially Docker group membership, take effect.

### 3.8 Create the PARA workspace and project checkouts

```sh
run_setup para
```

This script creates the personal PARA directory structure, installs QMK and
Hugo, and checks out the configured project and area repositories. It also
initializes the QMK firmware checkout and creates the configured QMK keymap
symlinks.

This step assumes that the SSH identities configured during setup-security
are available, that their public keys have been added to the relevant forge
accounts, and that the SSH host aliases from the stowed SSH configuration are
active.

### 3.9 Set up automated backups

Ensure that the backup medium is connected and that you are logged in to Proton
Pass CLI:

```sh
pass-cli login
```

Determine the filesystem UUID of the backup medium:

```sh
lsblk --fs
```

Also determine the LUKS UUID of each encrypted device whose header should be
backed up:

```sh
sudo cryptsetup luksUUID /dev/nvme0n1p2 # Replace with actual device
```

The backup location is determined by the following main configuration values:

```text
BACKUP_TARGET_NAME    Name of the backup medium, e.g. primary or secondary
BACKUP_PATH_TO_LABEL  Path below the mountpoint, e.g. personal or professional
MACHINE_LABEL         Stable label identifying the machine
```

By default, these produce the following machine backup root:

```text
/mnt/backup-primary/personal/laptop-lenovo-thinkpad-t14-gen2
```

The corresponding directory must already exist on the backup medium before
running the setup.

Run the setup with the filesystem UUID of the backup medium, followed by any
LUKS UUIDs:

```sh
"$DOTS_DIR/setup/scripts/run-setup-script" \
    "$DOTS_DIR/setup/scripts/setup-backup" \
    FILESYSTEM_UUID \
    LUKS_UUID_1 ... LUKS_UUIDn
```

Omit the LUKS UUID arguments if no LUKS headers should be backed up.

Override the backup labels by setting environment variables for the setup
command. For example, to configure a backup on a secondary medium:

```sh
BACKUP_TARGET_NAME=secondary \
"$DOTS_DIR/setup/scripts/run-setup-script" \
    "$DOTS_DIR/setup/scripts/setup-backup" \
    FILESYSTEM_UUID \
    LUKS_UUID_1 ... LUKS_UUIDn
```

Changing `BACKUP_TARGET_NAME` also changes the default mountpoint. For example,
`secondary` uses `/mnt/backup-secondary`. Set `BACKUP_MOUNTPOINT` explicitly
only when a different mountpoint is desired:

```sh
BACKUP_TARGET_NAME=secondary \
BACKUP_MOUNTPOINT=/mnt/external-backup \
"$DOTS_DIR/setup/scripts/run-setup-script" \
    "$DOTS_DIR/setup/scripts/setup-backup" \
    FILESYSTEM_UUID \
    LUKS_UUID_1 ... LUKS_UUIDn
```

Run the setup separately for each backup medium, using its own filesystem UUID
and a distinct `BACKUP_TARGET_NAME`. The same `BACKUP_PATH_TO_LABEL` and
`MACHINE_LABEL` can be reused across media when they contain equivalent backups.

This script installs and stows the Restic backup tooling, creates a new
installation-specific backup directory and Restic repository, and optionally
backs up the LUKS headers and metadata of the specified encrypted devices. It
generates a repository password, stores it in Proton Pass, and encrypts it as a
systemd credential for the backup services.

It also creates the repository-specific Restic configuration and prepares the
backup, maintenance, and manual scrub services.

Manual checkpoint: add the `/etc/fstab` entry printed by the script, if it is
not already present, and reload the system configuration:

```sh
sudo systemctl daemon-reload
```

Test the mount and unmount commands printed by the script as the regular user.
Then enable the backup and maintenance timers using the unit names printed by
the script.

Optionally start the printed backup service immediately and inspect its status
to verify the initial backup.

## 4. Manual network configuration

After the main setup, configure persistent network profiles.

### Guest WiFi

Use this for a shared or lower-priority WiFi network where the password may be stored system-wide and the connection is available to everyone.

```sh
nmcli device wifi connect "GuestWiFiSSID" --ask
nmcli connection modify "GuestWiFiSSID" \
  connection.autoconnect yes \
  connection.autoconnect-priority 50 \
  ipv4.route-metric 500 ipv6.route-metric 500
```

This makes the connection automatic, but lower priority than private WiFi and Ethernet.

### Private WiFi

Use this for the main private WiFi network. The password is stored in the user
keyring and the connection is restricted to the current user.

```sh
nmcli device wifi connect "PrivateWiFiSSID" --ask
nmcli connection modify "PrivateWiFiSSID" \
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

## 5. Testing and Troubleshooting

### Testing recovery of LUKS headers

For FIDO2 credentials and recovery keys, preferably boot from a Linux live or
rescue medium. A TPM2 enrollment may depend on the normal boot state and should
therefore generally be tested from the installed system.

Set the encrypted device, corresponding header backup, and an arbitrary temporary
mapping name:

```sh
LUKS_DEVICE=/dev/nvme0n1p2 # Replace with actual encrypted device
LUKS_BACKUP_FILE=/path/to/backup-header-file # Replace with corresponding header backup
LUKS_MAPPING_NAME=luks-recovery
```

Inspect the enrollments stored in the backup:

```sh
sudo cryptsetup luksDump "$LUKS_BACKUP_FILE"
```

Under `Tokens`, hardware enrollments such as `systemd-fido2` and `systemd-tpm2`
are listed with a token identifier and associated keyslot.

To test a specific hardware enrollment:

```sh
TOKEN_ID=0 # Replace with actual token identifier

sudo cryptsetup open \
    --type luks2 \
    --readonly \
    --header "$LUKS_BACKUP_FILE" \
    --token-only \
    --token-id "$TOKEN_ID" \
    --tries 1 \
    --verbose \
    "$LUKS_DEVICE" \
    "$LUKS_MAPPING_NAME"
```

When testing multiple FIDO2 enrollments, connect only the corresponding hardware
key and repeat the command for each token identifier.

For a TPM2 enrollment, run the command in a boot environment that satisfies its
PCR or policy constraints. It should request the TPM2 PIN if one was configured.

For a recovery key, find the keyslot associated with the `systemd-recovery`
token and test it explicitly:

```sh
KEYSLOT=3 # Replace with actual keyslot

sudo cryptsetup open \
    --type luks2 \
    --readonly \
    --header "$LUKS_BACKUP_FILE" \
    --disable-external-tokens \
    --key-slot "$KEYSLOT" \
    --tries 1 \
    --verbose \
    "$LUKS_DEVICE" \
    "$LUKS_MAPPING_NAME"
```

After opening the mapping, verify that it uses the expected device and is
read-only:

```sh
sudo cryptsetup status "$LUKS_MAPPING_NAME"
```

Then check that the decrypted payload has the expected filesystem or storage
container type and UUID:

```sh
sudo blkid -p "/dev/mapper/$LUKS_MAPPING_NAME"
```

The test succeeds when the selected credential opens the mapping and `blkid`
reports the expected payload type and UUID.

Close the mapping after each test:

```sh
sudo cryptsetup close "$LUKS_MAPPING_NAME"
```

Repeat the procedure for every credential that should remain usable.

If the recovery test succeeds and the on-device header needs recovery, close the
temporary mapping and restore the backup:

```sh
sudo cryptsetup luksHeaderRestore \
    --header-backup-file "$LUKS_BACKUP_FILE" \
    "$LUKS_DEVICE"
```

This replaces the current LUKS header, keyslots, and token enrollments with those
contained in the backup. After restoring it, review the enrollments and create a
new header backup.

### Find the latest setup log

```sh
latest_log=$(ls -t "${XDG_STATE_HOME:-$HOME/.local/state}/setup"/*.log | head -n 1)
printf '%s\n' "$latest_log"
tail -n 100 "$latest_log"
```
