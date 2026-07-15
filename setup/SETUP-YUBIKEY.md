# YubiKey Setup

This guide configures a YubiKey for:

- FIDO2 and U2F;
- PIV-backed `age` identities;
- FIDO2 disk unlocking;
- hardware-backed SSH keys.

Repeat the steps for every physical YubiKey. Keep only the key being configured connected.

## 1. Configure enabled applications

Enable FIDO2, U2F, and PIV over USB:

```sh
ykman config usb --enable FIDO2
ykman config usb --enable U2F
ykman config usb --enable PIV
```

Enable them over NFC:

```sh
ykman config nfc --enable FIDO2
ykman config nfc --enable U2F
ykman config nfc --enable PIV
```

Disable unused applications over USB:

```sh
ykman config usb --disable OATH
ykman config usb --disable OPENPGP
ykman config usb --disable HSMAUTH
ykman config usb --disable OTP
```

Disable them over NFC:

```sh
ykman config nfc --disable OATH
ykman config nfc --disable OPENPGP
ykman config nfc --disable HSMAUTH
ykman config nfc --disable OTP
```

Check the result:

```sh
ykman info
```

Only FIDO2, U2F, and PIV should remain enabled.

## 2. Configure FIDO2

Set the FIDO2 PIN and store it safely:

```sh
ykman fido access change-pin
```

Enroll the YubiKey for disk unlocking:

```sh
sudo sdbootutil enroll --method=fido2
```

Keep a tested recovery key or another unlock method available.

Generate a resident SSH key:

```sh
ssh-keygen -t ed25519-sk \
    -O resident \
    -O application=ssh:personal-forges \
    -O user=mm-personal-forges-primary \
    -C 'Personal: forges -- Primary YubiKey' \
    -f ~/.ssh/mm_ed25519_sk_personal_forges_primary
```

Add this option when every signature should require the FIDO2 PIN:

```sh
-O verify-required
```

Change `application`, `user`, comment, filename, and `primary` or `secondary` according to the key and its purpose.

## 3. Configure PIV

Change the PIV PIN:

```sh
ykman piv access change-pin
```

Change the PIV PUK and store it safely:

```sh
ykman piv access change-puk
```

Configure a TDES management key compatible with `age-plugin-yubikey`:

```sh
ykman piv access change-management-key \
    --algorithm TDES \
    --protect
```

## 4. Generate `age` identities

Create a protected directory for the local identity files:

```sh
AGE_IDENTITY_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/age/identities"

install -d -m 700 "$AGE_IDENTITY_DIR"
umask 077
```

Generate the personal file-encryption identity:

```sh
age-plugin-yubikey --generate \
    --name age_mm_personal_file_enc_primary \
    --pin-policy once \
    --touch-policy always \
    > "$AGE_IDENTITY_DIR/age_mm_personal_file_enc_primary.txt"
```

Generate the research file-encryption identity:

```sh
age-plugin-yubikey --generate \
    --name age_mm_research_file_enc_primary \
    --pin-policy once \
    --touch-policy always \
    > "$AGE_IDENTITY_DIR/age_mm_research_file_enc_primary.txt"
```

For another YubiKey, replace `primary` with `secondary`, `tertiary`, or another consistent role name.

List the configured identities:

```sh
age-plugin-yubikey --list-all
```

Record the printed `age1...` recipients. Each physical YubiKey has its own recipient.

## 5. Final checks

For every YubiKey, verify that:

- FIDO2, U2F, and PIV are enabled;
- unused applications are disabled;
- the FIDO2 PIN, PIV PIN, and PIV PUK are stored safely;
- disk unlocking works;
- the SSH public key is registered where needed;
- the personal and research `age` identities are recorded;
- a separate recovery method remains available.
