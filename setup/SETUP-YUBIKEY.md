# YubiKey Setup

This guide configures a YubiKey for:

- FIDO2 and U2F;
- PIV-backed `age` identities;
- FIDO2 disk unlocking;
- hardware-backed SSH keys.

Repeat the procedure for every physical YubiKey. Keep only the key being configured connected.

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

Set the FIDO2 PIN:

```sh
ykman fido access change-pin
```

Check the FIDO2 configuration:

```sh
ykman fido info
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
    -O application=ssh:<application> \
    -O user=<user-name> \
    -C '<comment>' \
    -f ~/.ssh/<key-file>
```

Add the following option when every signature should require the FIDO2 PIN:

```sh
-O verify-required
```

Inspect the generated public key:

```sh
ssh-keygen -lf ~/.ssh/<key-file>.pub
```

Recover resident key handles from the connected YubiKey when needed:

```sh
ssh-keygen -K
```

## 3. Configure PIV

Change the PIV PIN:

```sh
ykman piv access change-pin
```

Change the PIV PUK:

```sh
ykman piv access change-puk
```

Configure a TDES management key compatible with `age-plugin-yubikey`:

```sh
ykman piv access change-management-key \
    --algorithm TDES \
    --protect
```

Check the PIV configuration:

```sh
ykman piv info
```

## 4. Generate an `age` identity

Create a protected directory for local identity files:

```sh
AGE_IDENTITY_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/age/identities"

install -d -m 700 "$AGE_IDENTITY_DIR"
```

Generate an identity:

```sh
age-plugin-yubikey --generate \
    --name <identity-name> \
    --pin-policy once \
    --touch-policy always \
    > "$AGE_IDENTITY_DIR/<identity-file>.txt"
```

List identities available on connected YubiKeys:

```sh
age-plugin-yubikey --list-all
```

Check the local identity files:

```sh
ls -l "$AGE_IDENTITY_DIR"
```

The files should normally have mode `600`. Correct the permissions if necessary:

```sh
chmod 600 "$AGE_IDENTITY_DIR"/*.txt
```

Record the printed `age1...` recipient for each identity.

## 5. Validate encryption

Encrypt a test file using the recorded recipient:

```sh
age \
    -r 'age1...' \
    -o test.txt.age \
    test.txt
```

Decrypt it using the local identity file:

```sh
age \
    --decrypt \
    -i "$AGE_IDENTITY_DIR/<identity-file>.txt" \
    -o test.decrypted.txt \
    test.txt.age
```

Compare the original and decrypted files:

```sh
cmp test.txt test.decrypted.txt
```

A zero exit status indicates success.

## Repeating the setup

The SSH and `age` generation steps can be repeated for:

- different physical YubiKeys;
- different services or applications;
- different security domains or use cases.

Use distinct application strings, usernames, comments, filenames, and identity names where appropriate. Each physical YubiKey generates separate credentials and recipients.
