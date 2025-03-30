# Yazi (file manager)
## Ensure that, upon exiting, we can cd to CWD
function yazi() {
    local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
    /usr/bin/yazi "$@" --cwd-file="$tmp"
    if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
        builtin cd -- "$cwd"
    fi
    rm -f -- "$tmp"
}
