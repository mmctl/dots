# Checks whether cache file of given name exists and sources that;
# otherwise, evaluated given command and caches output
#
# Call as: _cached_eval <cache_file_name> <executable> <arguments>
# Example: _cached_eval fd-completion fd --gen-completions bash
_cached_eval() {
  local name=$1
  shift

  local cache_root="${XDG_CACHE_HOME:-$HOME/.cache}"
  local cache_dir="$cache_root/bash/eval"
  local cache_file="$cache_dir/$name.bash"
  local bin_path
  local tmp

  bin_path=$(command -v "$1") || return 0

  if [[ -r $cache_file && ! $bin_path -nt $cache_file ]]; then
    . "$cache_file"
    return
  fi

  mkdir -p "$cache_dir" || return 0

  tmp="$cache_file.$$.$RANDOM.tmp"

  if "$@" > "$tmp" && [[ -s $tmp ]] && bash -n "$tmp" 2>/dev/null; then
    mv "$tmp" "$cache_file"
    . "$cache_file"
  else
    rm -f "$tmp"
    [[ -r $cache_file ]] && . "$cache_file"
  fi
}
