# /usr/local/bin
set fish_user_paths $fish_user_paths '/usr/local/bin' '/usr/sbin' '/sbin' '/usr/bin' '/usr/sbin'

if test -e '/usr/libexec'
  set fish_user_paths $fish_user_paths '/usr/libexec'
end

# Latex
if test -e '/usr/local/textlive/2015/bin/x86_64-linux/tex'
  set fish_user_paths $fish_user_paths '/usr/local/texlive/2015/bin/x86_64-linux'
end

# Rust
if test -e "$HOME/.cargo/bin"
  set fish_user_paths $fish_user_paths "$HOME/.cargo/bin"
  set -gx RUST_BACKTRACE 1
end

if test -e "$HOME/.multirust/toolchains"
  set toolchain (rustup show | grep '(default)' | awk '{print $1}')
  if test -e ~/.multirust/toolchains/$toolchain/lib/rustlib/src/rust/src
    set -gx RUST_SRC_PATH ~/.multirust/toolchains/$toolchain/lib/rustlib/src/rust/src
  end
end

# Go-lang
if test -e "$HOME/.go"
  set -xg GOPATH "$HOME/.go"
end


