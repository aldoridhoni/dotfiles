# bin folders
for path in '/usr/local/bin' '/usr/sbin' '/sbin' '/usr/bin'  '/usr/libexec'
  if [ -d $path -a -r $path ]
    set fish_user_paths $fish_user_paths $path
  end
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
  set -l toolchain (rustup show | grep '(default)' | awk '{print $1}')
  if test -e "$HOME/.multirust/toolchains/$toolchain/lib/rustlib/src/rust/src"
    set -gx RUST_SRC_PATH "$HOME/.multirust/toolchains/$toolchain/lib/rustlib/src/rust/src"
  end
end

# Go-lang
if test -e "$HOME/.go"
  set -xg GOPATH "$HOME/.go"
end

# ruby gem
if test -e "$HOME/.gem/ruby" -a \( (command -v ruby > /dev/null; echo $status) -eq 0 \)
  set -l ruby_ver (ruby -e 'puts RUBY_VERSION')
  if test -e "$HOME/.gem/ruby/$ruby_ver/bin"
    set fish_user_paths $fish_user_paths "$HOME/.gem/ruby/$ruby_ver/bin"
  end
end
