# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

export GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles

for i in $GUIX_EXTRA_PROFILES/*; do
  profile=$i/$(basename "$i")
  if [ -f "$profile"/etc/profile ]; then
    GUIX_PROFILE="$profile"
    . "$GUIX_PROFILE"/etc/profile
  fi
  unset profile
done

export NIX_PATH=~/.nix-defexpr/channels/nixos/

. .profile
