# :NOTE: Install notes
1.) copy flake.nix to /etc/nixos/flake.nix: `sudo cp flake.nix /etc/nixos/flake.nix`.
2.) change my-machine-id variable to the correct machine (desktop or laptop)
3.) make sure dotfiles are cloned to /home/dono/dotfiles: `git clone https://github.com/dmille56/dotfiles`
4.) Switch to the config: `sudo nixos-rebuild switch --impure`

