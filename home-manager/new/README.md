# :NOTE: Install notes
1.) copy flake.nix to /etc/nixos/flake.nix: `sudo cp flake.nix /etc/nixos/flake.nix`.
2.) change my-machine-id variable to the correct machine (desktop or laptop)
3.) make sure dotfiles are cloned to /home/dono/dotfiles: `git clone https://github.com/dmille56/dotfiles`
4.) Switch to the new config: `sudo nixos-rebuild switch --impure`

# Non nix install:
1. setup syncthing directories
2. setup firefox / chrome extensions
  - vimium, ublock origin, stylus, location guard, vihn, old reddit redirect, reddit enhancement suite, what hacker news says, dracula theme, blocktube, unhook
3. setup vscode extensions
  - python, vscode neovim, direnv, dracula theme, grafana alloy, haskell, nix, c#

# Update / Managing NixOS Config
- Apply new/updated nixos config: `sudo nixos-rebuild switch`
- How to update nix pkgs: `cd /etc/nixos && sudo nix flake update`
- Garbage collect nix: `nix-collect-garbage -d`


