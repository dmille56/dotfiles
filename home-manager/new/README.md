# Install notes
1. copy flake.nix to /etc/nixos/flake.nix: `sudo cp flake.nix /etc/nixos/flake.nix`.
2. Choose the machine id for the host (`desktop` or `laptop`)
3. make sure dotfiles are cloned to /home/dono/dotfiles: `git clone https://github.com/dmille56/dotfiles`
4. Switch to the new config, passing the machine id: `MY_MACHINE_ID=desktop sudo nixos-rebuild switch --impure` (use `laptop` for a laptop)

# Non nix install:
1. setup syncthing directories
2. setup firefox / chrome extensions
    - vimium, ublock origin, stylus, location guard, vihn, old reddit redirect, reddit enhancement suite, what hacker news says, dracula theme, blocktube, unhook
3. setup vscode extensions
    - python, vscode neovim, direnv, dracula theme, grafana alloy, haskell, nix, c#
4. install .emacs file `cp ~/dotfiles/emacs-config/default.emacs ~/.emacs`. Home Manager installs `~/.emacs.d/early-init.el`; on machines without Home Manager, copy `~/dotfiles/emacs-config/early-init.el` there before launching Emacs. See [package management](../../emacs-config/AI/PACKAGE-MANAGEMENT.md).
    - After starting Emacs, manually run `M-x treesit-auto-install-all RET` to install the Tree-sitter grammars.
5. Initial pass password store
    1. Generate GPG key: `gpg --full-generate-key`
    2. `pass init`

# Update Emacs packages

1. Commit the working config and `emacs-config/straight-versions.el`.
2. Run `M-x straight-pull-package-and-deps` for one package, or `M-x straight-pull-all` for everything.
3. Restart Emacs and check your usual workflows.
4. If working, run `C-u M-x straight-freeze-versions RET`, then review and commit the lockfile and config changes.

To roll back: restore the previous lockfile from Git, run `M-x straight-thaw-versions`, and restart Emacs. Nix-supplied packages update through Nix.

# Update / Managing NixOS Config
- Apply new/updated nixos config: `MY_MACHINE_ID=desktop sudo nixos-rebuild switch --impure` (use `laptop` for a laptop)
- How to update nix pkgs (NOTE still after run switch command to apply the update): `cd /etc/nixos && sudo nix flake update`
- Remove old nix generations (run before garbage collect) from nixos-rebuild: `sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations old`
  - Have to run to remove them from boot entry: `sudo nixos-rebuild boot` (after garbage collect).
- Garbage collect nix: `nix-collect-garbage -d`... Need to use sudo for system wide cleanup.
- Optimise nix: `nix store optimise`... Need to use sudo for system wide cleanup.
- Re run home-manager activation scripts: `sudo systemctl restart home-manager-dono.service`
- How to test out a program without install (drops you in a shell with that program in it)... Replace hello with the actual pkg you want to use.
    - Classic way: `nix-shell -p hello`
    - Modern way: `nix shell nixpkgs#hello`
- Nix pkgs search: https://search.nixos.org/packages
