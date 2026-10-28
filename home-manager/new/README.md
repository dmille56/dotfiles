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

# X11 applications inside tmux

Connect with X11 forwarding (`ssh -X castle`, or `ssh -X user@host`) and attach
to tmux. New graphical applications still execute on the tmux host, but their
windows appear on the SSH client's display. Each session follows its most
recently attached client. When that client disconnects, it uses the newest
remaining client; with no clients attached, it restores the host's local desktop.
The client computer needs a running X server, and the host needs a logged-in
graphical session for the local fallback.

Home Manager captures the local display and auth path at graphical login and
when opening a local zsh terminal outside tmux. Hooks update tmux's environment
on attach, detach, and session changes. Existing zsh panes refresh `DISPLAY`,
`XAUTHORITY`, and `WAYLAND_DISPLAY` before each command. You can also run
`tmux-refresh-env` manually. The snapshot lives in `$XDG_RUNTIME_DIR` with mode
0600 and contains environment values, not copied X11 cookies.

After applying this configuration for the first time:

1. Run `tmux-display-env save-local` in a graphical terminal on the tmux host,
   outside tmux and SSH, or log out of the desktop and back in.
2. In tmux, run `tmux source-file ~/.config/tmux/tmux.conf` to load the new hooks.
3. Run `source ~/.zshrc` in each existing zsh pane. New shells load it automatically.

Already running GUI applications keep their original display. Likewise, an
already running script or application that launches GUI children keeps its own
environment; restart it from a refreshed shell to use the new display. Plain SSH
without X11 forwarding cannot display windows on the client.

To force a particular display, press **Ctrl-a, then Shift-x** in the tmux client
whose display you want. This identifies the actual client even when several
computers share a pane. Alternatively, in a local graphical terminal or an SSH
terminal with X11 forwarding on the tmux host, outside tmux, run
`tmux-display-env here SESSION` (replace `SESSION` with a name from `tmux ls`).
The selection applies to new launches throughout that session and lasts until
a client attaches, detaches, or changes sessions; automatic routing then resumes.
`tmux-refresh-env` refreshes the current shell immediately if needed.

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
