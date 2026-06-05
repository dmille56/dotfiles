# AGENTS.md

## Repo Map
- Active NixOS/Home Manager config lives in `home-manager/new/`; `home-manager/` is legacy/manual standalone HM config.
- Shared defaults are in `home-manager/new/common-*.nix`; host-specific entrypoints are `desktop-*.nix` and `laptop-*.nix`.
- Custom package/module helpers live in `nix/` and are imported from the HM modules.
- This repo is expected to stay at `/home/dono/dotfiles`; several Nix imports use that absolute path.
- `secrets.yaml` is SOPS-encrypted; do not edit it as plain YAML.

## Source Of Truth
- Prefer `flake.nix`/module code over README notes when they disagree.
- Most shared settings use `lib.mkDefault`/`lib.mkForce`; put machine-specific overrides in the host modules instead of rewriting shared defaults.

## Apply / Verify
- Do not run `sudo nixos-rebuild`, `sudo nix flake update`, or other `sudo nix` validation commands unless the user explicitly asks.
- Use non-sudo `nix` commands for inspection when needed; leave privileged apply steps to the user.
- If `home-manager/new/flake.nix` changes, keep `my-machine-id` set to the target machine (`desktop` or `laptop`) before rebuilding.
- After updating flakes, the repo notes say the user should run `sudo nix flake update` in `/etc/nixos`, then rebuild manually.
- For the standalone HM setup, use `home-manager switch --impure`; `sudo systemctl restart home-manager-dono.service` reruns activation.

## Host Gotchas
- `desktop` resolves to hostname `castle`; `laptop` resolves to `vladtop`.
- Desktop config enables OpenCLaw, NVIDIA, Tailscale, and an `x11vnc` service bound to SSH-tunneled port `50022`.
- Laptop config disables SSH.
- Keep the OpenCLaw activation hooks in `desktop-home.nix`; they repair symlinked skill files under `~/.openclaw/workspace/skills`.
