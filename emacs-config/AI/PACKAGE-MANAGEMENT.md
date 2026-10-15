# Package management

Straight installs packages; `use-package` configures them. Updates are manual.
The version lockfile is [straight-versions.el](../straight-versions.el).

## Update and save

1. Start with a committed working config and lockfile. Open `M-x elfeed` to register its lazy-loaded packages.
2. Run `M-x straight-pull-package-and-deps` for one package, or `M-x straight-pull-all` for everything.
3. Restart Emacs and check your normal workflows.
4. Open Elfeed again, then run `C-u M-x straight-freeze-versions RET` to record working revisions without attempting Git pushes.
5. Review and commit the lockfile with any config changes. Freeze only after confirming the setup works.

## Restore

Restore the desired lockfile from Git, open Elfeed, run `M-x straight-thaw-versions`, then restart Emacs. If stale compiled code persists, run `M-x straight-rebuild-all` and restart again.

Existing repositories need an explicit thaw to restore locked revisions. The lockfile does not pin Emacs, Nix packages, external tools, or local checkouts.

## New machines

Home Manager installs [early-init.el](../early-init.el) as `~/.emacs.d/early-init.el` to disable package.el activation. Copy it manually on machines without Home Manager. Install it before launching Emacs; the first startup downloads/builds missing packages.

See [PACKAGE-CATALOG.md](PACKAGE-CATALOG.md) for installation exceptions.
