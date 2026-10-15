# Package management

Straight installs packages; `use-package` configures them. Updates are manual.
The version lockfile is [straight-versions.el](../straight-versions.el).

## Clone depth

`main.el` sets `straight-vc-git-default-clone-depth` to `1` before bootstrap.
New package and recipe repository clones use shallow history unless their
recipes override it. Straight can fetch locked commits directly; if a server
does not support a shallow fetch, straight falls back to a full clone.

Full-history exceptions:

- `org` explicitly uses `:depth full`, preserving the inherited recipe and its
  build steps. Its build reads release tags to generate version information.
  This also protects packages that depend on Org, including Elfeed; Elfeed
  itself does not need a full clone.
- Straight's built-in `nongnu-elpa` recipe uses `:depth (full single-branch)`.
  Keep that upstream recipe setting. Packages retrieved from GNU/NonGNU ELPA
  source branches can also have their own full-history recipes.

The installed recipe cache and package sources were inspected when enabling
this setting. No additional required package overrides were found. Magit and
Org Roam can report a commit hash without tags; Yasnippet's `yas-about` can
report an unknown version. Their normal functionality does not require their
own repository history.

For another package that needs history, add `:depth full` to its existing
`:straight` recipe, or use `:straight (package-name :depth full)` if it has no
custom recipe. Register the override before packages that depend on it.

Changing the default does not shrink existing clones. Reclaiming their space
requires a separate migration after checking for local changes and commits;
do not delete repositories with uncommitted or unpushed work. Straight may
also automatically unshallow repositories for later version-control operations.

## Update and save

1. Commit the working config and lockfile.
2. Run `M-x straight-pull-package-and-deps` for one package, or `M-x straight-pull-all` for everything.
3. Restart Emacs and check your normal workflows.
4. If working, run `C-u M-x straight-freeze-versions RET`, then review and commit the lockfile and config changes.

## Restore

Restore the desired lockfile from Git, run `M-x straight-thaw-versions`, then restart Emacs. If stale compiled code persists, run `M-x straight-rebuild-all` and restart again.

Existing repositories need an explicit thaw to restore locked revisions. The lockfile does not pin Emacs, Nix packages, external tools, or local checkouts.

## New machines

Home Manager installs [early-init.el](../early-init.el) as `~/.emacs.d/early-init.el` to disable package.el activation. Copy it manually on machines without Home Manager. Install it before launching Emacs; the first startup downloads/builds missing packages.

After startup, manually run `M-x treesit-auto-install-all RET` to install the Tree-sitter grammars. Installing the Emacs packages does not automatically install all grammars.

See [PACKAGE-CATALOG.md](PACKAGE-CATALOG.md) for installation exceptions.
