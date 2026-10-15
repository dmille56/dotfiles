# Emacs package management

Straight installs external Lisp packages; use-package configures them. Automatic package updates are disabled.

## First startup after migration

1. Apply your normal Home Manager/NixOS configuration: common-home.nix manages ~/.emacs.d/early-init.el from emacs-config/early-init.el. On machines without Home Manager, copy the file there before starting Emacs. This prevents package.el from activating old ELPA packages before main.el loads.
2. Fully exit Emacs and start it again. Reloading main.el in an existing session cannot unload the old packages or cancel the old update timer. The first startup downloads and builds packages newly assigned to straight, so it needs network access and may take longer.
3. Open Elfeed once with M-x elfeed so its lazy configuration is registered, and exercise your normal workflows.
4. After a successful startup, run C-u M-x straight-freeze-versions. The prefix skips straight's push/check phase and records local revisions without attempting to push repositories.
5. Commit emacs-config/straight-versions.el with the configuration changes.

The initial lockfile records the HEAD revisions of all 76 pre-existing straight repositories, including recipe repositories. They had no tracked modifications when captured. It is a partial migration baseline: packages previously installed by package.el, including use-package itself, are added by the first freeze after migration. It is not a validated complete installation snapshot. Old ~/.emacs.d/elpa packages remain on disk for recovery but are no longer automatically activated.

## Updating deliberately

1. Start from a committed working configuration and lockfile.
2. Open Elfeed so its packages are registered.
3. Use M-x straight-pull-package-and-deps for a selected package, or M-x straight-pull-all for a deliberate full update.
4. Restart Emacs and check the workflows you use.
5. Open Elfeed again, then run C-u M-x straight-freeze-versions and review/commit the lockfile diff.

Do not freeze broken versions over your last working lockfile. No updates or thaws run automatically during startup. Lockfiles select revisions for newly cloned repositories; existing repositories need an explicit thaw to return to recorded revisions.

## Restoring a saved version set

Restore the desired straight-versions.el from Git, open Elfeed to register its packages, then run M-x straight-thaw-versions and restart Emacs. Review any prompts about local repository changes. If init reports an error after straight has loaded, Emacs normally remains usable: run the thaw command there and restart. If the bootstrap itself cannot load, restore the previous configuration from Git first. If changed bytecode remains after thawing, run M-x straight-rebuild-all and restart again.

The lockfile pins Lisp repository commits, not Emacs, Nix packages, external tools, or local development checkouts.

## Exceptions

- Nix supplies vterm, w3m, and jinx on Linux. They are registered as pseudo built-ins so dependencies do not cause straight to install another copy. Jinx's configuration remains commented out.
- Built-in smerge-mode, re-builder, erc, savehist, which-key, and editorconfig opt out with :straight nil. This assumes the current Emacs includes which-key and editorconfig (Emacs 30 or newer).
- vertico-directory is supplied by vertico; its declaration opts out of separate installation.
- EAF keeps its manual checkout and installation workflow.
- ufo-catcher, treesit-jump, and powershell-ts-mode use local checkouts when present, with straight fallbacks. Local declarations explicitly opt out of straight's default installer.
- Existing explicit recipes continue selecting their configured repositories.
- Ranger skips byte/native compilation with :build (:not compile): its optional Hydra macros are not declared as a build dependency, which otherwise produces bytecode that fails with void-variable ranger-hydra-go. Loading its source with Hydra available avoids that error.

See [PACKAGE-CATALOG.md](PACKAGE-CATALOG.md) for the package inventory.
