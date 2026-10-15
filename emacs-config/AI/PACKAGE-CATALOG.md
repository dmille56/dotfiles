# Package installation rules

Straight manages external packages by default in [main.el](../main.el), [linux-specific.el](../linux-specific.el), and [elfeed-config.el](../elfeed-config.el). These declarations are the package inventory; custom `:straight` recipes select specific repositories.

When adding a package, use an ordinary `(use-package package-name ...)` declaration. Use `:straight nil` for libraries supplied elsewhere. `:defer` controls loading, not necessarily installation.

## Exceptions to preserve

| Packages | Source / rule |
| --- | --- |
| `vterm`, `w3m`, `jinx` | Nix on Linux, configured in [common-home.nix](../../home-manager/new/common-home.nix). Registered as pseudo built-ins to avoid duplicate dependency installs. Jinx's Emacs configuration is commented out. |
| `editorconfig`, `erc`, `re-builder`, `savehist`, `smerge-mode`, `which-key` | Built into current Emacs; `:straight nil`. Assume Emacs 30 or newer. |
| `vertico-directory` | Included in `vertico`; `:straight nil`. |
| `eaf` | Manual checkout at `~/.emacs.d/site-lisp/emacs-application-framework`; `:straight nil`. |
| `powershell-ts-mode`, `treesit-jump`, `ufo-catcher` | Prefer `~/Desktop/prog/<package>/` with `:straight nil`; otherwise install through straight. |
| `ranger` | Keep `:build (:not compile)`. Its undeclared optional Hydra dependency otherwise produces broken bytecode (`void-variable ranger-hydra-go`). |

See [PACKAGE-MANAGEMENT.md](PACKAGE-MANAGEMENT.md) for updates, freezing, and recovery.
