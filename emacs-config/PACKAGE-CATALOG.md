# Emacs package installation catalog

Updated: 2026-10-14, after consolidating installation around straight.

## Installation rules

- Straight is the default installer for use-package declarations. use-package-always-ensure is disabled; package.el initialization, archive refreshes, and automatic updates have been removed.
- Straight itself is bootstrapped, then installs use-package and compat before the rest of the configuration.
- early-init.el disables automatic activation of previously installed package.el packages. Install it in ~/.emacs.d/early-init.el before restarting Emacs.
- Existing custom straight recipes remain in place. :defer controls loading and does not necessarily defer installation.
- The lockfile is [straight-versions.el](straight-versions.el). It initially records 76 existing repositories; the first freeze after migration adds the newly migrated packages.
- This inventory covers active declarations and explicit bootstrap installs, not every transitive dependency or previously installed package. Repeated imenu-list declarations count once. Elfeed's declarations load when Elfeed is opened.

## Straight: 149 named packages

| Package | Declaration | Installation |
| --- | --- | --- |
| `2048-game` | [main.el:1655](main.el#L1655) | Straight by default |
| `aider` | [main.el:1460](main.el#L1460) | Explicit straight recipe/declaration |
| `all-the-icons` | [main.el:1187](main.el#L1187) | Straight by default |
| `all-the-icons-dired` | [main.el:1187](main.el#L1187) | Straight by default |
| `atomic-chrome` | [main.el:1890](main.el#L1890) | Explicit straight recipe/declaration |
| `attrap` | [main.el:740](main.el#L740) | Straight by default |
| `autotetris-mode` | [main.el:1780](main.el#L1780) | Straight by default |
| `avy` | [main.el:1247](main.el#L1247) | Straight by default |
| `beacon` | [main.el:1359](main.el#L1359) | Straight by default |
| `blamer` | [main.el:847](main.el#L847) | Straight by default |
| `casual-avy` | [main.el:2079](main.el#L2079) | Explicit straight recipe/declaration |
| `casual-suite` | [main.el:663](main.el#L663) | Explicit straight recipe/declaration |
| `chatgpt-shell` | [main.el:1374](main.el#L1374) | Explicit straight recipe/declaration |
| `cheat-sh` | [main.el:1233](main.el#L1233) | Straight by default |
| `clippy` | [main.el:1803](main.el#L1803) | Straight by default |
| `command-log-mode` | [main.el:1805](main.el#L1805) | Straight by default |
| `company` | [main.el:1272](main.el#L1272) | Explicit straight recipe/declaration |
| `company-box` | [main.el:1276](main.el#L1276) | Explicit straight recipe/declaration |
| `compat` | [main.el:43](main.el#L43) | Direct straight call |
| `consult` | [main.el:1582](main.el#L1582) | Straight by default |
| `copilot` | [main.el:1495](main.el#L1495) | Explicit straight recipe/declaration |
| `copilot-chat` | [main.el:1501](main.el#L1501) | Explicit straight recipe/declaration |
| `corfu` | [main.el:1509](main.el#L1509) | Straight by default |
| `csproj-mode` | [main.el:808](main.el#L808) | Straight by default |
| `csv-mode` | [main.el:1351](main.el#L1351) | Straight by default |
| `dante` | [main.el:720](main.el#L720) | Straight by default |
| `dashboard` | [main.el:1281](main.el#L1281) | Straight by default |
| `diff-hl` | [main.el:1262](main.el#L1262) | Straight by default |
| `direnv` | [linux-specific.el:19](linux-specific.el#L19) | Straight by default |
| `doom-modeline` | [main.el:1039](main.el#L1039) | Straight by default |
| `dracula-theme` | [main.el:892](main.el#L892) | Straight by default |
| `dumb-jump` | [main.el:1504](main.el#L1504) | Explicit straight recipe/declaration |
| `eask-mode` | [main.el:1925](main.el#L1925) | Straight by default |
| `eat` | [main.el:1782](main.el#L1782) | Explicit straight recipe/declaration |
| `ebuku` | [main.el:1880](main.el#L1880) | Straight by default |
| `eee` | [main.el:2040](main.el#L2040) | Explicit straight recipe/declaration |
| `elfeed` | [elfeed-config.el:61](elfeed-config.el#L61) | Explicit straight recipe/declaration; lazy Elfeed config |
| `elfeed-tube` | [elfeed-config.el:131](elfeed-config.el#L131) | Explicit straight recipe/declaration; lazy Elfeed config |
| `elfeed-tube-mpv` | [elfeed-config.el:145](elfeed-config.el#L145) | Explicit straight recipe/declaration; lazy Elfeed config |
| `ellama` | [main.el:1882](main.el#L1882) | Straight by default |
| `embark` | [main.el:1565](main.el#L1565) | Straight by default |
| `embark-consult` | [main.el:1578](main.el#L1578) | Straight by default |
| `emms` | [main.el:941](main.el#L941) | Straight by default |
| `emojify` | [main.el:1677](main.el#L1677) | Straight by default |
| `eshell-syntax-highlighting` | [main.el:1623](main.el#L1623) | Explicit straight recipe/declaration |
| `eshell-toggle` | [main.el:1632](main.el#L1632) | Straight by default |
| `eshell-z` | [main.el:1823](main.el#L1823) | Straight by default |
| `esup` | [main.el:47](main.el#L47) | Straight by default |
| `evil` | [main.el:547](main.el#L547) | Straight by default |
| `evil-collection` | [main.el:567](main.el#L567) | Straight by default |
| `evil-leader` | [main.el:461](main.el#L461) | Straight by default |
| `evil-org` | [main.el:1043](main.el#L1043) | Straight by default |
| `evil-snipe` | [main.el:558](main.el#L558) | Straight by default |
| `evil-surround` | [main.el:590](main.el#L590) | Straight by default |
| `evil-textobj-tree-sitter` | [main.el:599](main.el#L599) | Straight by default |
| `evil-tutor` | [main.el:597](main.el#L597) | Straight by default |
| `fireplace` | [main.el:1778](main.el#L1778) | Straight by default |
| `flycheck` | [main.el:701](main.el#L701) | Straight by default |
| `flycheck-eglot` | [main.el:1763](main.el#L1763) | Straight by default |
| `flycheck-hl-todo` | [main.el:1341](main.el#L1341) | Straight by default |
| `forge` | [main.el:833](main.el#L833) | Explicit straight recipe/declaration |
| `forge-cursor` | [main.el:94](main.el#L94) | Direct straight call |
| `format-all` | [main.el:907](main.el#L907) | Straight by default |
| `gdscript-mode` | [main.el:1866](main.el#L1866) | Explicit straight recipe/declaration |
| `git-link` | [main.el:837](main.el#L837) | Straight by default |
| `golden-ratio` | [main.el:1296](main.el#L1296) | Straight by default |
| `gptel` | [main.el:1834](main.el#L1834) | Explicit straight recipe/declaration |
| `grafana-alloy-mode` | [main.el:1737](main.el#L1737) | Explicit straight recipe/declaration |
| `harpoon` | [main.el:1370](main.el#L1370) | Straight by default |
| `haskell-mode` | [main.el:716](main.el#L716) | Straight by default |
| `hl-todo` | [main.el:1338](main.el#L1338) | Explicit straight recipe/declaration |
| `hydra` | [main.el:117](main.el#L117) | Straight by default |
| `imenu-list` | [main.el:1872](main.el#L1872) | Straight by default |
| `indent-bars` | [main.el:624](main.el#L624) | Straight by default |
| `jtsx` | [main.el:1695](main.el#L1695) | Straight by default |
| `koopa-mode` | [main.el:972](main.el#L972) | Straight by default |
| `leetcode` | [main.el:1907](main.el#L1907) | Explicit straight recipe/declaration |
| `link-hint` | [main.el:1680](main.el#L1680) | Straight by default |
| `lsp-haskell` | [main.el:775](main.el#L775) | Straight by default |
| `lsp-mode` | [main.el:754](main.el#L754) | Straight by default |
| `lsp-treemacs` | [main.el:799](main.el#L799) | Straight by default |
| `lsp-ui` | [main.el:785](main.el#L785) | Straight by default |
| `magit` | [main.el:824](main.el#L824) | Explicit straight recipe/declaration |
| `marginalia` | [main.el:1559](main.el#L1559) | Straight by default |
| `markdown-mode` | [main.el:860](main.el#L860) | Straight by default |
| `markdown-table-wrap` | [main.el:1452](main.el#L1452) | Explicit straight recipe/declaration |
| `md-ts-mode` | [main.el:1447](main.el#L1447) | Explicit straight recipe/declaration |
| `md4rd` | [main.el:977](main.el#L977) | Straight by default |
| `mpv` | [elfeed-config.el:128](elfeed-config.el#L128) | Explicit straight recipe/declaration; lazy Elfeed config |
| `nix-mode` | [main.el:905](main.el#L905) | Explicit straight recipe/declaration |
| `no-littering` | [main.el:49](main.el#L49) | Straight by default |
| `nyan-mode` | [main.el:1209](main.el#L1209) | Straight by default |
| `ob-powershell` | [main.el:1073](main.el#L1073) | Straight by default |
| `orderless` | [main.el:1521](main.el#L1521) | Straight by default |
| `org` | [main.el:89](main.el#L89) | Direct straight call |
| `org-ai` | [main.el:1408](main.el#L1408) | Explicit straight recipe/declaration |
| `org-alert` | [main.el:1062](main.el#L1062) | Straight by default |
| `org-drill` | [main.el:1177](main.el#L1177) | Straight by default |
| `org-make-toc` | [main.el:1179](main.el#L1179) | Straight by default |
| `org-present` | [main.el:1092](main.el#L1092) | Straight by default |
| `org-roam` | [main.el:1154](main.el#L1154) | Explicit straight recipe/declaration |
| `org-timeblock` | [main.el:1093](main.el#L1093) | Straight by default |
| `ormolu` | [main.el:745](main.el#L745) | Straight by default |
| `outline-yaml` | [main.el:917](main.el#L917) | Explicit straight recipe/declaration |
| `package-lint` | [main.el:1888](main.el#L1888) | Straight by default |
| `pacmacs` | [main.el:1667](main.el#L1667) | Straight by default |
| `page-break-lines` | [main.el:1281](main.el#L1281) | Straight by default |
| `paredit` | [main.el:1878](main.el#L1878) | Straight by default |
| `pcsv` | [main.el:1372](main.el#L1372) | Straight by default |
| `perspective` | [main.el:1793](main.el#L1793) | Straight by default |
| `pi-coding-agent` | [main.el:1455](main.el#L1455) | Explicit straight recipe/declaration |
| `posframe` | [main.el:167](main.el#L167) | Straight by default |
| `powershell` | [main.el:971](main.el#L971) | Straight by default |
| `projectile` | [main.el:1220](main.el#L1220) | Straight by default |
| `rainbow-csv` | [main.el:1353](main.el#L1353) | Explicit straight recipe/declaration |
| `rainbow-delimiters` | [main.el:1242](main.el#L1242) | Straight by default |
| `ranger` | [main.el:1193](main.el#L1193) | Straight by default |
| `read-aloud` | [main.el:1270](main.el#L1270) | Straight by default |
| `ready-player` | [main.el:654](main.el#L654) | Straight by default |
| `reformatter` | [main.el:745](main.el#L745) | Straight by default |
| `restart-emacs` | [main.el:1245](main.el#L1245) | Straight by default |
| `rg` | [main.el:961](main.el#L961) | Explicit straight recipe/declaration |
| `ruff-format` | [main.el:1727](main.el#L1727) | Straight by default |
| `selectric-mode` | [main.el:1368](main.el#L1368) | Straight by default |
| `smart-mode-line` | [main.el:1032](main.el#L1032) | Straight by default |
| `smart-mode-line-powerline-theme` | [main.el:1030](main.el#L1030) | Straight by default |
| `smartparens` | [main.el:1807](main.el#L1807) | Straight by default |
| `spaceline` | [main.el:1019](main.el#L1019) | Straight by default |
| `spacious-padding` | [main.el:1307](main.el#L1307) | Straight by default |
| `sudoku` | [main.el:1657](main.el#L1657) | Straight by default |
| `svg-tag-mode` | [main.el:1326](main.el#L1326) | Straight by default |
| `transient` | [main.el:90](main.el#L90) | Direct straight call |
| `treemacs` | [main.el:708](main.el#L708) | Straight by default |
| `treemacs-evil` | [main.el:714](main.el#L714) | Straight by default |
| `treemacs-projectile` | [main.el:713](main.el#L713) | Straight by default |
| `treesit-auto` | [main.el:1682](main.el#L1682) | Straight by default |
| `treesit-fold` | [main.el:1690](main.el#L1690) | Explicit straight recipe/declaration |
| `ts-query-highlight` | [main.el:1825](main.el#L1825) | Explicit straight recipe/declaration |
| `typst-ts-mode` | [main.el:1712](main.el#L1712) | Explicit straight recipe/declaration |
| `undo-tree` | [main.el:694](main.el#L694) | Straight by default |
| `use-package` | [main.el:41](main.el#L41) | Direct straight call |
| `vertico` | [main.el:1532](main.el#L1532) | Straight by default |
| `wgrep` | [main.el:969](main.el#L969) | Straight by default |
| `yaml-mode` | [main.el:915](main.el#L915) | Straight by default |
| `yasnippet` | [main.el:801](main.el#L801) | Straight by default |
| `yasnippet-snippets` | [main.el:805](main.el#L805) | Straight by default |
| `zone-matrix` | [main.el:2089](main.el#L2089) | Explicit straight recipe/declaration |
| `zone-nyan` | [main.el:1218](main.el#L1218) | Straight by default |
| `zoxide` | [main.el:1814](main.el#L1814) | Straight by default |

## Local checkout preferred, straight fallback

| Package | Preferred checkout |
| --- | --- |
| `powershell-ts-mode` | `~/Desktop/prog/powershell-ts-mode/` |
| `treesit-jump` | `~/Desktop/prog/treesit-jump/` |
| `ufo-catcher` | `~/Desktop/prog/ufo-catcher/` |

The local branches explicitly set :straight nil.

## Built-in libraries

`editorconfig`, `erc`, `re-builder`, `savehist`, `smerge-mode`, `which-key`. Their declarations set :straight nil. Current Emacs supplies them; which-key and editorconfig require Emacs 30 or newer. Eglot and other directly required built-in libraries have no separate installer declaration.

## Nix packages

[common-home.nix](../home-manager/new/common-home.nix#L118) supplies vterm, w3m, and jinx. On Linux they are registered as pseudo built-ins to keep straight from installing additional copies through dependency resolution. The active vterm/w3m declarations set :straight nil; the jinx declaration is commented out.

## Existing/manual installations

| Package | Source |
| --- | --- |
| `vertico-directory` | Extension included in vertico; :straight nil prevents a separate install. |
| `eaf` | Manual checkout at ~/.emacs.d/site-lisp/emacs-application-framework, loaded only when present; :straight nil. |

## Local modules and inactive configuration

- eshell-extensions.el is a local autoloaded module; its eshell-toggle requirement is managed by straight.
- Loading twitchy.el and youtube.el is commented out. Their requirements include dash, helm, s, request, and all-the-icons, but they declare no additional installers.
- powershell-ts-shell-mode.el is a local module with no active load from main.el.
- auto-package-update was removed. Updates and version freezing are deliberate operations.

See [PACKAGE-MANAGEMENT.md](PACKAGE-MANAGEMENT.md) for startup, update, and restore instructions.
