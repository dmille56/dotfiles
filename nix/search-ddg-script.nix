with import <nixpkgs> { };

pkgs.writeShellScriptBin "search-ddg-script" ''
  #!${pkgs.bash}

  searchDdgTerms=$(${pkgs.rofi}/bin/rofi -dmenu -p "Search DuckDuckGo")
  if [[ -n $searchDdgTerms ]]; then
      firefox "https://duckduckgo.com/?q=$searchDdgTerms"
  fi
''
