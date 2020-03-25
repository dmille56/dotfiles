with import <nixpkgs> {};
with pkgs.python37Packages;

buildPythonPackage rec {
  name = "twitchy";
  version = "3.4.0";

  src = builtins.fetchGit {
    url = "git://github.com/BasioMeusPuga/twitchy.git";
    rev = "92e5176d1b66cc5f20c10ce298c849ebc4bb8c97";
  };

  propagatedBuildInputs = [ streamlink requests ];

  meta = {
    description = ''
      Command line streamlink wrapper for twitch.tv
    '';
  };
}
