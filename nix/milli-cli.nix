{ lib
, fetchFromGitHub
, buildNpmPackage
, nodejs
, vips
, pkg-config
}:

let
  rev = "8179a5a765693b3086c560ea83507b016d85179b";

  # Note: hashes are intentionally placeholders on first run.
  src = fetchFromGitHub {
    owner = "Amansingh-afk";
    repo = "milli";
    inherit rev;

    # Replace after the first nix build error.
    hash = "sha256-tN3OXKbtQL+Ql6VOjJJ/jUUbk7jbe1kxVPz8btQQGR4=";
  };
in

buildNpmPackage {
  pname = "milli";
  version = rev;

  inherit src;
  npmBuildScript = "build";

  # Replace after the first nix build error.
  # (Must be a valid hash string; leaving it empty lets nix fail with the
  # expected value.)
  npmDepsHash = "sha256-GXYts7eHPwXozioDoKKH9xihZ0eWho2VaTx6iFDxk+k=";

  # sharp native deps
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ vips ];

  # sharp needs vips from nixpkgs and some projects need writable cache dirs.
  makeCacheWritable = true;
  env.SHARP_FORCE_GLOBAL_LIBVIPS = "1";

  # No tests for the upstream CLI (and they would require extra tooling).
  doCheck = false;

  # The upstream project is a pure CLI. Ensure the output path contains the bin.
  # buildNpmPackage will install npm bins into $out/bin automatically.
  meta = with lib; {
    description = "milli CLI (rendering engine)";
    homepage = "https://github.com/Amansingh-afk/milli";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
