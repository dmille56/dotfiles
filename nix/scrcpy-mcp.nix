# Android automation from an AI client
#
# Home Manager installs this package through common-home.nix. It includes
# Node.js, ADB, scrcpy, and FFmpeg, including ffplay for the live viewer.
# The source commit and npm dependencies are pinned below.
#
# After applying Home Manager, enable USB debugging on the phone, connect it,
# accept the computer's RSA key, and check `adb devices`. Configure your AI
# client to launch scrcpy-mcp as a local MCP server over stdio. For clients
# using the mcpServers JSON format:
#
# {
#   "mcpServers": {
#     "android": {
#       "command": "/home/dono/.nix-profile/bin/scrcpy-mcp"
#     }
#   }
# }
#
# To watch automation, ask the AI to call start_session, then
# start_video_stream. The latter opens a live viewer using ffplay. Launch the
# AI client in your graphical session so it inherits the display environment.
# Alternatively, run `scrcpy` in a separate terminal to mirror the phone.
#
# Upstream: https://github.com/JuanCF/scrcpy-mcp
{ lib
, buildNpmPackage
, fetchFromGitHub
, nodejs_24
, makeWrapper
, android-tools
, scrcpy
, ffmpeg
, which
}:

buildNpmPackage {
  pname = "scrcpy-mcp";
  version = "0.5.0-unstable-2026-09-22";

  src = fetchFromGitHub {
    owner = "JuanCF";
    repo = "scrcpy-mcp";
    rev = "5f93d15617f9eb047cbe8a28d244e5cc91151551";
    hash = "sha256-oZdsEgbNMXvngDnK2ePbjvW9nw7h5SPjoN0t5lrX7EA=";
  };

  nodejs = nodejs_24;
  npmDepsHash = "sha256-RuM08JTxwGFX+wkjt1wratGtQTtnuL0raARQOiNE0mw=";
  npmBuildScript = "build";

  # Avoid ffmpeg-static's binary download; use Nix's FFmpeg at runtime.
  # esbuild's platform binary is already supplied by the npm lockfile.
  npmRebuildFlags = [ "--ignore-scripts" ];
  nativeBuildInputs = [ makeWrapper ];

  postFixup = ''
    wrapProgram "$out/bin/scrcpy-mcp" \
      --prefix PATH : ${lib.makeBinPath [ android-tools scrcpy ffmpeg which ]} \
      --set-default ADB_PATH ${android-tools}/bin/adb \
      --set-default SCRCPY_SERVER_PATH ${scrcpy}/share/scrcpy/scrcpy-server \
      --set-default SCRCPY_SERVER_VERSION ${scrcpy.version} \
      --set-default FFMPEG_PATH ${ffmpeg}/bin/ffmpeg \
      --set-default FFPLAY_PATH ${ffmpeg}/bin/ffplay
  '';

  meta = {
    description = "MCP server for Android control and live viewing via ADB and scrcpy";
    homepage = "https://github.com/JuanCF/scrcpy-mcp";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "scrcpy-mcp";
  };
}
