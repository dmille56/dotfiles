with import <nixpkgs> {};

callPackage (
    { dotnet-sdk_7,
      dotnet-runtime_7
    }:

    buildDotnetModule {
      pname = "ChatGPT.CLI";
      version = "1.0.0"; # todo: fix version
      src = fetchFromGitHub {
        owner = "wieslawsoltes";
        repo = "ChatGPT";
        rev = "2a13d32bb7255f761170091075d48a3e6010aa6f";
        sha256 = "1jvzg8d7a4s6sy2brph8zrczl6y8fbh1gd9348w75hg76qjv8dld";
      };
      dotnet-sdk = dotnet-sdk_7;
      dotnet-runtime = dotnet-runtime_7;
      nugetDeps = ./ChatGPT-CLI-deps.nix;
      projectFile = [
          "./src/ChatGPT.CLI/ChatGPT.CLI.csproj"
          # "./src/ChatGPT.UI.Desktop/ChatGPT.UI.Desktop.csproj" 
          # "./src/ChatGPT.Core/ChatGPT.Core.csproj" 
          # "./src/ChatGPT.UI/ChatGPT.UI.csproj" 
      ];
    }
) { }
