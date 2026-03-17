{ pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    cowsay
  ];

  home.sessionVariables = {
    MY_MACHINE_ID = lib.mkDefault "none";
    HELLO_WORLD_VAR = "hello world";
  };
}
