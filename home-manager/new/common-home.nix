{ pkgs, lib, ... }:
{
  home.packages = lib.mkDefault (with pkgs; [
    cowsay
  ]);

  home.sessionVariables = lib.mkDefault {
    MY_MACHINE_ID = "none";
    HELLO_WORLD_VAR = "hello world";
  };
}
