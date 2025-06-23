{
  pkgs,
  ...
}:
let
  flake = builtins.getFlake "git+file://home/alberto/src/agent-vm";
in
{
  # Configure host nix store as binary cache
  nix.settings.substituters = [
    "http://10.0.2.2:5000"
  ];
  nix.settings.trusted-public-keys = [
    "alberto-valverde-1:A+NbXRfx+Uo0tQNZ8hlip+1zru2P32l7/skPDeaZnxU="
  ];
  services.selenium-server.enable = true;
  agent-vm = {
    workspaceSource = "/home/alberto/src/agent-vm";
    port = 8000;
    uid = 1000;
    group = "users";
    shell = flake.devShells."${pkgs.system}".default;
  };
}
