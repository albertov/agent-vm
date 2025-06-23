{
  ...
}:
{
  # Configure host nix store as binary cache
  nix.settings.substituters = [
    "http://10.0.2.2:5000"
  ];
  nix.settings.trusted-public-keys = [
    "alberto-valverde-1:A+NbXRfx+Uo0tQNZ8hlip+1zru2P32l7/skPDeaZnxU="
  ];
  services.selenium-server.enable = true;
  services.mcp-proxy = {
    # These would be overrided in a module added by the create admin command
    # which imports this base config
    port = 8000;
    uid = 1000;
    group = "users";
    #shell = self.devShells."${pkgs.system}".default;
  };
}
