{
  config,
  pkgs,
  lib,
  ...
}:
with lib;

let
  cfg = config.services.selenium-server;
  cfgFile = pkgs.writeText "config.json" (builtins.toJSON (cfg.settings));
in
{
  options = {
    services.selenium-server = {
      enable = mkEnableOption "Selenium server for system tests";
      package = mkOption {
        type = types.package;
        default = pkgs.selenium-server-standalone;
      };
      port = mkOption {
        type = types.int;
        default = 4444;
      };
      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
      };
      openFirewall = mkOption {
        type = types.bool;
        default = false;
      };
      packages = mkOption {
        type = types.listOf types.package;
        default = with pkgs; [
          chromedriver
          chromium
          firefox
        ];
      };
      settings = mkOption {
        default = {
          inherit (cfg) host port;
          capabilities = [
            {
              browserName = "chrome";
              maxInstances = 10; # FIXME: make configurable
              seleniumProtocol = "WebDriver";
            }
          ];
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    users.groups.selenium-server = { };
    users.users.selenium-server = {
      description = "Selenium server owner";
      group = "selenium-server";
      home = "/var/lib/selenium-server";
      createHome = true;
      isSystemUser = true;
    };
    systemd.services.selenium-server = {
      description = "Selenium headless server";
      path = cfg.packages;
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
      ];
      postStart = ''
        while sleep 0.5; do
          if (: </dev/tcp/${cfg.settings.host}/${toString cfg.settings.port}) 2>/dev/null; then
            exit 0
          fi
        done
      '';
      serviceConfig = rec {
        Type = "exec";
        Restart = "always";
        ExecStart = "${cfg.package}/bin/selenium-server -config ${cfgFile}";
        User = "selenium-server";
        Group = "selenium-server";
        PrivateTmp = "yes";
        ProtectDevices = "yes";
        NoNewPrivileges = "yes";
        # TODO: Restrict more if possible
      };

    };
    networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.settings.port;
  };
}
