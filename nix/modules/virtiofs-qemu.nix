{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.virtualisation;

  # Create wrapper script that starts virtiofsd and modifies the VM script
  mkVirtiofsRunner = pkgs.writeShellApplication {
    name = "run-nixos-vm-virtiofs";
    runtimeInputs = [ pkgs.virtiofsd ];
    text = ''
          set -eu -o pipefail
          
          # Create temp directory if needed
          TMPDIR="$(mktemp -d nix-vm.XXXXXXXXXX --tmpdir)"
          export TMPDIR
          
          # Track virtiofsd PIDs
          VIRTIOFSD_PIDS=""
          
          # Cleanup function
          cleanup() {
            echo "Cleaning up virtiofsd daemons..."
            for pid in $VIRTIOFSD_PIDS; do
              kill "$pid" 2>/dev/null || true
            done
            rm -f "$TMPDIR"/virtiofsd-*.sock
          }
          
          trap cleanup EXIT INT TERM
          
          # Start virtiofsd for each shared directory
          ${concatStringsSep "\n" (
            mapAttrsToList (name: dir: ''
              echo "Starting virtiofsd for ${name}..."
              SOCKET_PATH="$TMPDIR/virtiofsd-${name}.sock"
              virtiofsd \
                --socket-path="$SOCKET_PATH" \
                --shared-dir="${dir.source}" \
                --cache=auto \
                --sandbox=none \
                --no-announce-submounts \
                --xattr \
                --security-label \
                --posix-acl \
                --log-level=info &
              VIRTIOFSD_PIDS="$VIRTIOFSD_PIDS $!"
            '') cfg.sharedDirectoriesVIO
          )}
          
         # Wait for socket to be created
          for i in {1..10}; do
            if [ -S "$SOCKET_PATH" ]; then
      	echo "Socket $SOCKET_PATH created $i"
      	break
            fi
            sleep 0.5
          done
          
          if [ ! -S "$SOCKET_PATH" ]; then
            echo "ERROR: Socket $SOCKET_PATH was not created!"
            exit 1
          fi
          # Build QEMU options for virtiofs
          VIRTIOFS_OPTS=""
          ${concatStringsSep "\n" (
            mapAttrsToList (name: _dir: ''
              VIRTIOFS_OPTS="$VIRTIOFS_OPTS -chardev socket,id=char-${name},path=$TMPDIR/virtiofsd-${name}.sock"
              VIRTIOFS_OPTS="$VIRTIOFS_OPTS -device vhost-user-fs-pci,chardev=char-${name},tag=${name}"
            '') cfg.sharedDirectoriesVIO
          )}
          
          # CRITICAL: Override the VM script's memory configuration for virtiofs
          export QEMU_OPTS="-m ${toString cfg.memorySize},slots=32,maxmem=64G -object memory-backend-file,id=mem,size=${toString cfg.memorySize}M,mem-path=/dev/shm,share=on -machine memory-backend=mem $VIRTIOFS_OPTS ''${QEMU_OPTS:-}"
          
          exec ${config.system.build.vm}/bin/run-nixos-vm "$@"
    '';
  };

in
{
  options.virtualisation.sharedDirectoriesVIO = mkOption {
    type = types.attrsOf (
      types.submodule {
        options = {
          source = mkOption {
            type = types.str;
            description = "Host path to share";
          };
          target = mkOption {
            type = types.str;
            description = "Guest mount point";
          };
        };
      }
    );
    default = { };
    description = "Directories to share with the VM using virtiofs";
  };
  config = mkIf (cfg.sharedDirectoriesVIO != { }) {
    # Ensure virtiofs kernel module is available
    boot.initrd.availableKernelModules = [ "virtiofs" ];
    boot.kernelModules = [ "virtiofs" ];
    # Add filesystem entries for virtiofs mounts
    fileSystems = mapAttrs' (name: dir: {
      name = dir.target;
      value = {
        device = name;
        fsType = "virtiofs";
        options = [ "defaults" ];
        neededForBoot = false;
      };
    }) cfg.sharedDirectoriesVIO;

    # Force mount at boot
    systemd.services = mapAttrs' (name: dir: {
      name = "mount-virtiofs-${name}";
      value = {
        description = "Mount virtiofs ${name}";
        after = [ "multi-user.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.util-linux}/bin/mount -t virtiofs ${name} ${dir.target}";
          RemainAfterExit = true;
        };
      };
    }) cfg.sharedDirectoriesVIO;

    # Create mount points
    systemd.tmpfiles.rules = mapAttrsToList (
      _name: dir: "d ${dir.target} 0755 root root -"
    ) cfg.sharedDirectoriesVIO;

    # Create the wrapped VM runner
    system.build.vmWithVirtioFS = mkVirtiofsRunner;
  };
}
