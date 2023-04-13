# Expose the executables for local Haskell packages
{ ... }:
{
  perSystem = { config, pkgs, lib, ... }: {
    options = {
      localPackagesStatic = lib.mkOption {
        type = lib.types.attrsOf lib.types.package;
        readOnly = true;
        default = lib.mapAttrs
          (_: p: pkgs.haskell.lib.justStaticExecutables p.package)
          (config.haskellProjects.default.outputs.packages);
        description = ''
          Local Haskell packages, containing only static executables (no libraries)

          Thus stripped of unnecessary transitive dependencies including GHC.
        '';
      };
    };
  };
}

