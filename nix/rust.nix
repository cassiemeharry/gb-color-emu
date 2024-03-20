{ sources ? import ./sources.nix
, targets ? []
}:

let
  pkgs = import sources.nixpkgs {
    overlays = [ (import sources.rust-overlay) ];
  };
  baseChannel = pkgs.rust-bin.stable."1.58.1".default;
  channel = baseChannel.override {
    extensions = ["rust-src" "rustfmt"];
    targets = targets;
  };
in
builtins.trace (builtins.attrNames channel) channel
