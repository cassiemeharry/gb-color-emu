let
  sources = import ./nix/sources.nix;
  rust = import ./nix/rust.nix {
    inherit sources;
    targets = [ "x86_64-unknown-linux-gnu" "wasm32-unknown-unknown" ];
  };
  pkgs = import sources.nixpkgs {};
  mgbdis-src = pkgs.fetchFromGitHub {
    owner = "mattcurrie";
    repo = "mgbdis";
    rev = "v1.4";
    sha256 = "1mjsvdwvlnzxsg8b4033xpassry567gzff27c1lbmn4yrajrqaqg";
  };
  mgbdis = pkgs.python3.pkgs.buildPythonApplication {
    pname = "mgbdis";
    version = "1.4";
    format = "other";
    buildPhase = "";
    installPhase = ''
      mkdir $out
      mkdir $out/bin
      echo >> $out/bin/mgbdis '#!${pkgs.bash}/bin/bash'
      echo >> $out/bin/mgbdis 'exec ${pkgs.python3}/bin/python3 ${mgbdis-src}/mgbdis.py "$@"'
      chmod +x $out/bin/mgbdis
    '';
    src = mgbdis-src;
  };
in
pkgs.mkShell {
  buildInputs = [
    mgbdis
    pkgs.dwarfdump
    pkgs.file
    pkgs.gdb
    pkgs.pkg-config
    pkgs.python3
    pkgs.unzip
    pkgs.valgrind
    pkgs.libvpx
    rust
  ];
  nativeBuildInputs = [
    pkgs.llvmPackages.clang
  ];
  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang}/lib";
  GDB_DEV_INCLUDE_PATH = "${pkgs.gdb}/include";
}
