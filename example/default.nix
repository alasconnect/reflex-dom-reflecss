{ reflex-platform ? import /home/vagrant/reflex-platform { config.android_sdk.accept_license = true; }
, warp ? false
}:
reflex-platform.project ({ pkgs, ... }: {
  useWarp = warp;
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    reflex-dom-reflecss = ../lib;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
