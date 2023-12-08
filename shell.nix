with (import <nixpkgs> { });
haskell.lib.buildStackProject {
  name = "HugoIncr";

  shellHook =
  ''
    export PS1="\[\033[01;32m\][\u@\h\[\033[01;37m\] |HugoIncr| \W\[\033[01;32m\]]\$\[\033[00m\] "
  '';
    
  # Missing for static release build:
  # -lc
  # -lm
  # -lpthread
  # -lgmp  <--- still missing (even though gmp.static below)
  # -lrt
  # -ldl
  # -lffi  <--- still missing
  #
  # Use a Docker container to build the release build?
  buildInputs = [
    zlib
    hpack
    hlint
    ormolu
    glibc.static
    zlib.static
    gmp.static
  ];
}

