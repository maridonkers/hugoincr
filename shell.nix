with (import <nixpkgs> { });
haskell.lib.buildStackProject {
  name = "HugoIncr";

  shellHook =
  ''
    export PS1="\[\033[01;32m\][\u@\h\[\033[01;37m\] |HugoIncr| \W\[\033[01;32m\]]\$\[\033[00m\] "
  '';
    
  buildInputs = [ zlib hpack hlint ormolu ];
}
