#!/bin/sh
set -e

FD=${FD:-fdfind}

case $1 in

"fmt")
  cargo fmt
  $FD --extension hs    | xargs ormolu       -i
  $FD --extension cabal | xargs cabal-fmt    -i
  $FD --extension proto | xargs clang-format -i
;;

"build")
  cargo build
  cabal build all
;;

"lint")
  cargo clippy
  cargo hakari verify
  hlint .
;;

"")
  exit 0
;;

*)
  echo "Unknown command $1"
  exit 1
;;

esac
