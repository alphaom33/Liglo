#!/bin/bash
cabal build --enable-profiling --profiling-detail=late exe:Readle
ASDF="$(cabal list-bin exe:Readle)"
eval $ASDF +RTS -pj -RTS
