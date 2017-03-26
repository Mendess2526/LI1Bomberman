#!/bin/bash
ghc Main.hs -o Bomberman 2> logfile
rm *.o
rm *.hi