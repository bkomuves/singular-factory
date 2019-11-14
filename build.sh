#!/bin/bash

rm *.o
rm test

CFLAGS=`pkg-config --cflags-only-I singular`
LIBS=`pkg-config --libs singular`

g++ -c -m64 -x c++ -fpic $CFLAGS ./cpp_bits/interface.cpp -o interface.o 
ghc --make -O main.hs interface.o -o main ${LIBS} -lstdc++
