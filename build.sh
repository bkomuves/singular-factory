#!/bin/bash

rm *.o
rm test

CFLAGS=`pkg-config --cflags-only-I factory`
LIBS=`pkg-config --libs factory`

g++ -c -m64 -x c++ -fpic $CFLAGS ./cpp_bits/interface.cpp -o interface.o 
ghc --make -O -package hgmp main.hs interface.o -o main ${LIBS} -lstdc++
