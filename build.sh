#!/bin/bash

rm *.o
rm test

g++ -c -m64 -x c++ -fpic -I/usr/include/singular -I/usr/include/x86_64-linux-gnu/singular ./cpp_bits/interface.cpp -o interface.o 

#ghc --make test.hs interface.o -o test -lfactory -lntl -lsingular_resources -lomalloc -lstdc++
#ghc --make test.hs interface.o -L/usr/lib/x86_64-linux-gnu -o test -lsingular-factory -lntl -lsingular-omalloc -lsingular-resources -lstdc++

ghc --make -O main.hs interface.o -L/usr/lib/x86_64-linux-gnu -o main -lsingular-factory -lntl -lsingular-omalloc -lsingular-resources -lstdc++


