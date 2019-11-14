
Haskell bindings to the Singular-Factory multivariate polynomial factorization engine.

[Singular](https://www.singular.uni-kl.de/) is a computer algebra system developed at 
the University of Kaiserslautern, and [Factory](https://www.singular.uni-kl.de/dox/html/factory_page.html)
is the polynomial factorization engine of Singular.

It handles multivariate polynomials over the integers, rationals, finite fields, and
algebraic and trancendental extensions of these. 


Installation instructions
=========================

Currently the library works on macOS (tested with Homebrew), and on Linux (tested with Debian).


macOS 
------

### With Homebrew:

1. Install Homebrew from https://brew.sh (if not already installed).
2. Install pkg-config using homebrew (if not already installed):
```
$ brew install pkg-config
```
3. Install singular:  
```    
$ brew install singular
```
4. Install this library:
```    
$ cabal update
$ cabal install --lib singular-factory
```    

Linux
-----

### On Debian:

1. Install pkg-config (if not already installed): 
```
$ sudo apt install pkg-config
```
2. Install libsingular:
```   
$ sudo apt install libsingular4-dev
```
3. Install this library:
```
$ cabal update
$ cabal install --lib singular-factory
```

### Other Linux distros:

It should be similar to the above: Install libsingular using your distro's package
manager, and hope that pkg-config will give the necessary information to Cabal.


Windows
-------

Unfortunately I couldn't make it work on Windows so far.

The primary problem is linking the cygwin-based Singular with the mingw-based GHC.
If you have advice on how to achieve this, please contact me.

