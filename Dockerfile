FROM cfv-base:latest

RUN ls /bin/bash

# Copy the project files:
WORKDIR /cfv
COPY ./dist-newstyle/sdist/cfv-0.1.0.0.tar.gz /cfv
RUN tar zxvf cfv-0.1.0.0.tar.gz
WORKDIR /cfv/cfv-0.1.0.0

# Test with GHC 9.8.4:
RUN ghcup set ghc 9.8.4
RUN ghc --version
RUN cabal build --enable-documentation
RUN cabal test

# Test with GHC 9.10.1:
RUN ghcup set ghc 9.10.1
RUN ghc --version
RUN cabal clean
RUN cabal build --enable-documentation
RUN cabal test

 # Test with GHC 9.10.2:
RUN ghcup set ghc 9.10.2
RUN ghc --version
RUN cabal clean
RUN cabal build --enable-documentation
RUN cabal test

# Test with GHC 9.10.3:
RUN ghcup set ghc 9.10.3
RUN ghc --version
RUN cabal clean
RUN cabal build --enable-documentation
RUN cabal test

# Test with GHC 9.12.2:
RUN ghcup set ghc 9.12.2
RUN ghc --version
RUN cabal clean
RUN cabal build --enable-documentation
RUN cabal test

# Test with GHC 9.12.4:
RUN ghcup set ghc 9.12.4
RUN ghc --version
RUN cabal clean
RUN cabal build --enable-documentation
RUN cabal test

# Test with GHC 9.14.1:
RUN ghcup set ghc 9.14.1
RUN ghc --version
RUN cabal clean
RUN cabal build --enable-documentation
RUN cabal test
