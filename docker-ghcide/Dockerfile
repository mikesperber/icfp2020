FROM haskell:8.8
RUN git clone --branch v0.2.0 https://github.com/digital-asset/ghcide.git
RUN cd ghcide && cabal update && cabal configure && cabal install
