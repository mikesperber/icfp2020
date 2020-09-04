#!/bin/sh
dist-newstyle/build/x86_64-linux/ghc-*/hearts-0.1.0.0/x/server/build/server/server -s Table -p 8080 &
dist-newstyle/build/x86_64-linux/ghc-*/hearts-0.1.0.0/x/server/build/server/server -s Player -p 8001 -n Mike -i 1 &
dist-newstyle/build/x86_64-linux/ghc-*/hearts-0.1.0.0/x/server/build/server/server -s Player -p 8002 -n Peter -i 2 &
dist-newstyle/build/x86_64-linux/ghc-*/hearts-0.1.0.0/x/server/build/server/server -s Player -p 8003 -n Nicole -i 3 &
dist-newstyle/build/x86_64-linux/ghc-*/hearts-0.1.0.0/x/server/build/server/server -s Player -p 8004 -n Annette -i 4 &
