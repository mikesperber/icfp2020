# Starting the Game

## Console

The `Sync` module has two functions `gameAlong` and
`gameInteractive`. (Very rudimentary.)

## UI version

- build the Haskell executable, which Cabal will put into a file like
  `dist-newstyle/build/x86_64-osx/ghc-8.8.3/hearts-0.1.0.0/x/server/build/server/server`

- start the table server:

```
.../server -s Table -p 8080 &
.../server -s Player -p 8001 -n Mike -i 1 &
.../server -s Player -p 8002 -n Peter -i 2 &
.../server -s Player -p 8003 -n Nicole -i 3 &
.../server -s Player -p 8004 -n Annette -i 4 &
```

- install [Elm](https://elm-lang.org/)

- cd to `hearts-frontend`, do:

```
elm reactor
```

- go to the URL there, typically
  [`http://localhost:8000`](http://localhost:8000)
  
- click on `examples`, then `09-hearts.elm`
