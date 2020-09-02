# Preparation

If you have a working Haskell/Cabal setup, things might just work.
There's also a working Nix configuration, which at this point requires
a recent `nix-unstable` channel, however.

Otherwise:

- install [Docker](https://www.docker.com/)
- give Docker at least 6GB RAM where relevant
- in the `docker-ghcide` directory, say do `docker build -t ghcide .`
  (this might take a while)
- install [Visual Studio Code](https://code.visualstudio.com/download)
- install VS Code Extension "Remote - Containers" 
- select  "View" -> "Command Palette" (some modifiers + P), type
  "containers",  select"Remote - Containers: Open Folder in Container"
- open the `hearts` directory

VS Code will then construct a container to do its business.  That
might also take while.

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
