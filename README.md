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

- in the `hearts` directory, run `./docker-shell.sh` to start a
  suitable shell in a Docker container

- inside the Docker container, do `cabal build` to build the server

- inside the Docker container, do `./run-server-inside-docker.sh` to
  start the server
  
- *outside* the Docker container:

- install [Elm](https://elm-lang.org/)

- cd to `hearts-frontend`, do:

```
elm reactor
```

- go to the URL there, typically
  [`http://localhost:8000`](http://localhost:8000)
  
- click on `examples`, then `HeartsFrontend.elm`
