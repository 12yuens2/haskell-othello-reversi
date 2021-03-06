# Othello Game
This is an implementation of [Othello](https://en.wikipedia.org/wiki/Reversi) written in Haskell. The game currently supports an AI player and the ability to play over the network against another player. 

![alt text](https://github.com/dR3am5t0rM/haskell-othello-reversi/blob/master/report/screenshots/validMove.png)

## Running the game
In order to compile this, you will need to be working in Linux. You will first 
need to install the gloss library, which is avaiable from hackage, be sure to 
install gloss-1.8.1.2 as that is the version of gloss this game is built on. 
A full list of dependencies can be found in the file Othello.cabal. Install any 
missing libraries using the command cabal install <library>. Then, to run the 
game: 
- `cabal configure` in the `code/` directory to configure
- `cabal build` in the `code/` directory to build an executeable, which can be found in `dist/build/othello/`
- Run the executable `othello` in `dist/build/othello` to play the game



## Command line arguments
Usage: `othello [OPTION...]`

Note: for each option if it is applied more than once  with different settings such as having `-aw -rw` or `-s 4 -s 10` then the last one will be applied

- `server`  
Sets this game as the server, the game will only launch once a connection from a client has been made. The server can set different game options and will send it's world to the client at the start of the game. The server will ignore any ai arguments.

- `client [HOSTNAME]`  
Sets this game as the client, a hostname must be provided for the client to connect to, any other options are ignored.

- `s [SIZE]`  
Sets the width of the game board, the size must be even and be between 4 and 16 starts the game in 'reversi' mode, where the first 2 pieces from both players can be placed anywhere.
    
- `ab`  
Sets the black player as the ai.

- `aw`  
Sets the white player as the ai.

- `rb`  
Sets the black player to be a random ai player.

- `rw`  
Sets the white player to be a random ai player.

- `h`  
Starts the game with hints on.




## In game controls
Use the mouse to click on positions to make moves.

There are various controls to change options in game:
- `h` - toggles hints on and off
- `u` - undo a move, disabled for network play
- `p` - pause the game, disabled for network play
- `s` - save the game to `save.othello`, disabled for network play
- `l` - load the game from `save.othello`, disabled for network play

Pressing the 'Esc' key will exit the game. 




## Documentation
The documentation is included in HTML format in the `documentation/` directory. 
It is generated from Haddock version 2.13.2, (c) Simon Marlow 2006.


