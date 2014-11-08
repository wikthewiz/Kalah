Kalah
=====

This is a haskell implementation of Kalah, also called Kalaha or Mancala,  which is a game in the mancala family. 

Rules
  1. At the beginning of the game, three seeds are placed in each house. 
  2. Each player controls the six houses and their seeds on the player's side of the board. The player's score is the number of seeds in the store to their right.
  3. Players take turns sowing their seeds. On a turn, the player removes all seeds from one of the houses under their control. Moving counter-clockwise, the player drops one seed in each house in turn, including the player's own store but not their opponent's.
  4. If the last sown seed lands in the player's store, the player gets an additional move. There is no limit on the number of moves a player can make in their turn
  5. If the last sown seed lands in an empty house owned by the player, and the opposite house contains seeds, both the last seed and the opposite seeds are captured and placed into the player's store.
  6. When one player no longer has any seeds in any of their houses, the game ends. The other player moves all remaining seeds to their store, and the player with the most seeds in their store wins.

It is possible for the game to end in a draw, with 18 seeds each. 
