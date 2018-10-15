# Minesweeper
Play minesweeper in Emacs

[![Build Status](https://api.travis-ci.org/calancha/Minesweeper.svg?branch=master)](https://travis-ci.org/calancha/Minesweeper)

This is an elisp implementation of the classical minesweeper game.
The target is localize all hidden mines (bombs) in a rectangular board
without detonating them.  You reveal the content of a cell with the
command **mines-dig**.

1. Cells with a bomb contain the character **x**; if you call **mines-dig**
   in these cells then you lost the game.

2. Cells without bomb at distance 1 from any cell with a mine
   contain a number: the number of bombs at distance 1 from this cell.
   If you reveal the content of this cell, then this number is shown.

3. Cells without a bomb at distance > 1 from any bomb contain **@**.
   If you reveal the content of this cell, then **@** is shown and
   all adjacent cells are recursively revealed.


If you think that an uncovered cell has a mine, you might flag it
with **mines-flag-cell**: the character **!** is shown in that cell.
If you call this command again in the same cell then unflags it.  This is
useful to visualize your progress in the game.

The game is completed once all mine-free cells are revealed, that is,
when the only uncovered cells equals the number of hidden mines.

### Screenshots

minesweeper screenshot:
![ScreenShot](/screenshots/screenshot-minesweeper.png)

Game over screenshot:
![ScreenShot](/screenshots/screenshot-minesweeper-gameover.png)
