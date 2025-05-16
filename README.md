# Polarity Placement Puzzle (Haskell)
This project implements a Haskell program to solve a constraint-based puzzle known as the Polarity Puzzle, which involves placing bar magnets (dominoes) on a two-dimensional grid under strict rules about magnetic polarity and adjacency.

# Puzzle Description
The Polarity Puzzle requires placing domino-shaped bar magnets on a board. Each magnet has a positive (+) and a negative (-) pole. The magnets can be placed either horizontally or vertically, and the goal is to position them such that the following constraints are satisfied:

1. Pole Count Constraints:

  - Numbers on the top and left sides of the board specify how many positive poles must appear in each column or row.
  - Numbers on the bottom and right sides of the board specify how many negative poles must appear in each column or row.

2. Polarity Adjacency Constraint:
  - No two positive poles may be adjacent horizontally or vertically.
  - No two negative poles may be adjacent horizontally or vertically.
  - This mimics the physical behavior of real bar magnets.

The solver can be tested with boards of different dimensions and constraint sets
