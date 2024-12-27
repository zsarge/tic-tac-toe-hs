# tic-tac-toe-hs

This is just a basic project to stay fresh in Haskell.

Tic-Tac-Toe was a good project to practice using Monads like `IO` and `Maybe`.

All the code is in [`Main.hs`](./app/Main.hs).

## Example Inputs

All inputs were run with `cabal run`.

<details>

<summary> 1,2,3,4,5,6,7 </summary>

```
 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
1
You selected 1
 X | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player O's turn!
What index do you want to move to?
2
You selected 2
 X | O | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
3
You selected 3
 X | O | X
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player O's turn!
What index do you want to move to?
4
You selected 4
 X | O | X
---+---+---
 O | 5 | 6
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
5
You selected 5
 X | O | X
---+---+---
 O | X | 6
---+---+---
 7 | 8 | 9

Player O's turn!
What index do you want to move to?
6
You selected 6
 X | O | X
---+---+---
 O | X | O
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
7
You selected 7
 X | O | X
---+---+---
 O | X | O
---+---+---
 X | 8 | 9

X wins!
```

</details>

<details>

<summary> 1,4,2,5,3 </summary>

```
 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
1
You selected 1
 X | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player O's turn!
What index do you want to move to?
4
You selected 4
 X | 2 | 3
---+---+---
 O | 5 | 6
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
2
You selected 2
 X | X | 3
---+---+---
 O | 5 | 6
---+---+---
 7 | 8 | 9

Player O's turn!
What index do you want to move to?
5
You selected 5
 X | X | 3
---+---+---
 O | O | 6
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
3
You selected 3
 X | X | X
---+---+---
 O | O | 6
---+---+---
 7 | 8 | 9

X wins!
```

</details>

<details>

<summary> 3,2,6,5,8,9,1,4,7 </summary>

```
 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
3
You selected 3
 1 | 2 | X
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player O's turn!
What index do you want to move to?
2
You selected 2
 1 | O | X
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
6
You selected 6
 1 | O | X
---+---+---
 4 | 5 | X
---+---+---
 7 | 8 | 9

Player O's turn!
What index do you want to move to?
5
You selected 5
 1 | O | X
---+---+---
 4 | O | X
---+---+---
 7 | 8 | 9

Player X's turn!
What index do you want to move to?
8
You selected 8
 1 | O | X
---+---+---
 4 | O | X
---+---+---
 7 | X | 9

Player O's turn!
What index do you want to move to?
9
You selected 9
 1 | O | X
---+---+---
 4 | O | X
---+---+---
 7 | X | O

Player X's turn!
What index do you want to move to?
1
You selected 1
 X | O | X
---+---+---
 4 | O | X
---+---+---
 7 | X | O

Player O's turn!
What index do you want to move to?
4
You selected 4
 X | O | X
---+---+---
 O | O | X
---+---+---
 7 | X | O

Player X's turn!
What index do you want to move to?
7
You selected 7
 X | O | X
---+---+---
 O | O | X
---+---+---
 X | X | O

Game Over!
```

</details>


