{-
Several Lambda Calculus challenges
FP/Introduction, Easy, 5 pts a piece

- Lambda Calculus - Evaluating Expressions #1 (https://www.hackerrank.com/challenges/lambda-calculus-getting-started):

  > (λx.x+1)3 == 4

- Lambda Calculus - Evaluating Expressions #2 (https://www.hackerrank.com/challenges/lambda-calculus-understanding-the-syntax)

  > (λx.x+1)((λy.y+2)3) == 6

- Lambda Calculus - Evaluating Expressions #3 (https://www.hackerrank.com/challenges/lambda-calculus-evaluate-the-expression)

  > (in Church encoding) λx.λy.x^47y == 47

- Lambda Calculus - Evaluating Expressions #4 (https://www.hackerrank.com/challenges/lambda-calculus-evaluate-the-expression-1)

  > (in Church numerals) λx.λy.x(xy) == 2

- Lambda Calculus - Evaluating Expressions #5 (https://www.hackerrank.com/challenges/lambda-calculus-evaluate-the-expression-2)

  > (in Church numerals) λx.λy.y == 0

- Lambda Calculus - Reductions #1 (https://www.hackerrank.com/challenges/lambda-calculus-reductions-1)

  > ((λx.(x y))(λz.z)) == y

- Lambda Calculus - Reductions #2 (https://www.hackerrank.com/challenges/lambda-calculus-reductions-2)

  > ((λx.((λy.(x y))x))(λz.w)) == w

- Lambda Calculus - Reductions #3 (https://www.hackerrank.com/challenges/lambda-calculus-reductions-3)

  > ((λx.(x x))(λx.(x x))) == CAN'T REDUCE

- Lambda Calculus - Reductions #4 (https://www.hackerrank.com/challenges/lambda-calculus-reductions-4)

  > (λg.((λf.((λx.(f (x x)))(λx.(f (x x))))) g)) == CAN'T REDUCE

-}
