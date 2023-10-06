{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder
import Data.Char

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('

-- Helper Parser for parenthesis
paren :: Parser Char
paren = (is '(') ||| (is ')')

-- chain function from https://tgdwyer.github.io/parsercombinators/ (Course Note) to handle repeated chains of operators 
chain :: Parser a -> Parser (a->a->a) -> Parser a
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) ||| pure a

--alpha functiom from week 11 tut solution
alpha :: Parser Char
alpha = satisfy isAlpha

-- long lambda expression parser for input
longLamExp :: Parser Builder
longLamExp = chain longLamBody (pure ap) 

-- Combine multiple parsers to parse the expression
longLamBody :: Parser Builder
longLamBody = longLambdaParen ||| longLamStart ||| (term <$> alpha)

-- parse the expression with parenthesis
longLambdaParen :: Parser Builder
longLambdaParen = do
    paren
    res <- longLamExp
    paren
    return res

-- parse the lambda symbol
longLamStart :: Parser Builder
longLamStart = do
    is 'λ' 
    var <- alpha
    is '.'
    lam var <$> longLamExp

-- long lambda expression parser
longLambdaP :: Parser Lambda
longLambdaP = build <$> longLamExp 

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

-- short lambda expression parser
shortLamExp :: Parser Builder
shortLamExp = (do
    is 'λ'
    shortLamParam
    ) 
    ||| shortLambdaParen

-- parse the parameter of short lambda expression
shortLamParam :: Parser Builder
shortLamParam = do
    var <- alpha
    lam var <$> shortLamHead

-- Parser that chain multiple parsers, The posible structure of head of short lambda expression
shortLamHead :: Parser Builder
shortLamHead = chain (shortLamParam ||| shortLambdaParen ||| dot) (pure ap)

-- Parser that chain multiple parsers, The posible structure of head of short lambda expression
shortLamBody :: Parser Builder
shortLamBody = chain (shortLamExp ||| shortLambdaParen ||| (term <$> alpha)) (pure ap)

-- dot parser
dot :: Parser Builder
dot = do
    is '.' 
    shortLamBody

-- short lambda parenthesis parser same as the long lambda 
shortLambdaParen :: Parser Builder
shortLambdaParen = do
    paren
    res <- shortLamBody
    paren
    return res

-- short lambda expression parser
shortLambdaP :: Parser Lambda
shortLambdaP = build <$> (chain shortLamExp (pure ap))

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

-- parser for any lambda expression, either long or short
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

--function from week 11 tut solution
-- | Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
spaces1 :: Parser String
spaces1 = list1 space

-- parse string that might have space infront it
string1 :: String -> Parser String
string1 x = do
    spaces 
    string x
    return x

-- parse string that might have space infront it and must have space after it
opString :: String -> Parser String
opString x = do
    spaces 
    string x
    spaces1
    return x

-- chain but for right associative function from hoogle
chainr :: Parser a -> Parser (a->a->a) -> Parser a
chainr p op = p >>= rest
  where
    rest x = ((\f y -> f x y) <$> op <*> (p >>= rest))
             ||| pure x

-- builder for if, and, or, not
ifLamb :: Builder
ifLamb = lam 'b' (lam 't' (lam 'f' (term 'b' `ap` term 't' `ap` term 'f')))

andLamb :: Builder -> Builder -> Builder
andLamb x y =  lam 'x' (lam 'y' $ ifLamb `ap` term 'x' `ap` term 'y' `ap` boolToLam False) `ap` x `ap` y

orLamb :: Builder -> Builder -> Builder
orLamb x y = lam 'x' (lam 'y' $ ifLamb `ap` term 'x' `ap` boolToLam True `ap` term 'y') `ap` x `ap` y

notLamb :: Builder
notLamb = lam 'x' $ ifLamb `ap` term 'x' `ap` boolToLam False `ap` boolToLam True

-- parser for True and False
tfParser :: Parser Builder
tfParser = (do 
    string1 "True"
    return $ boolToLam True)
    |||
    (do
    string1 "False"
    return $ boolToLam False)

-- parser for if, and, or, not 
ifParser :: Parser Builder
ifParser = do
    string1 "if"
    return ifLamb

notParser :: Parser Builder
notParser = do
    opString "not"
    return notLamb

andParser :: Parser (Builder -> Builder -> Builder)
andParser = do
    opString "and"
    return andLamb

orParser :: Parser (Builder -> Builder -> Builder)
orParser = do
    opString "or"
    return orLamb

-- parser for the logic expression, will create a builder expression
logicStart :: Parser Builder
logicStart = chain orOp (pure ap)

-- parser that chain 'and' operator and parse for 'or' then create builder expression
orOp :: Parser Builder
orOp = chain andOp orParser

-- parser that chain 'not' operator and parse for 'and' then create builder expression
andOp :: Parser Builder
andOp = chain notOp andParser

-- parser that chain possible state for logic expression
notOp :: Parser Builder
notOp = chainr logicExp (pure ap)

-- possible state for logic expression
logicExp :: Parser Builder
logicExp = tfParser ||| notParser ||| logicParen ||| logicIf

-- state when the expression has parenthesis
logicParen :: Parser Builder
logicParen = do
    spaces 
    paren
    x <- logicStart
    paren
    return x

--  state when it encounter if
logicIf :: Parser Builder
logicIf = do
        op1 <- ifParser
        op2 <- logicStart
        opString "then"
        op3 <- logicStart
        opString "else"
        op4 <- logicStart
        return (op1 `ap` op2 `ap` op3 `ap` op4)

-- logical expression parser
logicP :: Parser Lambda
logicP = build <$> logicStart

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- builder expression for add, substract, multiply, power (exponent)
addB :: Builder -> Builder -> Builder
addB m n =  lam 'x' ( lam 'y' $ term 'y' `ap` succF `ap` term 'x' ) `ap` m `ap` n

subB :: Builder -> Builder -> Builder
subB m n = lam 'x' ( lam 'y' $ term 'y' `ap` predF `ap` term 'x' ) `ap` m `ap` n

multB :: Builder -> Builder -> Builder
multB m n = lam 'x' (lam 'y' $ lam 'f' $ term 'x' `ap` (term 'y' `ap` term 'f')) `ap` m `ap` n

power :: Builder -> Builder -> Builder
power m n = lam 'x' (lam 'y' $ term 'y' `ap` term 'x') `ap` m `ap` n

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- builder expression for succ and pred functions
succF :: Builder
succF = lam 'n' $ lam 'f' $ lam 'x' $ term 'f' `ap` ( term 'n' `ap` term 'f' `ap` term 'x' )

predF :: Builder
predF = lam 'n' $ lam 'f' $ lam 'x' $ term 'n' `ap` (lam 'g' $ lam 'h' $ term 'h' `ap` (term 'g' `ap` term 'f')) `ap` (lam 'u' $ term 'x') `ap` (lam 'u' $ term 'u')

-- from week 11 tut solution
-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
digit :: Parser String
digit = munch1 isDigit

-- parser for number modify from course note
number :: Parser Builder
number = do
    spaces 
    x <- digit
    let n = read x
    return (intToLam n)

-- parser for '+' and '-'
opParser :: Parser (Builder -> Builder -> Builder)
opParser = (do
    spaces
    is '+'
    return addB)
    |||(do
    spaces
    is '-'
    return subB)

-- basic arithmetic expression parser
basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> (chain number opParser)

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

-- parser for multiply and power 
multParser :: Parser (Builder -> Builder -> Builder)
multParser = do
    spaces
    is '*'
    return multB

powerParser :: Parser (Builder -> Builder -> Builder)
powerParser = do
    string1 "**"
    return power

-- parser for arithmetic expression that will create the builder expression
arithExp :: Parser Builder
arithExp = chain multOp opParser

-- parser for mult and chain powerOp
multOp :: Parser Builder
multOp = chain powerOp multParser

-- parser for power and chain for arithOp
powerOp :: Parser Builder
powerOp = chain arithOp powerParser

arithOp :: Parser Builder
arithOp = arithParen ||| number

-- parser for parenthesis in arithmetic expression
arithParen :: Parser Builder
arithParen = do
    paren
    x <- arithExp
    paren
    return x

-- arithmetic expression parser
arithmeticP :: Parser Lambda
arithmeticP = build <$> arithExp


-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True
-- builder expression for isZero
isZero :: Builder
isZero = lam 'n' (term 'n' `ap` lam 'x' (boolToLam False) `ap` boolToLam True)

-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False

-- builder expression for lesser equal, equal, not equal, greater, lesser, greater equal
leqB :: Builder -> Builder -> Builder
leqB = ap . ap (lam 'm' $ lam 'n' $ isZero `ap` subB (term 'm') (term 'n'))

eqB :: Builder -> Builder -> Builder
eqB = ap.ap(lam 'm' $lam 'n' (andLamb (leqB (term 'm') (term 'n')) (leqB (term 'n') (term 'm'))))

neqB :: Builder -> Builder -> Builder
neqB m n = notLamb `ap` eqB m n

grB :: Builder -> Builder -> Builder
grB m n = notLamb `ap` leqB m n

ngrB :: Builder -> Builder -> Builder
ngrB m n = grB n m 

greqB :: Builder -> Builder -> Builder
greqB m n = notLamb `ap` ngrB m n

-- parser for lesser equal, equal, not equal, greater, lesser, greater equal
leqParser :: Parser (Builder -> Builder -> Builder)
leqParser = do
    string1 "<="
    return leqB

greqParser :: Parser (Builder -> Builder -> Builder)
greqParser = do
    string1 ">="
    return greqB

eqParser :: Parser (Builder -> Builder -> Builder)
eqParser = do
    string1 "=="
    return eqB

neqParser :: Parser (Builder -> Builder -> Builder)
neqParser = do
    string1 "!="
    return neqB

grParser :: Parser (Builder -> Builder -> Builder)
grParser = do
    string1 ">"
    return grB

ngrParser :: Parser (Builder -> Builder -> Builder)
ngrParser = do
    string1 "<"
    return ngrB

-- possible comparator
compOp :: Parser (Builder -> Builder -> Builder)
compOp = leqParser ||| greqParser ||| eqParser ||| neqParser ||| grParser ||| ngrParser

-- parser for comparator expression that will create builder expression
compExp :: Parser Builder
compExp = chain compAnd compOp

compAnd :: Parser Builder
compAnd = chain compOr andParser 

compOr :: Parser Builder
compOr = chain (tfParser|||arithExp) orParser

-- comparator expression parser
complexCalcP :: Parser Lambda
complexCalcP = build <$> compExp

{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof

-- parser for square bracket
brac :: Parser Char
brac = (is '[') ||| (is ']')

-- builder expression for null, isNull, cons, head, tail
nullB :: Builder
nullB = lam 'c' (lam 'n' (term 'n'))

isNullB :: Builder
isNullB = lam 'l' $ term 'l' `ap` lam 'h' (lam 't' (boolToLam False)) `ap` boolToLam True

consB :: Builder
consB = lam 'h' $lam 't' $lam 'c' $lam 'n' (term 'c' `ap` term 'h' `ap` (term 't' `ap` term 'c' `ap` term 'n'))

headB :: Builder
headB =  lam 'l' (term 'l' `ap` lam 'h' (lam 't' (term 'h')) `ap` boolToLam False)

tailB ::  Builder
tailB = lam 'l' $ lam 'c' $ lam 'n' (term 'l' `ap` lam 'h' (lam 't' (lam 'g' (term 'g' `ap` term 'h' `ap` (term 't' `ap` term 'c')))) `ap` lam 't' (term 'n' `ap` lam 'h' (lam 't' (term 't'))))

-- parser for list expression that will create builder expression
listExp :: Parser Builder
listExp = chain listBody (pure ap)

-- possible list expression structure
listBody :: Parser Builder
listBody = empty ||| listStart ||| listEnd ||| comma ||| tfParser ||| number

-- parser for null
empty :: Parser Builder
empty = do
    spaces
    brac
    brac 
    return nullB

-- parser for '['
listStart :: Parser Builder
listStart = do
    spaces
    is '['
    a <- listBody
    b <- listExp
    return $ consB `ap` a `ap` b

-- parser for ']' thaat will return null builder
listEnd :: Parser Builder
listEnd = do
    spaces
    is ']'
    return nullB

-- parser for comma which will skip it
comma :: Parser Builder
comma = do
    spaces
    is ','
    spaces
    a <- listBody
    b <- listExp
    return $ consB `ap` a `ap` b

-- list expression parser
listP :: Parser Lambda
listP = build <$> listExp

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False

-- parser for head, rest(tail), isNull, cons
headParser :: Parser Builder
headParser = do
    string1 "head"
    spaces1
    return headB

restParser :: Parser Builder
restParser = do
    string1 "rest"
    spaces1
    return tailB

isNParser :: Parser Builder
isNParser = do 
    string1 "isNull"
    spaces1
    return isNullB

consParser :: Parser Builder
consParser = do
    string1 "cons"
    spaces
    return consB

-- list operation operator
listOp :: Parser Builder
listOp = headParser ||| restParser ||| isNParser ||| consParser

-- parser for list operation expression that will create its builder expression
listOpExp :: Parser Builder
listOpExp = chain listOpExps (pure ap)

listOpExps :: Parser Builder
listOpExps = listExp ||| listOp

-- list operation expression parser
listOpP :: Parser Lambda
listOpP = build <$> listOpExp


-- | Exercise 2

-- | Implement your function(s) of choice below!
