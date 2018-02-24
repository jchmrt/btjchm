{-# LANGUAGE OverloadedStrings #-}
module Calc ( calculate ) where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String

-- |Look up an Identifier
lookupId :: String -> Double
lookupId "pi" = pi
lookupId _    = nan

-- |Look up a Function
lookupFunc :: String -> ([Double] -> Double)
lookupFunc "sin"  = unFunc sin
lookupFunc "cos"  = unFunc cos
lookupFunc "tan"  = unFunc tan
lookupFunc "asin" = unFunc asin
lookupFunc "acos" = unFunc acos
lookupFunc "atan" = unFunc atan
lookupFunc "ln"   = unFunc log
lookupFunc "sqrt" = unFunc sqrt
lookupFunc "log"  = binFunc logBase
lookupFunc "max"  = binFunc max
lookupFunc _      = const nan

-- |Look up an Operator
lookupOp :: Char -> [Double] -> [Double]
lookupOp '^' = binOp (**)
lookupOp '*' = binOp (*)
lookupOp '/' = binOp (/)
lookupOp '+' = binOp (+)
lookupOp '-' = binOp (-)
lookupOp _   = binOp (\_ _ -> nan)

-- Function generators
unFunc :: (Double -> Double) -> [Double] -> Double
unFunc fun (f:_) = fun f
unFunc _ _ = nan

binFunc :: (Double -> Double -> Double) -> [Double] -> Double
binFunc fun (a:b:_) = fun a b
binFunc _ _ = nan

-- Operator generators
binOp :: (Double -> Double -> Double) -> [Double] -> [Double]
binOp f (b:a:fs) = f a b:fs
binOp _ _        = [nan]

-- Not a Number
nan :: Double
nan = 0.0/0.0


-- Interpretion:

data Token = Number Double
           | Operator Int Char
           | Identifier String
           | Function String [[Token]]
           | Parens [Token]
           deriving (Show, Eq)

data Out = Num Double
         | Op Char
         deriving (Show, Eq)

calculate :: String -> T.Text
calculate = T.pack . show . interpret . parseTokens

interpret :: [Token] -> Double
interpret = rpn . shuntyard . removeFuncs

rpn :: [Out] -> Double
rpn operators = rpn' operators []
  where
    rpn' :: [Out] -> [Double] -> Double
    rpn' [] (f:_) = f
    rpn' (Num f:ops) fs = rpn' ops (f:fs)
    rpn' (Op c:ops) fs = rpn' ops (lookupOp c fs)
    rpn' _ _ = nan

shuntyard :: [Token] -> [Out]
shuntyard toks = reverse $ shunt toks [] []
  where
    shunt :: [Token] -> [Token] -> [Out] -> [Out]
    shunt [] [] out = out
    shunt [] (Operator _ c:ops) out = shunt [] ops (Op c:out)
    shunt (Number f:ts) ops out = shunt ts ops (Num f:out)
    shunt (Operator p c:ts) [] out = shunt ts [Operator p c] out
    shunt (Operator p1 c1:ts) (Operator p2 c2:ops) out
      | p2 >= p1  = shunt (Operator p1 c1:ts) ops (Op c2:out)
      | otherwise = shunt ts (Operator p1 c1:Operator p2 c2:ops) out
    shunt _ _ _ = []

removeFuncs :: [Token] -> [Token]
removeFuncs = map removeFunc
  where
    removeFunc :: Token -> Token
    removeFunc (Function f ts) = Number $ (lookupFunc f) (map interpret ts)
    removeFunc (Parens ts) = Number $ interpret ts
    removeFunc (Identifier i) = Number $ lookupId i
    removeFunc t = t


-- Parsing

parseTokens :: String -> [Token]
parseTokens str = case parse (many1 tok) "" str of
  Left _ -> []
  Right ts -> ts

nat :: Integral i => Parser i
nat = fromInteger . read <$> many1 digit

int :: Integral i => Parser i
int = (*) <$> option 1 (-1 <$ char '-') <*> nat

float :: (Read a, RealFloat a) => Parser a
float = read <$> floatStr
  where
    floatStr = (++) <$> (show <$> int)
                    <*> (option "" ((:) <$> char '.' <*> (show <$> nat)))

number :: Parser Token
number = Number <$> float

operator :: Parser Token
operator = (Operator 4 <$> char '^')
           <|> (Operator 3 <$> char '*')
           <|> (Operator 3 <$> char '/')
           <|> (Operator 2 <$> char '+')
           <|> (Operator 2 <$> char '-')

function :: Parser Token
function = do
  name <- many1 letter
  _ <- char '('
  args <- sepBy funcArg (char ',')
  _ <- char ')'
  return $ Function name args

funcArg :: Parser [Token]
funcArg = many1 tok

identifier :: Parser Token
identifier = Identifier <$> many1 letter

parens :: Parser Token
parens = do
  _ <- char '('
  toks <- many1 tok
  _ <- char ')'
  return $ Parens toks

tok :: Parser Token
tok = do
  spaces
  res <- try operator <|> try number <|> try function <|> try identifier <|> parens
  spaces
  return res

