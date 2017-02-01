-- -----------------------------------------------------------------------------
--
-- AbsSyn.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- This module provides a concrete representation for regular expressions and
-- scanners.  Scanners are used for tokenising files in preparation for parsing.
--
-- ----------------------------------------------------------------------------}

module AbsSyn where

import Data.Char

infixl 4 :|
infixl 5 :%%

test_eps :: RExp
test_eps = Eps

test_asbs :: RExp
test_asbs =  Star (Ch (=='a'))  :%%   Star (Ch (=='b'))

test_as_or_bs :: RExp
test_as_or_bs =  Star (Ch (=='a'))  :|   Star (Ch (=='b'))

test_as :: RExp
test_as = Eps :| Ch (=='a') :%% test_as

test_bs :: RExp
test_bs = Eps :| Ch (=='b') :%% test_bs

test_asbs' :: RExp
test_asbs' = test_as :%% test_bs

test_as_or_bs' :: RExp
test_as_or_bs' = test_as :| test_bs


-- -----------------------------------------------------------------------------
-- Regular expressions

-- `RExp' provides an abstract syntax for regular expressions.  `Eps' will
-- match empty strings; `Ch p' matches strings containinng a single character
-- `c' if `p c' is true; `re1 :%% re2' matches a string if `re1' matches one of
-- its prefixes and `re2' matches the rest; `re1 :| re2' matches a string if
-- `re1' or `re2' matches it; `Star re', `Plus re' and `Ques re' can be
-- expressed in terms of the other operators.  See the definitions of `ARexp'
-- for a formal definition of the semantics of these operators.

data RExp
  = Eps
  | Ch (Char->Bool)
  | RExp :%% RExp
  | RExp :| RExp
  | Star RExp
  | Plus RExp
  | Ques RExp

pretty :: RExp -> String
pretty re = case re of
  Eps     -> "()"
  Ch f    -> pretty_set f
  l :%% r -> pretty l ++ pretty r
  l :| r  -> pretty l ++ "|" ++ pretty r
  Star r  -> pretty r ++ "*"
  Plus r  -> pretty r ++ "+"
  Ques r  -> pretty r ++ "?"

string :: String -> RExp
string l = foldr (:%%) Eps (map (\c->Ch (c==)) l)

c :: RExp
c = Ch (== 'c')

h :: RExp
h = Ch (== 'h')

r :: RExp
r = Ch (== 'r')

i :: RExp
i = Ch (== 'i')

s :: RExp
s = Ch (== 's')

chris :: RExp
chris = c :%% (h :%% (r :%% (i :%% (s :%% Eps))))

tom :: RExp
tom = string "tom"

pretty_set :: (Char->Bool) -> String
pretty_set f = case length chs of
    1 -> chs
    _ -> "[" ++ chs ++ "]"
  where
    chs = filter f alphabet

alphabet :: [Char]
alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


{------------------------------------------------------------------------------
                          Abstract Regular Expression
------------------------------------------------------------------------------}


-- This section contains demonstrations; it is not part of Alex.

-- This function illustrates `ARexp'. It returns true if the string in its
-- argument is matched by the regular expression.

type ARexp = String -> [Int]

recognise :: RExp -> String -> Bool
recognise re inp = any (==length inp) $ arexp re inp

recognise' :: RExp -> String -> Bool
recognise' re inp = any (==length inp) $ arexp re inp

any' :: (a->Bool) -> [a] -> Bool
any' f l = or $ map (\x->f x) l

-- `ARexp' provides an regular expressions in abstract format.  Here regular
-- expressions are represented by a function that takes the string to be
-- matched and returns the sizes of all the prefixes matched by the regular
-- expression (the list may contain duplicates).  Each of the `RExp' operators
-- are represented by similarly named functions over ARexp.  The `ap' function
-- takes an `ARExp', a string and returns the sizes of all the prefixes
-- matching that regular expression.  `arexp' converts an `RExp' to an `ARexp'.


arexp:: RExp -> ARexp
arexp Eps          = eps_ar
arexp (Ch p)       = ch_ar p
arexp (re :%% re') = arexp re `seq_ar` arexp re'
arexp (re :|  re') = arexp re `bar_ar` arexp re'
arexp (Star re)    = star_ar (arexp re)
arexp (Plus re)    = plus_ar (arexp re)
arexp (Ques re)    = ques_ar (arexp re)


star_ar :: ARexp -> ARexp
star_ar sc =  eps_ar `bar_ar` plus_ar sc

plus_ar :: ARexp -> ARexp
plus_ar sc = sc `seq_ar` star_ar sc

ques_ar :: ARexp -> ARexp
ques_ar sc = eps_ar `bar_ar` sc


--ap_ar :: ARexp -> String -> [Int]
--ap_ar sc = sc

eps_ar :: ARexp
eps_ar _ = [0]

ch_ar :: (Char->Bool) -> ARexp
ch_ar _ ""    = []
ch_ar p (c:_) = if p c then [1] else []

seq_ar :: ARexp -> ARexp -> ARexp
seq_ar sc sc' = \inp -> [ n+m | n<-sc inp, m<-sc' (drop n inp) ]

bar_ar :: ARexp -> ARexp -> ARexp
bar_ar sc sc' = \inp -> sc inp ++ sc' inp


-- -----------------------------------------------------------------------------
-- Code generation targets


-- a fair merge
(%++) :: [a] -> [a] -> [a]
[]     %++ []     = []
(x:xs) %++ []     = x : xs
[]     %++ (y:ys) = y : ys
(x:xs) %++ (y:ys) = x : y : xs %++ ys


data Target = GhcTarget | HaskellTarget

data List a
  = Cons a (List a)
  | Nil

primes :: [Integer]
primes = sieve [2..]
  where
    sieve []     = error "primes"
    sieve (p:xs) = p : sieve [ x | x<-xs, x `mod` p /= 0 ]

qsort :: Ord a => [a] -> [a]
qsort []    = []
qsort (p:t) = qsort [ x | x<-t, x<p ] ++ [p] ++ qsort [ x | x<-t, x>=p ]

{-
data [a]
  = a : [a]
  | []
-}
