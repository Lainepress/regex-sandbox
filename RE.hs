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


infixl 4 :|
infixl 5 :%%

test_eps :: RExp
test_eps = Eps

test_asbs :: RExp
test_asbs = Star (Ch (=='a')) :%% Star (Ch (=='b'))

test_as :: RExp
test_as = Eps :| Ch (=='a') :%% test_as

test_bs :: RExp
test_bs = Eps :| Ch (=='b') :%% test_bs

test_asbs' :: RExp
test_asbs' = test_as :%% test_bs



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
  Ch f    -> "[" ++ filter f alphabet ++ "]"
  l :%% r -> pretty l ++ pretty r
  l :| r  -> pretty l ++ "|" ++ pretty r
  Star r  -> pretty r ++ "*"
  Plus r  -> pretty r ++ "+"
  Ques r  -> pretty r ++ "?"

alphabet :: [Char]
alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


{------------------------------------------------------------------------------
                          Abstract Regular Expression
------------------------------------------------------------------------------}


-- This section contains demonstrations; it is not part of Alex.

-- This function illustrates `ARexp'. It returns true if the string in its
-- argument is matched by the regular expression.

recognise:: RExp -> String -> Bool
recognise re inp = any (==len) (ap_ar (arexp re) inp)
        where
        len = length inp


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
arexp (re :| re')  = arexp re `bar_ar` arexp re'
arexp (Star re)    = star_ar (arexp re)
arexp (Plus re)    = plus_ar (arexp re)
arexp (Ques re)    = ques_ar (arexp re)


star_ar:: ARexp -> ARexp
star_ar sc =  eps_ar `bar_ar` plus_ar sc

plus_ar:: ARexp -> ARexp
plus_ar sc = sc `seq_ar` star_ar sc

ques_ar:: ARexp -> ARexp
ques_ar sc = eps_ar `bar_ar` sc


type ARexp = String -> [Int]

ap_ar:: ARexp -> String -> [Int]
ap_ar sc = sc

eps_ar:: ARexp
eps_ar inp = [0]

ch_ar:: (Char->Bool) -> ARexp
ch_ar p "" = []
ch_ar p (c:rst) = if p c then [1] else []

seq_ar:: ARexp -> ARexp -> ARexp
seq_ar sc sc' inp = [n+m| n<-sc inp, m<-sc' (drop n inp)]

bar_ar:: ARexp -> ARexp -> ARexp
bar_ar sc sc' inp = sc inp ++ sc' inp


-- -----------------------------------------------------------------------------
-- Code generation targets

data Target = GhcTarget | HaskellTarget
