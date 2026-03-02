module SMTLib.Parse

import Data.List
import Data.String
import SMTLib.AST

%default total

-- ============================================================================
-- Simple String-based S-Expression Parser
-- ============================================================================

||| Check if character is valid symbol character
isSymbolChar : Char -> Bool
isSymbolChar c = isAlphaNum c || c == '~' || c == '!' || c == '@'
              || c == '#' || c == '$' || c == '%' || c == '^' || c == '&'
              || c == '*' || c == '_' || c == '-' || c == '+' || c == '='
              || c == '<' || c == '>' || c == '.' || c == '?' || c == '/'

||| Check if character is a digit
isDigitChar : Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

||| Drop leading whitespace
dropSpaces : String -> String
dropSpaces s = pack $ dropWhile isSpace (unpack s)

||| Read a symbol from string
readSymbol : String -> Maybe (String, String)
readSymbol s = case unpack s of
  [] => Nothing
  (c :: cs) => if isSymbolChar c
    then let (sym, rest) = span isSymbolChar (c :: cs)
         in Just (pack sym, pack rest)
    else Nothing

||| Read a keyword (starts with colon)
readKeyword : String -> Maybe (String, String)
readKeyword s = case unpack s of
  (':' :: cs) => case readSymbol (pack cs) of
    Just (kw, rest) => Just (":" ++ kw, rest)
    Nothing => Nothing
  _ => Nothing

||| Read digits
readDigits : String -> Maybe (String, String)
readDigits s = case unpack s of
  [] => Nothing
  (c :: cs) => if isDigitChar c
    then let (ds, rest) = span isDigitChar (c :: cs)
         in Just (pack ds, pack rest)
    else Nothing

mutual
  ||| Parse an S-Expression from string
  ||| Returns (parsed result, remaining string)
  export
  parseSExpr' : String -> Maybe (SExpr, String)
  parseSExpr' s = let s' = dropSpaces s in
    case unpack s' of
      [] => Nothing
      ('(' :: rest) => case assert_total (parseSList' (pack rest)) of
        Just (exprs, rest') => let s'' = dropSpaces rest' in
          case unpack s'' of
            (')' :: rs) => Just (SList exprs, pack rs)
            _ => Nothing
        Nothing => Nothing
      (':' :: _) => case readKeyword s' of
        Just (kw, rest) => Just (SKeyword kw, rest)
        Nothing => Nothing
      _ => case readSymbol s' of
        Just (sym, rest) => Just (SSymbol (MkSymbol sym), rest)
        Nothing => Nothing

  ||| Parse a list of S-Expressions
  parseSList' : String -> Maybe (List SExpr, String)
  parseSList' s = let s' = dropSpaces s in
    case unpack s' of
      [] => Just ([], "")
      (')' :: _) => Just ([], s')
      _ => case assert_total (parseSExpr' s') of
        Nothing => Just ([], s')
        Just (e, rest) => case assert_total (parseSList' rest) of
          Nothing => Just ([e], rest)
          Just (es, rest2) => Just (e :: es, rest2)

||| Parse an S-Expression (full string, fail if not all consumed)
export
parseSExpr : String -> Maybe SExpr
parseSExpr s = case parseSExpr' s of
  Just (e, rest) => if all isSpace (unpack (dropSpaces rest)) then Just e else Nothing
  Nothing => Nothing

||| Parse a list of S-Expressions
export
parseSList : String -> Maybe (List SExpr)
parseSList s = case parseSList' s of
  Just (es, rest) => if all isSpace (unpack (dropSpaces rest)) then Just es else Nothing
  Nothing => Nothing