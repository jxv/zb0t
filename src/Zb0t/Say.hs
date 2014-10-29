{-# LANGUAGE FlexibleContexts #-}

module Zb0t.Say 
    ( zsay
    ) where

import Data.Char

data ZType
  = ZText
  | ZKeep

zsay :: String -> String
zsay str
  | or (map (not . isAscii) str) = "zuch zryp-ztic. zery zgly. zwow."
  | otherwise = concat $ map z (zeparate str)
  where z :: (ZType, String) -> String
        z (ZText, s) = zify (map toLower s)
	z (ZKeep, s) = s

zeparate :: String -> [(ZType, String)]
zeparate [] = []
zeparate str@(c:_) =
  let (ztype, (resolved, rest))
        | group c = (ZText, span group str)
        | otherwise =  (ZKeep, span (not . group) str)
  in (ztype, resolved) : (zeparate rest)
  where group x = isAlphaNum x || x `elem` "[]"

zify :: String -> String
zify str
  | length str < 4 = replicate (4 - length str) 'z' ++ str
  | length str == 4 = 'z' : drop 1 str 
  | hasVowel str = zify (dropVowel str)
  | otherwise = zify (drop 1 str)

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

hasVowel :: String -> Bool
hasVowel = or . map isVowel

dropVowel :: String -> String
dropVowel [] = []
dropVowel (c:str)
  | isVowel c = str
  | otherwise = c : dropVowel str

