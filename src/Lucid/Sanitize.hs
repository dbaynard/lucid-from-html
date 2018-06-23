{-# LANGUAGE PackageImports #-}

-- | A program to sanitize an HTML tag to a Haskell function.
--

module Lucid.Sanitize (
    module Lucid.Sanitize
)   where

import "base" Data.Char (toLower, toUpper)

-- | Sanitize a tag. This function returns a name that can be used as
-- combinator in haskell source code.
--
-- Examples:
--
-- > sanitize "class" == "class_"
-- > sanitize "http-equiv" == "httpEquiv"
--
sanitize :: String -> String
sanitize str
    | lower == "tt" = "code_"
    | otherwise               = appendUnderscore $ removeDash lower
  where
    lower = map toLower str

    -- Remove a dash, replacing it by camelcase notation
    --
    -- Example:
    --
    -- > removeDash "foo-bar" == "fooBar"
    --
    removeDash ('-' : x : xs) = toUpper x : removeDash xs
    removeDash (x : xs) = x : removeDash xs
    removeDash [] = []

    appendUnderscore = (++ "_")

