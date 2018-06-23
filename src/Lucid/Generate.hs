{-# LANGUAGE PackageImports #-}

-- | A module for conversion from HTML to Lucid Haskell code.
--

module Lucid.Generate (
    module Lucid.Generate
)   where

import "base" Data.List (stripPrefix, intercalate)
import "base" Data.Maybe (listToMaybe)
import "base" Data.Char (toLower, isSpace)
import "base" Control.Arrow (first)

import "tagsoup" Text.HTML.TagSoup

import Lucid.Sanitize (sanitize)
import Lucid.Combinators

-- | Simple type to represent attributes.
--
type Attributes = [(String, String)]

-- | Intermediate tree representation. This representation contains several
-- constructors aimed at pretty-printing.
--
data Html = Parent String Attributes Html
          | Block [Html]
          | Text String
          | Comment String
          | Doctype
          deriving (Show)

-- | Different combinator types.
--
data CombinatorType = ParentCombinator
                    | LeafCombinator
                    | UnknownCombinator
                    deriving (Eq, Show)

-- | Traverse the list of tags to produce an intermediate representation of the
-- HTML tree.
--
makeTree :: Bool                  -- ^ Should ignore errors
         -> [String]              -- ^ Stack of open tags
         -> [Tag String]          -- ^ Tags to parse
         -> (Html, [Tag String])  -- ^ (Result, unparsed part)
makeTree ignore stack []
    | null stack || ignore = (Block [], [])
    | otherwise = error $ "Error: tags left open at the end: " ++ show stack
makeTree ignore stack (TagPosition row _ : x : xs) = case x of
    TagOpen tag attrs -> if toLower' tag == "!doctype"
        then addHtml Doctype xs
        else let tag' = toLower' tag
                 (inner, t) = case combinatorType tag' of
                    LeafCombinator -> (Block [], xs)
                    _ -> makeTree ignore (tag' : stack) xs
                 p = Parent tag' (map (first toLower') attrs) inner
             in addHtml p t
    -- The closing tag must match the stack. If it is a closing leaf, we can
    -- ignore it
    TagClose tag ->
        let isLeafCombinator = combinatorType tag == LeafCombinator
            matchesStack = listToMaybe stack == Just (toLower' tag)
        in case (isLeafCombinator, matchesStack, ignore) of
            -- It's a leaf combinator, don't care about this element
            (True, _, _)          -> makeTree ignore stack xs
            -- It's a parent and the stack doesn't match
            (False, False, False) -> error $
                "Line " ++ show row ++ ": " ++ show tag ++ " closed but "
                        ++ show stack ++ " should be closed instead."
            -- Stack might not match but we ignore it anyway
            (False, _, _)         -> (Block [], xs)
    TagText text -> addHtml (Text text) xs
    TagComment comment -> addHtml (Comment comment) xs
    _ -> makeTree ignore stack xs
  where
    addHtml html xs' = let (Block l, r) = makeTree ignore stack xs'
                       in (Block (html : l), r)

    toLower' = map toLower
makeTree _ _ _ = error "TagSoup error"

-- | Remove empty text from the HTML.
--
removeEmptyText :: Html -> Html
removeEmptyText (Block b) = Block $ map removeEmptyText $ flip filter b $ \h ->
    case h of Text text -> any (not . isSpace) text
              _         -> True
removeEmptyText (Parent tag attrs inner) =
    Parent tag attrs $ removeEmptyText inner
removeEmptyText x = x

-- | Try to eliminiate Block constructors as much as possible.
--
minimizeBlocks :: Html -> Html
minimizeBlocks (Parent t a (Block [x])) = minimizeBlocks $ Parent t a x
minimizeBlocks (Parent t a x) = Parent t a $ minimizeBlocks x
minimizeBlocks (Block x) = Block $ map minimizeBlocks x
minimizeBlocks x = x

-- | Get the type of a combinator
--
combinatorType :: String -> CombinatorType
combinatorType combinator
    | combinator == "docTypeHtml" = ParentCombinator
    | combinator == "tt" = ParentCombinator
    | combinator `elem` parents html5 = ParentCombinator
    | combinator `elem` leafs html5 = LeafCombinator
    | otherwise = UnknownCombinator

-- | Produce the Lucid code from the HTML. The result is a list of lines.
--
fromHtml :: Options      -- ^ Building options
         -> Html         -- ^ HTML tree
         -> [String]     -- ^ Resulting lines of code
fromHtml _ Doctype = ["doctype_"]
fromHtml opts t
  | (Text text) <- t
    = ["\"" ++ concatMap escape (trim text) ++ "\""]
  -- preserve comments as is
  | (Comment comment) <- t
    = ["toHtmlRaw  \"<!--" ++ concatMap escape comment ++ "-->\""]
  where
    -- Remove whitespace on both ends of a string
    trim
      | noTrimText_ opts = id
      | otherwise        = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    -- Escape a number of characters
    escape '"'  = "\\\""
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape '\\' = "\\\\"
    escape x    = [x]
-- fromHtml _ (Comment comment) = map ("-- " ++) $ lines comment
fromHtml opts (Block block) =
    concatMap (fromHtml opts) block
fromHtml opts (Parent tag attrs inner) =
    case combinatorType tag of
        -- Actual parent tags
        ParentCombinator -> case inner of
            (Block ls) -> if null ls
                then [combinator ++
                        (if null attrs then " " else " $ ") ++ "\"\""]
                else (combinator ++ " $ do") :
                        indent (fromHtml opts inner)
            -- We join non-block parents for better readability.
            x -> let ls = fromHtml opts x
                     apply = if dropApply x then " " else " $ "
                 in case ls of (y : ys) -> (combinator ++ apply ++ y) : ys
                               [] -> [combinator]

        -- Leaf tags
        LeafCombinator -> [combinator]

        -- Unknown tag
        UnknownCombinator -> if ignore_ opts
            then fromHtml opts inner
            else error $ "Tag " ++ tag ++ " is illegal in html5"
  where
    combinator :: String
    combinator = sanitize tag ++ attributes' attrs
    attributes' :: Show a => [(String, a)] -> [Char]
    -- hack for <br> that need attributes in Lucid
    attributes' [] = if sanitize tag == "br_" 
                       then " []"
                       else ""
    attributes' xs =  (" [ " ++) . (++ " ]") . intercalate ", " . fmap displayAttr $ xs
    displayAttr :: Show a => (String, a) -> String
    displayAttr (k, v) = case k `elem` attributes html5 of
        True  -> let k' = sanitize k 
                 in case k' of 
                        "autofocus_" -> k'
                        "checked_" -> k'
                        _ -> k' ++ " " ++ show v
        False -> case stripPrefix "data-" k of
            Just prefix -> "data_" ++ " "
                        ++ show prefix
                        ++ " " ++ show v
            Nothing | ignore_ opts -> ""
                    | otherwise  -> error $ "Attribute "
                                 ++ k ++ " is illegal in html5"

    -- Check if we can drop the apply operator ($), for readability reasons.
    -- This would change:
    --
    -- > p $ "Some text"
    --
    -- Into
    --
    -- > p "Some text"
    --
    dropApply (Parent _ _ _) = False
    dropApply (Block _) = False
    dropApply _ = null attrs

-- | Produce the code needed for initial imports.
--
getImports :: [String]
getImports =
    [ "{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}"
    , ""
    , "import Lucid"
    ]

-- | Imports for extra functions
--
getExtraImports :: [String]
getExtraImports =
    [ "import Lucid.Base"
    , "import Data.Text"
    ]

-- | Produce the code for IO
--
getIOImports :: [String]
getIOImports = 
    [ "import System.IO (stdout)"
    , "import Data.Text.Lazy.IO as L"
    , ""
    , "main :: IO ()"
    , "main = L.hPutStr stdout (renderText template1)"
    ]

-- | Functions to generate missed attributes
--
getExtraFunctions :: [String]
getExtraFunctions =
    [ "ariaHidden_ :: Text -> Attribute"
    , "ariaHidden_ = makeAttribute \"aria-hidden\""
    , ""
    , "property_ :: Text -> Attribute"
    , "property_ = makeAttribute \"property\""
    ]


-- | Convert the HTML to lucid code.
--
lucidFromHtml :: Options      -- ^ Build options
              -> String       -- ^ Template name
              -> String       -- ^ HTML code
              -> String       -- ^ Resulting code
lucidFromHtml opts name =
    unlines . addSignature . fromHtml opts
            . minimizeBlocks
            . removeEmptyText . fst . makeTree (ignore_ opts) []
            . parseTagsOptions parseOptions { optTagPosition = True }
  where
    addSignature body = [ name ++ " :: Html ()"
                        , name ++ " = do"
                        ] ++ indent body

-- | Indent block of code.
--
indent :: [String] -> [String]
indent = map ("    " ++)

-- | The options record passed to 'lucidFromHtml'
--
data Options = Options
             { ignore_     :: Bool -- ^ ignore errors
             , noTrimText_ :: Bool -- ^ do not trim text
             }
  deriving (Show)

