{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ViewPatterns #-}

-- | The purpose of this package is to associate web pages each with a
-- data type that contains all the necessary parameters for that page,
-- by automatically deriving that representation from the data types
-- themselves. It is an experimental package to test the idea.
-- 
-- The data type constructors must be nullary or record, and there
-- should only be one constructor (later this might be revised). The
-- fields of the constructor /MUST/ each be prefixed with the name of
-- the constructor, as per the common idiom when dealing with Haskell
-- data types. This also ensures that no data-type-generated URL can
-- be in conflict with another, code-wise or representation-wise.
--
-- For example, consider a page displays some conference/event. It
-- ought to be defined thiswise:
--
-- @
-- data Event = Event { eventId     :: Maybe Integer -- ^ The event id.
--                    , eventScope  :: Bool          -- ^ Show the scope?
--                    , eventLayout :: Layout        -- ^ Layout for the page.
--                    }
--   deriving (Data,Typeable,Show)
-- @
--
-- And an Enum type for layout:
-- 
-- @
-- data Layout =
--   Wide | Thin | Collapsed
--   deriving (Typeable,Data,Show,Enum)
-- @
--
-- (@Show@ is not required, but is included for inspection purposes.)
--
-- Now I can format that as a URL:
--
-- @
-- λ> formatURLPath $ Event (Just 0) False  Wide
-- \"\/event\/id\/0\/layout\/wide\"
-- @
-- 
-- And parse that URL back in:
--
-- @
-- λ> parseURLPath \"\/event\/id\/0\/layout\/wide\" :: Maybe Event
-- Just (Event {eventId = Just 0, eventScope = False, eventLayout = Wide})
-- @
--
-- Nullary data types also work:
--
-- @
-- data Home = Home
--   deriving (Data,Typeable,Show)
-- 
-- λ> parseURLPath \"/home\" :: Maybe Home
-- Just Home
-- λ> formatURLPath Home
-- \"/home\"
-- λ> 
-- @
--
-- The supported types for URL parameters are:
-- 
--    * Standard @Integer@ type.
-- 
--    * Standard @Bool@ type.
--    
--    * Any type with nullary constructors (such as @Layout@ above.). 
--    
--    * @Maybe a@ where @a@ is one of the above. Maybe is useful for
--      optionally omitting parameters from URLs.
-- 
-- Any other types cannot (should not (?)) be serialized at the URL-level.
-- 
-- There is the possibility to read/write @String@, but it does make
-- much sense to put arbitrary strings in URLs, so it does no special
-- encoding/decoding for @String@s. There is, however, the use case for
-- encoding slugs, such as blog titles, e.g. @\/posts\/name\/my-blog-title@, and that
-- is why support is included. You must ensure that these are properly
-- normalized yourself.
--

module Web.URL.Generic
  (-- * Parsing and formatting
   parseURLPath
  ,formatURLPath
   -- * Internal API (i.e. not stable; subject to change; for educational purposes)
   -- ** Parsing
  ,Parse
  ,parseCons
  ,urlToAssoc
  ,fromURLString
  ,parseData
  ,parseInteger
  ,parseBool
  -- ** Formatting
  ,formatCons
  ,formatURLParamValue
  ,showData
  -- ** Misc
  ,fieldToKey
  ,upperToDashes
  ,dashesToUpper)
  where

import Data.List
import Control.Applicative
import Data.Data
import Data.Generics.Text
import Control.Monad.State
import Data.Generics.Aliases
import Data.Char

-- | Simple maybe alias.
type Parse a = Maybe a

-- | Parse a URL path, e.g. @\/foo\/id\/1@, into its corresponding data
-- type, e.g. @Foo { fooId = 1 }@.
parseURLPath :: Data a
             => String  -- ^ The URL path.
             -> Maybe a -- ^ The record.
parseURLPath (span (/='/') . dropWhile (=='/') -> (cons,rest)) =
  case rest of
    '/':fields -> parseCons cons $ urlToAssoc fields
    _          -> parseCons cons []

-- | Format a record value, e.g. @Foo { fooId = 1 }@, into its
-- corresponding URL path, e.g. @\/foo\/id\/1@.
formatURLPath :: Data a
              => a      -- ^ The record.
              -> String -- ^ The URL path.
formatURLPath = formatCons
{-# INLINE formatURLPath #-}

-- | Format a constructor value to a URL.
formatCons :: Data a => a -- ^ The record value to format, e.g. @Foo { fooId = 1 }@
           -> String      -- ^ The corresponding URL, e.g. @\/foo\/id\/1@
formatCons d = "/" ++ intercalate "/" (ncons : concat pairs) where
  pairs = map (\(key,value) -> [key,value]) $ filter nempty $ zip keys values
  nempty = not . null . snd
  keys = map (fieldToKey cons) (constrFields cons)
  values = gmapQ formatURLParamValue d
  cons = indexConstr (dataTypeOf d) 1
  ncons = upperToDashes $ showConstr cons

-- | Format a constructor's field value to a URL parameter.
formatURLParamValue :: Data a
                    => a      -- ^ A URL parameter value.
                    -> String -- ^ A URL-friendly version.
formatURLParamValue =
  showData
  `extQ`
  (show :: Integer -> String)
  `extQ`
  (showMaybe show :: Maybe Integer -> String)
  `extQ`
  (showBool :: Bool -> String)
  `extQ`
  (showMaybe showBool :: Maybe Bool -> String)
  `extQ`
  (id :: String -> String)

    where showMaybe f (Just x) = f x
          showMaybe _ Nothing  = ""
          showBool True = "true"; showBool False = ""

-- | Show any Haskell (Data instance) constructor e.g. @FooBar@ to
-- @foo-bar@. This is only reliable for nullary constructors like Enums
-- and such. But that by itself is very useful, so it's worth including.
showData :: Data a => a -> String
showData = upperToDashes . filter (not . (`elem` "()")) . gshow

-- | URL string to association list.
urlToAssoc :: String -- ^ A string containing \/foo\/1\/bar\/2 key\/values.
           -> [(String,String)] -- ^ An association list.
urlToAssoc cs = assoc $ go cs [] where
  go ('/':cs) acc = reverse acc : go cs []
  go (c:cs)   acc = go cs (c:acc)
  go []       acc = reverse acc : []
  
  assoc (key:value:xs) = (key,value) : assoc xs
  assoc _ = []

-- | Parse a constructor from a string.
parseCons :: Data a
          => String            -- ^ The constructor name.
          -> [(String,String)] -- ^ The parameters.
          -> Maybe a           -- ^ The record value.
parseCons consName assocs = check where
  check | consName == upperToDashes (show cons) = parse
        | otherwise = Nothing
  parse = do
    keyvalues <- mapM getField $ constrFields cons
    evalStateT (fromConstrM (do ((_key,value):cs) <- get
                                put cs
                                case fromURLString value of
                                  Just x -> return x
                                  _      -> lift $ Nothing)
                            cons)
               keyvalues
  getField (fieldToKey cons -> key) =
     case lookup key assocs of
       Just value -> return (key,value)
       Nothing    -> return (key,"")
  cons = indexConstr (dataTypeOf $ resType parse) 1
  resType :: Maybe a -> a
  resType = undefined

-- | Parse a URL string into a simple value (integer\/bool\/string).
fromURLString :: Data a => String -> Maybe a
fromURLString str =
  parseData str
  `extR`
  (opt parseInteger str :: Parse (Maybe Integer))
  `extR`
  (opt parseBool str :: Parse (Maybe Bool))
  `extR`
  (pure (Just str) :: Parse (Maybe String))
  `extR`
  (parseInteger str :: Parse Integer)
  `extR`
  (parseBool str :: Parse Bool)
  `extR`
  (return str :: Parse String)

  where opt p str = Just <$> p str <|> return Nothing

-- | Parse any constructor. It really only works well for nullary
-- constructors like Enum values, but that in itself is very useful.
parseData :: Data a => String -> Parse a
parseData str =
  case gread ("(" ++ dashesToUpper str ++ ")") of
    [(x,"")] -> return x
    _        -> case gread ("(Just (" ++ dashesToUpper str ++ "))") of
      [(x,"")] -> return x
      _        -> case gread ("(Nothing)") of
        [(x,"")] | null str -> return x
        _        -> Nothing

-- | Parse an integer.
parseInteger :: String -> Parse Integer
parseInteger x =
  case reads x of
    [(x,"")] -> return x
    _        -> Nothing

-- | Parse a boolean (@true@\/@false@).
parseBool :: String -> Parse Bool
parseBool x =
  case reads (caseUp x) of
    [(x,"")] -> return x
    _ | x == ""   -> return False
      | otherwise -> Nothing
      
  where caseUp (x:xs) = toUpper x : xs
        caseUp []     = []

-- | Normalize a record field to a slug-ish name e.g. @fooBarMu@ => @bar-mu@.
fieldToKey :: Show a
           => a      -- ^ The Haskell constructor e.g. @Foo@.
           -> String -- ^ The field name e.g. @fooBarMu@.
           -> String -- ^ The slug e.g. @bar-mu@.
fieldToKey cons = upperToDashes . drop (length (show cons))

-- | Convert uppercase @CamelCase to slug-ish camel-case@.
upperToDashes :: [Char] -> [Char]
upperToDashes = drop 1 . go where
  go (c:cs)
    | isUpper c = '-' : toLower c : go cs
    | otherwise = c : go cs
  go [] = []

-- | Convert slug-ish @camel-case@ to uppercase @CamelCase@.
dashesToUpper :: [Char] -> [Char]
dashesToUpper     = uphead . go where
  go ('-':'-':cs) = go ('-' : cs)
  go ('-':c:cs)   = toUpper c : go cs
  go (c:cs)       = c : go cs
  go []           = []
  uphead (c:cs) = toUpper c : cs
  uphead []     = []
