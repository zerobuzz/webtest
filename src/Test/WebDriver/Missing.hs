{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fwarn-unused-imports #-}

module Test.WebDriver.Missing
where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List as List
import Data.Monoid
import Data.String.Conversions
import Prelude hiding ((++))
import Test.WebDriver
import Test.WebDriver.Classes
import Text.Printf

import qualified Data.Text as ST


-- * selector sequences

-- | 'findElems'' etc below return an element several times with
-- different ID strings if it is matched by several layers.  This
-- function eliminates those duplicates.  This triggers @O(n^2)@ http
-- request, where @n@ is the size of the input list!
nubElems :: forall wd . (WebDriver wd) => [Element] -> wd [Element]
nubElems = f []
  where
    f :: [Element] -> [Element] -> wd [Element]
    f acc [] = return $ reverse acc
    f acc (x:xs) = do
        bad <- or <$> mapM (x <==>) acc
        let acc' = if bad then acc else (x:acc)
        f acc' xs

-- | Find all elements on the page matching the given sequence of
-- selectors.  (FIXME: think about the [] case again; also in
-- 'findElemsFrom''.)
--
-- See also 'nubElems'.
findElems' :: WebDriver wd => [Selector] -> wd [Element]
findElems'          []      = findElems (ByXPath "//html")
findElems'          (x:xs)  = findElems x           >>= fmap concat . mapM (`findElemsFrom'` xs)

-- | Find all elements matching a selector sequence, using the given
-- element as root.
--
-- See also 'nubElems'.
findElemsFrom' :: WebDriver wd => Element -> [Selector] -> wd [Element]
findElemsFrom' elem []      = return [elem]
findElemsFrom' elem (x:xs)  = findElemsFrom elem x  >>= fmap concat . mapM (`findElemsFrom'` xs)

-- | Like 'findElems'', but raises an error in case of unexpected
-- number of found 'Element's.
--
-- See also 'nubElems'.
findElemsN' :: WebDriver wd => Int -> [Selector] -> wd [Element]
findElemsN' i selectors = do
    es <- findElems' selectors
    if length es == i
        then return es
        else fail $ printf "findElemsN': found %i elements at %s, expected %i\n" (length es) i (show selectors)

-- | Like 'findElemsFrom'', but raises an error in case of unexpected
-- number of found 'Element's.
--
-- See also 'nubElems'.
findElemsFromN' :: WebDriver wd => Int -> Element -> [Selector] -> wd [Element]
findElemsFromN' i element selectors = do
    es <- findElemsFrom' element selectors
    if length es == i
        then return es
        else fail $ printf "findElemsFromN': found %i elements at %s, expected %i\n" (length es) i (show (element, selectors))


-- * an xpath data type

-- http://oreilly.com/perl/excerpts/system-admin-with-perl/ten-minute-xpath-utorial.html

-- | Data type for constructing well-formed XPath expressions.  (Only
-- a sub-language of XPath is supported.)  The many joint lists form a
-- disjunction.
newtype XPath = XPath [[XPathJoint]]
  deriving (Eq, Show, Ord)

data XPathJoint =
    XPathTag ST
  | XPathAny
  | XPathDeepAny
  | XPathAttr ST
  | XPathAttrAny
  | XPathText
  | XPathQualified ST XPathQualify
  deriving (Eq, Show, Ord)

data XPathQualify =
    XPathIx Int
  | XPathLast
  | XPathAttrEq ST ST
  | XPathAttrMatches ST ST
  | XPathAttrContains ST ST
  | XPathTextEq ST
  deriving (Eq, Show, Ord)

-- | This function mimics the (missing) 'ByXPath'' constructor of the
-- 'Selector' type.
byXPath :: XPath -> Selector
byXPath (XPath ors) = byXPath' ors

-- | Variant of 'byXPath'.
byXPath' :: [[XPathJoint]] -> Selector
byXPath' = ByXPath . compileXPath . XPath

-- | Compile a structured 'XPath' expression to a string.  (If
-- 'XPathAttrMatches' is not working for you, you may be using XPath
-- <2.0?)
compileXPath :: XPath -> ST
compileXPath (XPath xpathjoints) = dsj xpathjoints
  where
    dsj :: [[XPathJoint]] -> ST
    dsj = ST.intercalate "|" . map xpj

    xpj :: [XPathJoint] -> ST
    xpj [] = "/"
    xpj xs = xpj' xs

    xpj' :: [XPathJoint] -> ST
    xpj' []                        = ""
    xpj' (XPathTag st:xs)          = "/" <> st                         <> xpj' xs
    xpj' (XPathQualified st q:xs)  = "/" <> st <> "[" <> xpq q <> "]"  <> xpj' xs
    xpj' (XPathAny:xs)             = "/*"                              <> xpj' xs
    xpj' (XPathDeepAny:xs)         = "/"                               <> xpj' xs
    xpj' [XPathAttr st]            = "/@" <> st
    xpj' [XPathAttrAny]            = "/@*"
    xpj' [XPathText]               = "/text()"
    xpj' badpath = error $ "compileXPath: " ++ show badpath

    xpq :: XPathQualify -> ST
    xpq (XPathIx i)             = cs (show i)
    xpq XPathLast               = "last()"
    xpq (XPathAttrEq k v)       = "@" <> k <> "=" <> cs (show v)
    xpq (XPathAttrContains k v) = "contains(@" <> k <> ", " <> cs (show v) <> ")"
    xpq (XPathAttrMatches k v)  = "matches(@" <> k <> ", " <> cs (show v) <> ")"
    xpq (XPathTextEq v)         = "text()=" <> cs (show v)


-- ** some ad-hoc xpath compiler tests

testSuite :: [(XPath, ST)]
testSuite =
    (XPath [[]], "/") :
    (XPath [[XPathTag "blerb"]], "/blerb") :
    (XPath [[XPathAny]], "/*") :
    (XPath [[XPathDeepAny]], "//") :
    (XPath [[XPathAttr "rat"]], "/@rat") :
    (XPath [[XPathAttrAny]], "/@*") :
    (XPath [[XPathText]], "/text()") :
    (XPath [[XPathQualified "a" (XPathIx 3)]], "/a[3]") :
    (XPath [[XPathQualified "a" XPathLast]], "/a[last()]") :
    (XPath [[XPathQualified "a" (XPathAttrEq "fi" "ooph")]], "/a[@fi=\"ooph\"]") :
    (XPath [[XPathQualified "a" (XPathTextEq "ouph")]], "/a[text()=\"ouph\"]") :
    (XPath [[XPathDeepAny, XPathTag "a"]], "//a") :
    (XPath [[XPathTag "b", XPathDeepAny, XPathTag "a"]], "/b//a") :
    (XPath [[XPathDeepAny, XPathTag "a"], [XPathDeepAny, XPathTag "button"]], "//a|//button") :
    []

runTestSuite :: IO ()
runTestSuite = mapM_ (putStrLn . cs . f) testSuite
  where
    f (xpath, st) = let st' = compileXPath xpath in
                    if st' == st then "[ok]    " <> st'
                                 else "***     is:        " <> cs (show st') <> "\n" <>
                                      "        should be: " <> cs (show st)

-- FIXME: still missing: regex matching on attribute values; attribute
-- search without deciding on a tag first.
