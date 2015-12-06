{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverlappingInstances, TypeSynonymInstances, UndecidableInstances, TypeFamilies #-}
module HJScript.XMLGenerator (
--        ToChildNodes(..), ToAttributeNode(..),

        XML, Child, Attribute, genElement, genEElement, asChild, asAttr, fromStringLit, Attr(..)
        ) where

--import qualified HSX.XMLGenerator as HSX (XMLGen(..))
import qualified Data.Text.Lazy as TL
import HSP.XMLGenerator

import HJScript.Monad
import HJScript.Lang

import HJScript.DOM.Node
import HJScript.DOM.AttributeNode
import HJScript.DOM.ElementNode
import HJScript.DOM.TextNode
import HJScript.DOM.Document

type XML   = Exp ElementNode
type Child = Exp Node
type Attribute = Exp AttributeNode

instance XMLGen HJScript' where
 type XMLType          HJScript' = XML
 type StringType       HJScript' = String
 newtype ChildType     HJScript' = HJSChild Child
 newtype AttributeType HJScript' = HJSAttr Attribute
 genElement = element
 genEElement = eElement
 xmlToChild = HJSChild . castToNode
 pcdataToChild str = HJSChild . castToNode $ document # createTextNode (string str)

fromStringLit :: String -> String
fromStringLit = id

element :: (EmbedAsChild HJScript' c,
            EmbedAsAttr HJScript' a)
            => Name String -> [a] -> [c] -> HJScript XML
element (ns, ln) atts xmls = do
  let name = (maybe id (\x y -> y ++ ':':x) ns) ln
  elem <- fmap val $ varWith $ document # createElement (string name)
  cxml <- fmap concat $ mapM asChild xmls
  ats  <- fmap concat $ mapM asAttr atts
  mapM (\attr  -> elem # setAttributeNode attr) $ map stripAttr ats
  mapM (\child -> elem # appendChild child) $ map stripChild cxml
  return elem

eElement :: EmbedAsAttr HJScript' a => Name String -> [a] -> HJScript XML
eElement n attrs = element n attrs ([] :: [Child])
{-
instance EmbedAsAttr HJScript' (Attr TL.Text TL.Text) where
    asAttr (n := v) = asAttr (TL.unpack n := TL.unpack v)
-}
instance EmbedAsChild HJScript' TL.Text where
    asChild t = asChild (TL.unpack t)

instance XMLGenerator HJScript'

--------------------------------------------
-- EmbedAsChild and EmbedAsAttr

instance EmbedAsChild HJScript' Child where
 asChild = asChild . HJSChild

instance EmbedAsChild HJScript' JString where
 asChild jstr = asChild $ castToNode $ document # createTextNode jstr

--instance EmbedAsChild HJScript' String where
-- asChild = asChild . string

instance EmbedAsChild HJScript' Char where
 asChild = asChild . (:[])

-- This instance should already be there, probably doesn't work due
-- to type families not being fully supported yet.
instance EmbedAsChild HJScript' XML where
 asChild = return . return . xmlToChild

instance EmbedAsAttr HJScript' Attribute where
 asAttr = asAttr . HJSAttr

instance (IsAttrNodeValue a, IsName n TL.Text) => EmbedAsAttr HJScript' (Attr n a) where
 asAttr (k := a) = asAttr $ do
    let (ns, ln) = toName k
        name = (maybe id (\x y -> y ++ ':':x) (fmap TL.unpack ns)) (TL.unpack ln)
    v <- toAttrNodeValue a
    an <- inVar $ document # createAttribute (string name)
    an # value .=. v
    return an


class IsAttrNodeValue a where
 toAttrNodeValue :: a -> HJScript JString

instance JShow a => IsAttrNodeValue a where
    toAttrNodeValue = return . jshow

instance IsAttrNodeValue TL.Text where
    toAttrNodeValue = return . jshow . TL.unpack


instance IsAttrNodeValue a => IsAttrNodeValue (HJScript a) where
 toAttrNodeValue = (>>= toAttrNodeValue)

-----------------------------------
-- SetAttr and AppendChild.

instance SetAttr HJScript' XML where
 setAll en ats = do
        ev <- inVar en
        as <- ats
        mapM (\attr -> ev # setAttributeNode attr) (map stripAttr as)
        return ev

instance AppendChild HJScript' XML where
 appAll en cns = do
        ev <- inVar en
        cs <- cns
        mapM (\child -> ev # appendChild child) (map stripChild cs)
        return ev

stripAttr  (HJSAttr  a) = a
stripChild (HJSChild c) = c
