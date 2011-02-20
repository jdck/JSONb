


{-# LANGUAGE StandaloneDeriving
  #-}


{-| JSON datatype definition.
 -}


module Text.JSONb.Simple where


import Data.ByteString

import Data.Trie



{-| A monomorphic JSON datatype, backed with 'Rational', strict 'ByteString'
    and 'ByteString' 'Trie'.
 -}
data JSON
  = Object (Trie JSON)
  | KeyValues [(ByteString, JSON)]
  | Array [JSON]
  | String ByteString
  | Number Rational
  | Boolean Bool
  | Null
deriving instance Eq JSON
instance Show JSON where
  show json                  =  case json of
    Object trie             ->  unlines $ "Object" : trie_show trie
    KeyValues keyvals       ->  unlines $ "Object" : keyvals_show keyvals    
    Array list              ->  unlines $ "Array" : fmap show list
    String bytes            ->  unwords ["String", show bytes]
    Number rational         ->  unwords ["Number", show rational]
    Boolean bool            ->  unwords ["Boolean", show bool]
    Null                    ->  "Null"
   where
    trie_show                =  fmap edge_show . toList
    keyvals_show             =  fmap edge_show

    edge_show (k, v)       =  unwords [show k, "->", show v]
