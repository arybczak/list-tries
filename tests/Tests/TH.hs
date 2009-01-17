-- File created: 2009-01-09 13:57:13

{-# LANGUAGE EmptyDataDecls, PatternGuards, TemplateHaskell #-}

module Tests.TH
   ( Module(..)
   , TrieType, ListElemType
   , makeFunc, makeCases, makeProps
   , setsOnly, mapsOnly, allTries, mapsOnlyNotEnum, allTriesNotEnum
   ) where

import Control.Arrow ((***))
import Data.Char     (isDigit)
import Data.Maybe    (isJust)
import Data.List     (break, isSuffixOf)
import Language.Haskell.TH
   ( Exp(..), Lit(..), Stmt(..), Dec(..), Type(..), Clause(..), Pat(..)
   , Guard(..), Body(..), Match(..)
   , Q, ExpQ
   , Name, nameBase, mkName
   )

data Module = SetModule String | MapModule String

moduleName :: Module -> String
moduleName (SetModule m) = m
moduleName (MapModule m) = m

data TestType = Case | Property

data ListElemType

data TrieType_ a
type TrieType = TrieType_ Int

keyType  = ''Char
elemType = ''Int

replaceTypes :: Module -> Type -> Type
replaceTypes m (ForallT names cxt t) = ForallT names cxt (replaceTypes m t)
replaceTypes m (AppT t1 t2) = AppT (replaceTypes m t1) (replaceTypes m t2)
replaceTypes m (ConT t) | t == ''TrieType =
   case m of
        SetModule m' -> ConT (mkName $ m' ++ ".TrieSet") `AppT` ConT keyType
        MapModule m' -> ConT (mkName $ m' ++ ".TrieMap") `AppT` ConT keyType
                                                         `AppT` ConT elemType
replaceTypes m (ConT t) | t == ''ListElemType =
   case m of
        SetModule _ -> ListT `AppT` ConT keyType
        MapModule _ -> TupleT 2 `AppT` (ListT `AppT` ConT keyType)
                                `AppT` (ConT elemType)
                                       
replaceTypes _ x = x

-- Given, say:
--    [SetModule "S", MapModule "M"]
--    [("x",Just (AppT (TupleT 2) (ConT Int) (ConT TrieType)))]
--    [d| f x y = x |]
--
-- generate: [d| f_S y = S.x  :: (Int,S.TrieSet Char)
--               f_M y = S2.x :: (Int,M.TrieMap Char Int)
--             |]
--
-- WARNING: shadowing names will break this! For instance the following:
--
--   f x y = let x = y in x
--
-- will result in:
--
--   f_S y = let x = y in S.x
--
-- Which is obviously very different in terms of semantics.
--
-- (Yes, this could be handled properly but I couldn't be bothered.)
makeFunc :: [Module] -> [String] -> Q [Dec] -> Q [Dec]
makeFunc modules expands =
   let expandFuns = map expandTopDec modules
    in fmap (\decs -> concat [map f decs | f <- expandFuns])
 where
   isExpandable n = nameBase n `elem` expands

   expandTopDec modu (FunD name clauses) =
      FunD (modularName (nameBase name) (moduleName modu))
           (map (expandClause modu) clauses)
   expandTopDec _ _ =
      error "expandTopDec :: shouldn't ever see this declaration type"

   expandDec modu (FunD name clauses) =
      FunD name (map (expandClause modu) clauses)
   expandDec modu (ValD pat body decs) =
      ValD pat (expandBody modu body) (map (expandDec modu) decs)
   expandDec _ x@(SigD _ _) = x
   expandDec _ _ =
      error "expandDec :: shouldn't ever see this declaration type"

   expandClause modu (Clause pats body decs) =
      Clause (concatMap clearPat pats)
             (expandBody modu body)
             (map (expandDec modu) decs)

   -- Remove matching ones from the function arguments
   clearPat (VarP n) | isExpandable n = []
   clearPat x = [x]

   expandBody modu (NormalB expr)    = NormalB (expandE modu expr)
   expandBody modu (GuardedB guards) =
      GuardedB (map (expandGuard modu *** expandE modu) guards)

   expandE m (VarE n) | isExpandable n = qualify VarE m n
   expandE m (ConE n) | isExpandable n = qualify ConE m n
   expandE m (AppE e1 e2)         = AppE (expandE m e1) (expandE m e2)
   expandE m (InfixE me1 e me2)   = InfixE (fmap (expandE m) me1)
                                           (expandE m e)
                                           (fmap (expandE m) me2)
   expandE m (LamE pats e)        = LamE pats (expandE m e)
   expandE m (TupE es)            = TupE (map (expandE m) es)
   expandE m (CondE e1 e2 e3)     = CondE (expandE m e1)
                                          (expandE m e2)
                                          (expandE m e3)
   expandE m (LetE decs e)        = LetE (map (expandDec m) decs) (expandE m e)
   expandE m (CaseE e matches)    = CaseE (expandE m e)
                                          (map (expandMatch m) matches)
   expandE m (DoE stmts)          = DoE (map (expandStmt m) stmts)
   expandE m (CompE stmts)        = CompE (map (expandStmt m) stmts)
   expandE m (SigE e t)           = SigE (expandE m e) (replaceTypes m t)
   expandE m (RecConE name fexps) = RecConE name (map (expandFieldExp m) fexps)
   expandE m (RecUpdE name fexps) = RecUpdE name (map (expandFieldExp m) fexps)
   expandE _ x = x

   qualify expr modu name =
      expr $ mkName (moduleName modu ++ "." ++ nameBase name)

   expandMatch modu (Match pat body decs) =
      Match pat (expandBody modu body) (map (expandDec modu) decs)

   expandStmt modu (BindS pat expr) = BindS pat (expandE modu expr)
   expandStmt modu (LetS decs)      = LetS (map (expandDec modu) decs)
   expandStmt modu (NoBindS expr)   = NoBindS (expandE modu expr)
   expandStmt _    (ParS _)         = error "expandStmt :: ParS? What's that?"

   expandFieldExp modu (name,expr) = (name, expandE modu expr)

   expandGuard modu (NormalG expr) = NormalG (expandE modu expr)
   expandGuard modu (PatG stmts)   = PatG (map (expandStmt modu) stmts)

makeTests :: TestType -> [Module] -> String -> ExpQ
makeTests typ modules test =
   return$
      VarE (mkName "testGroup") `AppE`
      LitE (StringL (testName typ test)) `AppE`
      ListE (
         map (\m -> let mn = moduleName m
                        n  = modularName test mn
                     in VarE (mkName "testProperty") `AppE`
                        LitE (StringL (relevantPart mn)) `AppE`
                        VarE n)
             modules)

makeCases = makeTests Case
makeProps = makeTests Property                           

-- Used to name the generated functions
modularName :: String -> String -> Name
modularName name modu =
   mkName $ name ++ "_" ++ map (\c -> if c == '.' then '_' else c) modu

testName :: TestType -> String -> String
testName Case test = test
testName Property test =
   let (s,num) = break isDigit.tail.dropWhile (/= '_') $ test
    in concat
          [ s
          , if null num
               then ""
               else "-"
          , num
          ]

relevantPart :: String -> String
relevantPart = drop (length "Data.Trie.")

setsOnly = [SetModule "Data.Trie.Set.Eq"
           ,SetModule "Data.Trie.Set.Ord"
           ,SetModule "Data.Trie.Set.Enum"
           ,SetModule "Data.Trie.Patricia.Set.Eq"
           ,SetModule "Data.Trie.Patricia.Set.Ord"
           ,SetModule "Data.Trie.Patricia.Set.Enum"
           ]
mapsOnly = [MapModule "Data.Trie.Map.Eq"
           ,MapModule "Data.Trie.Map.Ord"
           ,MapModule "Data.Trie.Map.Enum"
           ,MapModule "Data.Trie.Patricia.Map.Eq"
           ,MapModule "Data.Trie.Patricia.Map.Ord"
           ,MapModule "Data.Trie.Patricia.Map.Enum"
           ]
allTries = setsOnly ++ mapsOnly

-- FIXME: remove these as soon as there are no more undefined in Base.Map
mapsOnlyNotEnum = filter (not.isSuffixOf "Enum".moduleName) mapsOnly
allTriesNotEnum = filter (not.isSuffixOf "Enum".moduleName) allTries
