module TcRwLocs (tcRwLocs) where

import HsSyn
import Annotations
import Name
import TcRnMonad
import SrcLoc
import Outputable
import FastString
import TcEnv
import Id
import Type
import Control.Monad (unless)
import TysWiredIn
import Kind

tcRwLocs :: [LRwLocDecl Name] -> TcM ()
tcRwLocs = mapM_ tcRwLoc

tcRwLoc :: LRwLocDecl Name -> TcM ()
tcRwLoc rwloc@(L loc (RewriteLoc name target)) = setSrcSpan loc $ addErrCtxt errCtxt $ do
  exp <- addLocationType <$> lookupType name
  act <- lookupType target
  let msg = vcat [ text "Expected type:" <+> ppr exp
                 , text "  Actual type:" <+> ppr act ]
  unless (exp `eqType` act) $ do
    addErr (hang (ptext (sLit "Invalid type for") <+> ppr target) 2 msg)
  where
    errCtxt = hang (ptext (sLit "In the pragma")) 2 (ppr rwloc)

lookupType :: Name -> TcM Type
lookupType name = tcLookup name >>= \thing -> case thing of
  AGlobal (AnId id_) -> return (idType id_)
  ATcId {tct_id = id_} -> return (idType id_)
  _ -> failWith (ppr thing <+> ptext (sLit "not allowed here!"))

addLocationType :: Type -> Type
addLocationType t = case splitForAllTy_maybe t of
  -- keep all foralls and constraints at the beginning
  Just (var, t_) -> mkForAllTy var (addLocationType t_)
  Nothing -> case splitFunTy_maybe t of
    Just (t1, t2) | isConstraintKind (typeKind t1) -> mkFunTy t1 (addLocationType t2)
    _ -> mkFunTy stringTy t
