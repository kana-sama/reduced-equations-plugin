{-# LANGUAGE LambdaCase #-}
module Plugin where

import GHC.Plugins
import GHC
import Data.Generics.Uniplate.Data qualified as Uniplate
import Debug.Trace qualified as Debug.Trace
import Data.Data (Data)
import GHC.Hs.Dump (showAstDataFull)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Control.Monad.State
import Control.Lens hiding (transform)
import Data.Data.Lens (tinplate)


pretty :: Data a => a -> String
pretty = showSDocUnsafe . showAstDataFull

trace :: Data a => a -> b -> b
trace a b = Debug.Trace.trace (pretty a) b

traceId :: Data a => a -> a
traceId x = trace x x

plugin = defaultPlugin
  { parsedResultAction = \_ _ parsed -> do
      let L l mod = parsed.parsedResultModule.hpm_module
      let mod' = transform mod
      pure parsed{parsedResultModule = parsed.parsedResultModule{hpm_module = L l mod'}}
  , pluginRecompile = purePlugin
  }

getArities :: HsModule GhcPs -> Map RdrName Int
getArities mod = flip execState Map.empty do
  forOf_ tinplate mod \case
    FunBind{fun_id, fun_matches} -> do
      let arity = unLoc fun_matches.mg_alts & unLoc . head & (.m_pats) & length
      at (unLoc fun_id) %= \case
        Nothing -> Just arity
        Just x -> Just (max x arity)
    (_ :: HsBindLR GhcPs GhcPs) -> pure ()

transform :: HsModule GhcPs -> HsModule GhcPs
transform mod = mod &
  Uniplate.transformBi \case
    bind@FunBind{fun_id, fun_matches} -> do
      let arity = arities Map.! unLoc fun_id
      bind{fun_matches = matchGroup arity fun_matches}
    (decl :: HsBindLR GhcPs GhcPs) -> decl
  where
    arities = getArities mod

    matchGroup :: Int -> MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
    matchGroup a group@MG{mg_alts = L l alts} = group{mg_alts = L l [L l (match a m) | L l m <- alts ]}

    match :: Int -> Match GhcPs (LHsExpr GhcPs) -> Match GhcPs (LHsExpr GhcPs)
    match a match
      | a > length match.m_pats =
          let fresh = freshVars (a - length match.m_pats)
           in match{ m_pats = match.m_pats ++ [varP v | v <- fresh]
                   , m_grhss = grhss fresh match.m_grhss }
      | otherwise = match

    grhss :: [RdrName] -> GRHSs GhcPs (LHsExpr GhcPs) -> GRHSs GhcPs (LHsExpr GhcPs)
    grhss fresh grhss = grhss{grhssGRHSs = [L l (grhs fresh g) | L l g <- grhss.grhssGRHSs]}

    grhs :: [RdrName] -> GRHS GhcPs (LHsExpr GhcPs) -> GRHS GhcPs (LHsExpr GhcPs)
    grhs fresh (GRHS ext guards body) =
      GRHS ext guards (foldr (\f b -> appE b (varE f)) body fresh)

varP :: RdrName -> LPat GhcPs
varP v = noLocA (VarPat noExtField (noLocA v))

appE :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
appE a b = noLocA (HsApp noAnn a b)

varE :: RdrName -> LHsExpr GhcPs
varE v = noLocA (HsVar noExtField (noLocA v))

rdr :: String -> RdrName
rdr = mkVarUnqual . mkFastString

freshVars :: Int -> [RdrName]
freshVars n = [rdr ("__fresh_" ++ show i ++ "__") | i <- [0..n-1]]
