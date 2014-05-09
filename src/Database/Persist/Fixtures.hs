{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Fixtures (
    genFixtures,
    genFixtureFrom
) where

import Control.Exception.Lifted
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Data.ByteString as B
import Data.String
import Data.Text (pack, unpack)
import Data.Yaml
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sql
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.FilePath
import System.IO.Error
import Text.Printf

{-
- genFixtures ''Team ''runDB
-
- withAllTeamFixtures $ do
-
- withTeamFixtures (\t -> teamDivision t == 1) $ do
-}

genFixtures :: String -> String -> Q [Dec]
genFixtures name' runner' = do
    let name = mkName name'
        runner = mkName runner'
        filename = psToDBName lowerCaseSettings (pack name')
        fp = "fixtures" </> unpack filename <.> "yml"
    genFixtureFrom fp name runner

genFixtureFrom :: FilePath -> Name -> Name -> Q [Dec]
genFixtureFrom fp name runner = do
    let loadAllName = mkName $ printf "withAll%sFixtures" (unName name)
        loadSomeName = mkName $ printf "with%sFixtures" (unName name)
    requireInstance name ''FromJSON
    requireInstance name ''PersistEntity
    contents <- runIO $ readFile fp
        `catchIOError` (\e -> error $ "Unable to open fixtures file: " ++ show e)
    let parseAndInsert =
            [| \ fltr action -> do
            let entities = either error (filter fltr)
                         $ decodeEither (fromString $(stringE contents))
            keys <- $(varE runner) $ mapM insert entities
            action (zipWith Entity keys entities) `finally`
                ($(varE runner) $ deleteWhere ([] :: [Filter $(conT name)])) |]
    someFun <- funD loadSomeName [clause [] (normalB parseAndInsert) []]
    someTy <- sigD loadSomeName
              [t| (Monad m, MonadBaseControl IO m, MonadThrow m, MonadIO m)
                  => ($(conT name) -> Bool) -> ([Entity $(conT name)] -> m b) -> m b|]
    fun <- funD loadAllName
        [clause [] (normalB [| $(varE loadSomeName) (const True) |]) []]
    ty <- sigD loadAllName
              [t| (Monad m, MonadBaseControl IO m, MonadThrow m, MonadIO m)
                  => ([Entity $(conT name)] -> m b) -> m b |]
    return [someTy, someFun, ty, fun]
    where
        requireInstance n c = do
            ins <- reifyInstances c [ConT n]
            case ins of
                [] -> error $
                    printf "To be used in fixtures, `%s' must be an instance of %s."
                                  (unName n) (unName c)
                _ -> return ()

unName :: Name -> String
unName (Name (OccName s) _) = s
