{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module: Database.Persist.Fixtures
License: MIT
Maintainer: me@joelt.io
Stability: experimental
Portability: GHC

Convenience functions for generating persistent fixtures.

These functions require @-XFlexibleContexts@.
-}

module Database.Persist.Fixtures (
    genFixtures,
    genFixturesFrom
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

-- | Generates fixture-loading functions. The entity type passed to this
-- function must be an instance of 'FromJSON' (and, obviously,
-- 'PersistEntity').
--
-- The default file from which fixtures are loaded is
-- @.\/fixtures\/[table_name].yml@. If you wish to specify the file
-- yourself, use 'genFixturesFrom'.
--
-- The first argument to @genFixtures@ is the name of the datatype you wish
-- to generate fixtures for. The second argument is the name of the
-- function (which must be in scope at the splice point) which executes
-- your database queries in the parent monad.
--
-- For example, the code
--
-- @genFixtures \"User\" "runDB"@
--
-- will define two new functions:
--
-- @withUserFixtures :: ('Monad' m, 'MonadBaseControl' 'IO' m, 'MonadThrow' m, 'MonadIO' m) => (User -> 'Bool') -> (['Entity' User] -> m b) -> m b@
--
-- @withUserFixtures filter action@ runs @action@, having loaded all of the
-- entities which satisfy @filter@. @action@ is passed a list of the
-- inserted entities. The entities will be deleted after @action@
-- completes, even if it throws an exception.
--
-- @withAllUserFixtures :: ('Monad' m, 'MonadBaseControl' 'IO' m, 'MonadThrow' m, 'MonadIO' m) => (['Entity' User] -> m b) -> m b@
--
-- @withAllUserFixtures@ is just @withUserFixtures (const True)@; it loads
-- all the defined fixtures.
--
-- A trivial example of the usage of this function might be:
--
-- @
-- genFixtures \"User\" \"runDB\"
--
-- runDB act = runSqlite ":memory:" (runMigrate migrateAll >> act)
--
-- main :: IO ()
-- main = do
--     withAllUserFixtures $ \\ (user : _) -> do
--         putStr "The first user's name is: "
--         print (userName $ entityVal user)
--     withUserFixtures (\\ user -> head (userName user) == \'H\') $ \\ users -> do
--         putStr "These users' names all begin with H: "
--         print users
-- @
genFixtures :: String -> String -> Q [Dec]
genFixtures name runner = do
    let filename = psToDBName lowerCaseSettings (pack name)
        fp = "fixtures" </> unpack filename <.> "yml"
    genFixturesFrom fp name runner

-- | Like 'genFixtures', but takes as an argument the filepath from which
-- the fixtures should be loaded. The file should be a YAML file
-- representing a list of entities.
genFixturesFrom :: FilePath -> String -> String -> Q [Dec]
genFixturesFrom fp name' runner' = do
    let name = mkName name'
        runner = mkName runner'
        loadAllName = mkName $ printf "withAll%sFixtures" name'
        loadSomeName = mkName $ printf "with%sFixtures" name'
    requireInstance name ''FromJSON
    requireInstance name ''PersistEntity
    contents <- runIO $ readFile fp
        `catchIOError` (\ e -> error $ "Unable to open fixtures file: " ++ show e)
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
                  => ($(conT name) -> Bool) -> ([Entity $(conT name)] -> m b) -> m b |]
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