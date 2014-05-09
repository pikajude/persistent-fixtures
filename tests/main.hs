{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Database.Persist.Fixtures
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Exit
import Test.HUnit

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    alive Bool
|]

instance FromJSON User where
    parseJSON (Object v) =
        User <$> v .: "name"
             <*> v .: "alive"

    parseJSON _ = mzero

genFixturesFrom "tests/fixtures/user.yml" "User" "runner"

runner m = runSqlite ":memory:" (runMigration migrateAll >> m)

main :: IO ()
main = do
    counts <- runTestTT $ TestList
        [TestLabel "all" $ TestCase $ withAllUserFixtures $ \ ks ->
            assertBool "length" $ length ks == 2
        , TestLabel "filtering" $ TestCase $ withUserFixtures userAlive $ \ us ->
            assertBool "alive" (all (userAlive . entityVal) us)
        ]
    when (errors counts > 0 || failures counts > 0) $ do
        print counts
        exitFailure
