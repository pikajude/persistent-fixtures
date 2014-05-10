{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Resource
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

runner m = runSqlite ":memory:" (runMigration migrateAll >> m)

genFixturesFrom "tests/fixtures/user.yml" "User" "runner"

main :: IO ()
main = do
    cs <- runTestTT $ TestList
        [TestLabel "all" $ TestCase $ withAllUserFixtures $ \ ks ->
            assertBool "length" $ length ks == 2
        , TestLabel "filtering" $ TestCase $ withUserFixtures userAlive $ \ us ->
            assertBool "alive" (all (userAlive . entityVal) us)
        ]
    when (errors cs > 0 || failures cs > 0) $ do
        print cs
        exitFailure
