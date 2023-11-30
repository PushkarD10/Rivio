{-# LANGUAGE QuasiQuotes #-}

module Alchemist.App where

import Alchemist.DSL.Parser.API
import Alchemist.DSL.Parser.Storage
import Alchemist.DSL.Syntax.API
import Alchemist.DSL.Syntax.Storage
import Alchemist.Generator.Haskell
import Alchemist.Generator.SQL
import Alchemist.Utils
import qualified Data.Text as T
import Kernel.Prelude

mkBeamTable :: FilePath -> FilePath -> IO ()
mkBeamTable filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile (filePath ++ "/" ++ tableNameHaskell t ++ ".hs") (generateBeamTable t)) tableDef

mkBeamQueries :: FilePath -> FilePath -> IO ()
mkBeamQueries filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile (filePath ++ "/" ++ tableNameHaskell t ++ ".hs") (generateBeamQueries t)) tableDef

mkDomainType :: FilePath -> FilePath -> IO ()
mkDomainType filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile (filePath ++ "/" ++ tableNameHaskell t ++ ".hs") (generateDomainType t)) tableDef

mkSQLFile :: FilePath -> FilePath -> IO ()
mkSQLFile filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile (filePath ++ "/" ++ tableNameSql t ++ ".sql") (generateSQL t)) tableDef

mkServantAPI :: FilePath -> FilePath -> IO ()
mkServantAPI filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile (filePath ++ "/" ++ T.unpack (_moduleName apiDef) ++ ".hs") (generateServantAPI apiDef)

mkDomainHandler :: FilePath -> FilePath -> IO ()
mkDomainHandler filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile (filePath ++ "/" ++ T.unpack (_moduleName apiDef) ++ ".hs") (generateDomainHandler apiDef)
