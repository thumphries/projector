-- | Projector build tool.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           BuildInfo_ambiata_projector_html
import           DependencyInfo_ambiata_projector_html

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Options.Applicative  (Parser)

import           P

import           System.IO  (IO)
import qualified System.IO as IO

import           X.Options.Applicative (dispatch, safeCommand, RunType (..), SafeCommand (..))


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  cmd <- dispatch (safeCommand cinemaP)
  case cmd of
    VersionCommand ->
      T.putStrLn ("cinema-" <> (T.pack buildInfoVersion))
    DependencyCommand ->
      traverse_ (T.putStrLn . T.pack) dependencyInfo
    RunCommand DryRun c ->
      IO.print c
    RunCommand RealRun c ->
      run c

run :: () -> IO ()
run =
  pure

cinemaP :: Parser ()
cinemaP =
  pure ()
