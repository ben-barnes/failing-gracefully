{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Control.Exception (throwIO)
import Control.Monad (return)
import Control.Monad.Catch (Handler(Handler))
import Control.Monad.Trans.Either (EitherT, handlesEitherT, runEitherT)
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Text (Text)
import System.IO (FilePath, IO, putStrLn)
import System.IO.Error (
    isAlreadyInUseError
  , isDoesNotExistError
  , isPermissionError
  )

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  res <- runEitherT (readFile "foo.txt")
  case res of
    Right c -> T.putStr c
    Left e  -> T.putStrLn (renderOpenFileError e)

data FileError
  = AlreadyInUse
  | DoesNotExist
  | PermissionError

renderFileError :: FileError -> Text
renderFileError AlreadyInUse = "Already in use."
renderFileError DoesNotExist = "Does not exist."
renderFileError PermissionError = "Permission error."

data OpenFileError = OpenFileError FilePath FileError

renderOpenFileError :: OpenFileError -> Text
renderOpenFileError (OpenFileError path err) =
  "Error opening file (" <> T.pack path <> "): " <> renderFileError err

fileErrorHandler :: Handler IO FileError
fileErrorHandler =
  let selectException e | isAlreadyInUseError e = return AlreadyInUse
                        | isDoesNotExistError e = return DoesNotExist
                        | isPermissionError e   = return PermissionError
                        | otherwise = throwIO e
  in  Handler selectException

readFile :: FilePath -> EitherT OpenFileError IO Text
readFile filePath =
  let liftError err = OpenFileError filePath <$> err
  in  handlesEitherT [liftError fileErrorHandler] (T.readFile filePath)
