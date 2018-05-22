{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (return)
import Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, newEitherT)
import Control.Monad.Trans.Either.Exit (orDie)
import Data.Bifunctor (first)
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (maximum, minimum, zip)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Traversable (traverse)
import GHC.Float (Double)
import Prelude ((-), (/))
import System.IO (
    FilePath
  , IO
  , IOMode(ReadMode, WriteMode)
  , withFile
  )
import System.IO.Error (
    isAlreadyInUseError
  , isDoesNotExistError
  , isPermissionError
  )
import Text.Show (show)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

main :: IO ()
main = orDie renderAppError $ do
  contents <- firstEitherT AppFileError $ readFile "input"
  nums <- hoistEither $ first AppLineError $ parseLines contents
  normalised <- hoistEither $ first AppNormaliseError $ normalise nums
  firstEitherT AppFileError $ writeFile "output" $ renderDoubles normalised

renderDoubles :: [Double] -> Text
renderDoubles ns = T.unlines (T.pack . show <$> ns)

data AppError
  = AppFileError FileError
  | AppLineError LineError
  | AppNormaliseError NormaliseError

renderAppError :: AppError -> Text
renderAppError (AppFileError e) = renderFileError e
renderAppError (AppLineError e) = renderLineError e
renderAppError (AppNormaliseError e) = renderNormaliseError e

data NormaliseError
  = EmptyList
  | SingletonList
  | ZeroRange

normalise :: [Double] -> Either NormaliseError [Double]
normalise [] = Left EmptyList
normalise [_] = Left SingletonList
normalise ns =
  let max = maximum ns
      min = minimum ns
      range = max - min
      scale n = (n - min) / range
  in  if range == 0
         then Left ZeroRange
         else Right $ scale <$> ns

renderNormaliseError :: NormaliseError -> Text
renderNormaliseError EmptyList = "Cannot normalise an empty list."
renderNormaliseError SingletonList = "Cannot normalise a list with one value."
renderNormaliseError ZeroRange = "Cannot normalise when all values are the same."

readFile:: FilePath -> EitherT FileError IO Text
readFile filePath =
  let getFileContents = do
        contents <- withFile filePath ReadMode T.hGetContents
        return $ Right contents

      handleError e = do
        error <- selectFileError e
        return $ Left (ReadFileError filePath error)

  in  newEitherT $ catch getFileContents handleError

writeFile :: FilePath -> Text -> EitherT FileError IO ()
writeFile filePath contents =
  let setFileContents = do
        withFile filePath WriteMode (\h -> T.hPutStr h contents)
        return $ Right ()

      handleError e = do
        error <- selectFileError e
        return $ Left (WriteFileError filePath error)

  in  newEitherT $ catch setFileContents handleError

data FileErrorType
  = AlreadyInUse
  | DoesNotExist
  | PermissionError

renderFileErrorType :: FileErrorType -> Text
renderFileErrorType AlreadyInUse = "Already in use."
renderFileErrorType DoesNotExist = "Does not exist."
renderFileErrorType PermissionError = "Permission error."

data FileError
  = ReadFileError FilePath FileErrorType
  | WriteFileError FilePath FileErrorType

renderFileError :: FileError -> Text
renderFileError (ReadFileError path err) =
  "Error reading file (" <> T.pack path <> "): " <> renderFileErrorType err
renderFileError (WriteFileError path err) =
  "Error writing to file (" <> T.pack path <> "): " <> renderFileErrorType err

selectFileError :: IOException -> IO FileErrorType
selectFileError e | isAlreadyInUseError e = return AlreadyInUse
                  | isDoesNotExistError e = return DoesNotExist
                  | isPermissionError e   = return PermissionError
                  | otherwise             = throwIO e

parseDouble :: Text -> Either ParseError Double
parseDouble t =
  case T.rational t of
    Right (a, "") -> Right a
    Right (_, t') -> Left $ InputRemaining t'
    Left _        -> Left $ InvalidNumber t

data ParseError
  = InputRemaining Text
  | InvalidNumber Text

renderParseError :: ParseError -> Text
renderParseError (InputRemaining t) = "Some input was not consumed: " <> t
renderParseError (InvalidNumber t)  = "Could not parse number from text: " <> t

parseLines :: Text -> Either LineError [Double]
parseLines t =
  let lines = T.lines t
      lineNumbers = LineNumber <$> [1..]
      parseLine (n, line) = first (InvalidLine n) (parseDouble line)
  in  traverse parseLine (lineNumbers `zip` lines)

newtype LineNumber = LineNumber Int

renderLineNumber :: LineNumber -> Text
renderLineNumber (LineNumber n) = T.pack . show $ n

data LineError
  = InvalidLine LineNumber ParseError

renderLineError :: LineError -> Text
renderLineError (InvalidLine n e) =
  "Error on line " <> renderLineNumber n <> ": " <> renderParseError e
