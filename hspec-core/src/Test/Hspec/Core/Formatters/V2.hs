{-# LANGUAGE CPP #-}
-- |
-- Stability: experimental
--
-- This module contains formatters that can be used with
-- `Test.Hspec.Core.Runner.hspecWith`.
module Test.Hspec.Core.Formatters.V2 (

-- * Formatters
  silent
, checks
, specdoc
, progress
, failed_examples

-- * Implementing a custom Formatter
-- |
-- A formatter is a set of actions.  Each action is evaluated when a certain
-- situation is encountered during a test run.
--
-- Actions live in the `FormatM` monad.  It provides access to the runner state
-- and primitives for appending to the generated report.
, Formatter (..)
, Item(..)
, Result(..)
, FailureReason (..)
, FormatM
, formatterToFormat

-- ** Accessing the runner state
, getSuccessCount
, getPendingCount
, getFailCount
, getTotalCount

, FailureRecord (..)
, getFailMessages
, usedSeed

, printTimes

, Seconds(..)
, getCPUTime
, getRealTime

-- ** Appending to the generated report
, write
, writeLine
, writeTransient

-- ** Dealing with colors
, withInfoColor
, withSuccessColor
, withPendingColor
, withFailColor

, useDiff
, extraChunk
, missingChunk

-- ** Helpers
, formatLocation
, formatException

#ifdef TEST
, Chunk(..)
, ColorChunk(..)
, indentChunks
#endif
) where

import           Data.Char
import           Prelude ()
import           Test.Hspec.Core.Compat hiding (First)

import           Data.Maybe
import           Test.Hspec.Core.Util
import           Test.Hspec.Core.Clock
import           Test.Hspec.Core.Spec (Location(..))
import           Text.Printf
import           Control.Monad.IO.Class
import           Control.Exception

-- We use an explicit import list for "Test.Hspec.Formatters.Monad", to make
-- sure, that we only use the public API to implement formatters.
--
-- Everything imported here has to be re-exported, so that users can implement
-- their own formatters.
import Test.Hspec.Core.Formatters.Monad (
    Formatter (..)
  , Item(..)
  , Result(..)
  , FailureReason (..)
  , FormatM

  , getSuccessCount
  , getPendingCount
  , getFailCount
  , getTotalCount

  , FailureRecord (..)
  , getFailMessages
  , usedSeed

  , printTimes
  , getCPUTime
  , getRealTime

  , write
  , writeLine
  , writeTransient

  , withInfoColor
  , withSuccessColor
  , withPendingColor
  , withFailColor

  , useDiff
  , extraChunk
  , missingChunk
  )

import           Test.Hspec.Core.Formatters.Internal (formatterToFormat)
import           Test.Hspec.Core.Formatters.Diff

silent :: Formatter
silent = Formatter {
  formatterStarted      = return ()
, formatterGroupStarted = \ _ -> return ()
, formatterGroupDone    = \ _ -> return ()
, formatterProgress     = \ _ _ -> return ()
, formatterItemStarted  = \ _ -> return ()
, formatterItemDone     = \ _ _ -> return ()
, formatterDone         = return ()
}

checks :: Formatter
checks = specdoc {
  formatterProgress = \(nesting, requirement) p -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [" ++ (formatProgress p) ++ "]"

, formatterItemStarted = \(nesting, requirement) -> do
    writeTransient $ indentationFor nesting ++ requirement ++ " [ ]"

, formatterItemDone = \ (nesting, requirement) item -> do
    uncurry (writeResult nesting requirement (itemDuration item) (itemInfo item)) $ case itemResult item of
      Success {} -> (withSuccessColor, "✔")
      Pending {} -> (withPendingColor, "‐")
      Failure {} -> (withFailColor, "✘")
    case itemResult item of
      Success {} -> return ()
      Failure {} -> return ()
      Pending _ reason -> withPendingColor $ do
        writeLine $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason
} where
    indentationFor nesting = replicate (length nesting * 2) ' '

    writeResult :: [String] -> String -> Seconds -> String -> (FormatM () -> FormatM ()) -> String -> FormatM ()
    writeResult nesting requirement duration info withColor symbol = do
      shouldPrintTimes <- printTimes
      write $ indentationFor nesting ++ requirement ++ " ["
      withColor $ write symbol
      writeLine $ "]" ++ if shouldPrintTimes then times else ""
      forM_ (lines info) $ \ s ->
        writeLine $ indentationFor ("" : nesting) ++ s
      where
        dt :: Int
        dt = toMilliseconds duration

        times
          | dt == 0 = ""
          | otherwise = " (" ++ show dt ++ "ms)"

    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total

specdoc :: Formatter
specdoc = silent {

  formatterStarted = do
    writeLine ""

, formatterGroupStarted = \ (nesting, name) -> do
    writeLine (indentationFor nesting ++ name)

, formatterProgress = \_ p -> do
    writeTransient (formatProgress p)

, formatterItemDone = \(nesting, requirement) item -> do
    let duration = itemDuration item
        info = itemInfo item

    case itemResult item of
      Success -> withSuccessColor $ do
        writeResult nesting requirement duration info
      Pending _ reason -> withPendingColor $ do
        writeResult nesting requirement duration info
        writeLine $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason
      Failure {} -> withFailColor $ do
        n <- getFailCount
        writeResult nesting (requirement ++ " FAILED [" ++ show n ++ "]") duration info

, formatterDone = defaultFailedFormatter >> defaultFooter
} where
    indentationFor nesting = replicate (length nesting * 2) ' '

    writeResult nesting requirement (Seconds duration) info = do
      shouldPrintTimes <- printTimes
      writeLine $ indentationFor nesting ++ requirement ++ if shouldPrintTimes then times else ""
      forM_ (lines info) $ \ s ->
        writeLine $ indentationFor ("" : nesting) ++ s
      where
        dt :: Int
        dt = floor (duration * 1000)

        times
          | dt == 0 = ""
          | otherwise = " (" ++ show dt ++ "ms)"

    formatProgress (current, total)
      | total == 0 = show current
      | otherwise  = show current ++ "/" ++ show total

progress :: Formatter
progress = failed_examples {
  formatterItemDone = \ _ item -> case itemResult item of
    Success{} -> withSuccessColor $ write "."
    Pending{} -> withPendingColor $ write "."
    Failure{} -> withFailColor $ write "F"
}

failed_examples :: Formatter
failed_examples   = silent {
  formatterDone = defaultFailedFormatter >> defaultFooter
}

defaultFailedFormatter :: FormatM ()
defaultFailedFormatter = do
  writeLine ""

  failures <- getFailMessages

  unless (null failures) $ do
    writeLine "Failures:"
    writeLine ""

    forM_ (zip [1..] failures) $ \x -> do
      formatFailure x
      writeLine ""

    write "Randomized with seed " >> usedSeed >>= writeLine . show
    writeLine ""
  where
    formatFailure :: (Int, FailureRecord) -> FormatM ()
    formatFailure (n, FailureRecord mLoc path reason) = do
      forM_ mLoc $ \loc -> do
        withInfoColor $ writeLine ("  " ++ formatLocation loc)
      write ("  " ++ show n ++ ") ")
      writeLine (formatRequirement path)
      case reason of
        NoReason -> return ()
        Reason err -> withFailColor $ indent err
        ExpectedButGot preface expected actual -> do
          mapM_ indent preface

          b <- useDiff

          let threshold = 2 :: Seconds

          mchunks <- liftIO $ if b
            then timeout threshold (evaluate $ smartDiff expected actual)
            else return Nothing

          case mchunks of
            Just chunks -> do
              writeDiff chunks extraChunk missingChunk
            Nothing -> do
              writeDiff [First expected, Second actual] write write
          where
            writeDiff chunks extra missing = do
              writeChunks "expected: " (expectedChunks chunks) extra
              writeChunks " but got: " (actualChunks chunks) missing

            writeChunks :: String -> [Chunk] -> (String -> FormatM ()) -> FormatM ()
            writeChunks pre chunks colorize = do
              withFailColor $ write (indentation ++ pre)
              forM_ (indentChunks indentation_ chunks) $ \ chunk -> case chunk of
                PlainChunk a -> write a
                ColorChunk a -> colorize a
              writeLine ""
              where
                indentation_ = indentation ++ replicate (length pre) ' '

        Error _ e -> withFailColor . indent $ (("uncaught exception: " ++) . formatException) e

      writeLine ""
      writeLine ("  To rerun use: --match " ++ show (joinPath path))
      where
        indentation = "       "
        indent message = do
          forM_ (lines message) $ \line -> do
            writeLine (indentation ++ line)

data Chunk = Original String | Modified String
  deriving (Eq, Show)

expectedChunks :: [Diff] -> [Chunk]
expectedChunks = mapMaybe $ \ chunk -> case chunk of
  Both a -> Just $ Original a
  First a -> Just $ Modified a
  Second _ -> Nothing

actualChunks :: [Diff] -> [Chunk]
actualChunks = mapMaybe $ \ chunk -> case chunk of
  Both a -> Just $ Original a
  First _ -> Nothing
  Second a -> Just $ Modified a

data ColorChunk = PlainChunk String | ColorChunk String
  deriving (Eq, Show)

simplify :: [ColorChunk] -> [ColorChunk]
simplify input = case input of
  PlainChunk "" : xs -> simplify xs
  PlainChunk xs : PlainChunk ys : zs -> simplify $ PlainChunk (xs ++ ys) : zs
  x : xs -> x : simplify xs
  [] -> []

addIsLast :: [a] -> [(a, Bool)]
addIsLast input = case input of
  [] -> []
  [x] -> [(x, True)]
  x : xs -> (x, False) : addIsLast xs

indentChunks :: String -> [Chunk] -> [ColorChunk]
indentChunks indentation = simplify . concatMap indented_ . addIsLast
  where
    indented_ :: (Chunk, Bool) -> [ColorChunk]
    indented_ (x, isLast) = case x of
      Original y -> [indentOriginal indentation y]
      Modified y -> indentModified isLast indentation y

indentOriginal :: String -> String -> ColorChunk
indentOriginal indentation = PlainChunk . go
  where
    go text = case break (== '\n') text of
      (xs, _ : ys) -> xs ++ "\n" ++ indentation ++ go ys
      (xs, _) -> xs

indentModified :: Bool -> String -> String -> [ColorChunk]
indentModified isLast indentation input = go input
  where
    go text = case break (== '\n') text of
      ("", "") -> []
      (xs, _ : ys) -> segment xs ++ PlainChunk "\n" : PlainChunk indentation : go ys
      (xs, "") -> lastSegment xs

    segment xs = case span isSpace $ reverse xs of
      ("", _) -> [ColorChunk xs]
      (_, "") -> [ColorChunk xs]
      (ys, zs) -> [ColorChunk (reverse zs), ColorChunk (reverse ys)]

    lastSegment xs
      | all isSpace input = [ColorChunk xs]
      | all (== ' ') xs && not isLast = [PlainChunk xs]
      | otherwise = segment xs

defaultFooter :: FormatM ()
defaultFooter = do

  writeLine =<< (++)
    <$> (printf "Finished in %1.4f seconds" <$> getRealTime)
    <*> (maybe "" (printf ", used %1.4f seconds of CPU time") <$> getCPUTime)

  fails   <- getFailCount
  pending <- getPendingCount
  total   <- getTotalCount

  let
    output =
         pluralize total   "example"
      ++ ", " ++ pluralize fails "failure"
      ++ if pending == 0 then "" else ", " ++ show pending ++ " pending"
    c | fails /= 0   = withFailColor
      | pending /= 0 = withPendingColor
      | otherwise    = withSuccessColor
  c $ writeLine output

formatLocation :: Location -> String
formatLocation (Location file line column) = file ++ ":" ++ show line ++ ":" ++ show column ++ ": "
