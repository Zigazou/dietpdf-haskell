{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import BaselineResources qualified as Baseline

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless)
import Control.Monad.State (evalStateT, get, gets)
import Control.Monad.Trans.Except (runExceptT)

import Data.ByteString qualified as BS
import Data.Foldable (foldl')
import Data.List (sort)
import Data.PDF.PDFObject (PDFObject (..))
import Data.PDF.PDFPartition (PDFPartition (..))
import Data.PDF.PDFWork (PDFWork, evalPDFWorkT)
import Data.PDF.WorkData (WorkData, wPDF)

import GHC.Clock (getMonotonicTimeNSec)

import OptimizedResources qualified as Optimized

import PDF.Document.Parser (pdfParse)
import PDF.Processing.PDFWork (importObjects)

import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Mem (performGC)

import Text.Printf (printf)

-- Force transformed dictionaries without serializing stream bytes in the timer.
forceObject :: PDFObject -> ()
forceObject object = case object of
  PDFIndirectObject _ _ value -> forceObject value
  PDFIndirectObjectWithStream _ _ dictionary bytes ->
    forceValues dictionary `seq` BS.length bytes `seq` ()
  PDFDictionary dictionary -> forceValues dictionary
  PDFArray values          -> forceValues values
  _                        -> object `seq` ()

forceValues :: Foldable f => f PDFObject -> ()
forceValues = foldl' (\() object -> forceObject object) ()

forcePartition :: PDFPartition -> ()
forcePartition pdf = forceValues (ppObjectsWithStream pdf)
  `seq` forceValues (ppObjectsWithoutStream pdf)

measure :: WorkData -> PDFWork IO () -> IO (Double, String)
measure initial action = do
  performGC
  start <- getMonotonicTimeNSec
  result <- runExceptT (evalStateT (action >> gets wPDF) initial)

  case result of
    Left err -> fail (show err)
    Right pdf -> do
      _ <- evaluate (forcePartition pdf)
      end <- getMonotonicTimeNSec

      -- PDFObject's Eq compares indirect object numbers only. Compare the full
      -- representation outside the timer to detect changed resource entries.
      let
        representation :: String
        representation = show pdf

      _ <- evaluate (length representation)
      return (fromIntegral (end - start) / 1e9, representation)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  paths <- getArgs

  when (null paths) (fail "Supply at least one PDF path")

  forM_ paths $ \path -> do
    bytes <- BS.readFile path
    Right document <- runExceptT (pdfParse bytes)
    Right initial <- evalPDFWorkT (importObjects document >> get)
    _ <- evaluate (forcePartition (wPDF initial))

    putStrLn path

    samples <- forM [1..5 :: Int] $ \iteration -> do
      let
        baseline :: IO (Double, String)
        baseline = measure initial Baseline.removeUnusedResources

        optimized :: IO (Double, String)
        optimized = measure initial Optimized.removeUnusedResources

      ((before, expected), (after, actual)) <-
        if odd iteration
          then
            (,) <$> baseline <*> optimized
          else do
            current <- optimized
            original <- baseline
            return (original, current)

      unless (expected == actual) (fail "Resource removal outputs differ")
      printf "  baseline=%.6fs optimized=%.6fs identical=yes\n" before after
      return (before, after)

    let
      median :: [Double] -> Double
      median values = sort values !! 2

      before:: Double
      before = median (map fst samples)

      after :: Double
      after = median (map snd samples)

    printf "  median: %.6fs -> %.6fs (%.2fx)\n" before after (before / after)
