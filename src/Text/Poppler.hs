{-# LANGUAGE OverloadedStrings #-}
module Text.Poppler (pdfToHtml,pdfToHtmlTextOnly,PdfToHtmlOption(..)) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           System.Exit
import           System.IO (hFlush)
import           System.IO.Temp
import           System.Process

data PdfToHtmlOption = FirstPageToConvert Int |
                       LastPageToConvert Int |
                       ExchangePdfLinksByHtml |
                       GenerateComplexDocument |
                       GeneratesingleDocumentThatIncludesAllPages |
                       IgnoreImages |
                       GenerateNoFrames |
                       ZoomThePdfDocument Double |
                       OutputHiddenText |
                       DoNotMergeParagraphs |
                       WordBreakThreshold Double
                     deriving Show

makeOption :: PdfToHtmlOption -> String
makeOption (FirstPageToConvert n) = "-f "++show n
makeOption (LastPageToConvert n) = "-l "++show n
makeOption ExchangePdfLinksByHtml = "-p"
makeOption GenerateComplexDocument = "-c"
makeOption GeneratesingleDocumentThatIncludesAllPages = "-s"
makeOption IgnoreImages = "-i"
makeOption GenerateNoFrames = "-noframes"
makeOption (ZoomThePdfDocument x) = "-zoom "++show x
makeOption OutputHiddenText = "-hidden"
makeOption DoNotMergeParagraphs = "-nomerge"
makeOption (WordBreakThreshold x) = "-wbt "++show x

pdfToHtml :: [PdfToHtmlOption] -> FilePath -> FilePath -> IO (Either (Int,String) String)
pdfToHtml options input output = do
    let args = map makeOption options++[input,output]
    (exitCode,stdout,stderr) <- readProcessWithExitCode "pdftohtml" args ""
    case exitCode of
      ExitSuccess -> do
        return (Right stdout)
      ExitFailure e -> do
        return (Left (e,stderr))

pdfToHtmlTextOnly :: [PdfToHtmlOption] -> ByteString -> IO (Either (Int,String) Text)
pdfToHtmlTextOnly options input = do
  let tmpIn = "hPoppler.pdf"
      tmpOut = "hPoppler.html"
      options' = IgnoreImages
                 :GeneratesingleDocumentThatIncludesAllPages
                 :GenerateNoFrames
                 :options
  withSystemTempFile tmpIn $ \pathIn hIn ->
    withSystemTempFile tmpOut $ \pathOut hOut -> do
      B.hPut hIn input >> hFlush hIn
      let args = map makeOption options'++[pathIn,pathOut]
      (exitCode,stdout,stderr) <- readProcessWithExitCode "pdftohtml" args ""
      case exitCode of
        ExitSuccess -> do
          t <- T.hGetContents hOut
          --print exitCode
          --putStrLn stdout
          --putStrLn stderr
          return (Right t)
        ExitFailure e -> do
          return (Left (e,stderr))
