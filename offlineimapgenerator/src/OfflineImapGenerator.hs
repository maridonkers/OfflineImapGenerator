{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OfflineImapGenerator where

import Control.Exception (finally)
import qualified Control.Foldl as Fold
import Data.List
import Data.Maybe
import Data.PublicSuffix
import qualified Data.Text as T
import System.IO
import Turtle

offlineImaprcTemplate :: String
offlineImaprcTemplate = ".offlineimaprct"

offlineImaprc :: String
offlineImaprc = ".offlineimaprc"

remotehostKey :: String
remotehostKey = "remotehost"

certFingerprintKey :: String
certFingerprintKey = "cert_fingerprint"

cmdPrefix :: String
cmdPrefix = "openssl s_client -connect"

cmdPostfix :: String
cmdPostfix = ":443 < /dev/null 2>/dev/null | openssl x509 -fingerprint -noout -in /dev/stdin"

generate :: String -> Bool -> IO ()
generate home verbose = do
  rcth <- openFile (prefixPathWith home offlineImaprcTemplate) ReadMode
  rch <- openFile (prefixPathWith home offlineImaprc) WriteMode
  finally
    (processSections verbose rcth rch)
    ( do
        hClose rch
        hClose rcth
        echo "Done."
    )
  where
    prefixPathWith :: String -> System.IO.FilePath -> System.IO.FilePath
    prefixPathWith home path =
      home ++ "/" ++ path

getCmdResult :: Maybe Line -> String
getCmdResult response =
  case lineToText <$> response of
    Nothing -> "ERROR ** Got nothing"
    Just s -> if s == "-1" then "ERROR ** Got -1" else (tail . init) $ show s

type Property = String

type Section = [Property]

processSections :: Bool -> Handle -> Handle -> IO ()
processSections verbose rcth rch = do
  eof <- hIsEOF rcth
  if eof
    then pure ()
    else do
      line <- getLineSkipComment
      processSections' line
  where
    getLineSkipComment :: IO String
    getLineSkipComment = do
      e <- hIsEOF rcth
      if e
        then pure ""
        else do
          l <- hGetLine rcth
          if "#" `isPrefixOf` l -- TODO whitespace before comment
            then getLineSkipComment
            else pure l
          
    processSections' :: Property -> IO ()
    processSections' line' = do
      (eof', line'', sect) <- getSection (False, line', [])
      let section = reverse sect
      processSection section
      if eof'
        then pure ()
        else do
          processSections' line''

    getSection :: (Bool, Property, Section) -> IO (Bool, Property, Section)
    getSection (_, prop, sect) = do
      eof'' <- hIsEOF rcth
      if eof''
        then pure (eof'', prop, getLastSect prop sect)
        else do
          prop' <- getLineSkipComment
          if isSection prop'
            then pure (eof'', prop', getLastSect prop sect)
            else getSection (False, prop', prop : sect)

    isSection :: Property -> Bool
    isSection p = "[" `isPrefixOf` p -- TODO whitespace before the `[` character
    getLastSect :: Property -> Section -> Section
    getLastSect p s =
      if isSection p then s else p : s

    processSection :: Section -> IO ()
    processSection section' = do
      let remoteHost = getRemoteHost section'
      when (verbose) $ do
        print section'
      if isJust remoteHost
        then do
          let Just rh = remoteHost
          crf <- getCertificateFingerprint rh
          writeSection section' crf
        else writeSection section' Nothing
      where
        getRemoteHost :: Section -> Maybe String
        getRemoteHost s = do
          let rhss = filter (T.isInfixOf (T.pack remotehostKey) . T.pack) s
          let lrhss = length rhss
          if lrhss <= 0 || lrhss > 1
            then Nothing
            else do
              let (rhs : _) = rhss
              let ih = fromMaybe (-1) $ elemIndex '=' rhs
              if ih >= 0
                then Just $ trim $ drop (ih + 1) rhs
                else Nothing

        getCertificateFingerprint :: String -> IO (Maybe String)
        getCertificateFingerprint rh' = do
          let rh'' = getBaseRemoteHost rh'
          let cmd = T.pack $ cmdPrefix ++ " " ++ rh'' ++ cmdPostfix
          cmdOut <- fold (inshell cmd empty) Fold.head
          let crs = getCmdResult cmdOut
          let icf = fromMaybe (-1) $ elemIndex '=' crs
          if icf >= 0
            then pure $ Just $ drop (icf + 1) crs
            else pure Nothing
          where
            getBaseRemoteHost :: String -> String
            getBaseRemoteHost r = do
              let rphs = publicSuffix r
              let rs = T.split (== '.') (T.pack r)
              let lrs = T.unpack $ last rs
              let lrsb = T.unpack (last $ init rs)
              if lrs == rphs
                then lrsb ++ "." ++ lrs
                else r

        writeSection :: Section -> Maybe String -> IO ()
        writeSection s mc = do
          mapM_ (writeProperty mc) s
          where
            writeProperty :: Maybe String -> String -> IO ()
            writeProperty mc p = do
              if isJust mc
                then do
                  let Just c = mc
                  let ic = T.isInfixOf (T.pack certFingerprintKey) (T.pack p)
                  let p' =
                        if ic then certFingerprintKey ++ " = " ++ c else p
                  when (ic) $ putStrLn p'
                  hPutStrLn rch p'
                else hPutStrLn rch p

trim :: String -> String
trim s = T.unpack $ T.strip $ T.pack s
