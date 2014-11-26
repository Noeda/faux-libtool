{-# LANGUAGE DeriveDataTypeable #-}

module Main ( main ) where

import Data.Typeable
import Data.Monoid
import Data.List
import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.Trans.Writer.Strict
import Control.Monad.IO.Class
import System.Posix.Files
import System.Directory
import System.Exit
import System.IO.Error
import System.Process
import System.Environment
import System.IO.Temp

data WhatToDo = WhatToDo
    { output :: (Maybe FilePath)
    , searchPaths :: [FilePath]
    , linkLibraries :: [FilePath]
    , inFiles :: [FilePath] }
    deriving ( Eq, Ord, Show, Read, Typeable )

instance Monoid WhatToDo where
    mempty = WhatToDo { output = Nothing
                      , searchPaths = []
                      , linkLibraries = []
                      , inFiles = [] }
    wtd1 `mappend` wtd2 =
        WhatToDo { output = output wtd2 <|> output wtd1
                 , searchPaths = searchPaths wtd1 <> searchPaths wtd2
                 , linkLibraries = linkLibraries wtd1 <> linkLibraries wtd2
                 , inFiles = inFiles wtd1 <> inFiles wtd2 }

main :: IO ()
main = do
    args <- getArgs
    let wtd = processArgs args mempty
    when (output wtd == Nothing) $
        error "No outfile specified."

    let Just outfile = output wtd
    print wtd

    withSystemTempDirectory "faux-libtool" $ \dir_name -> do
        infiles <- collectInFiles dir_name wtd
        _ <- tryIOError $ removeLink outfile
        _ <- readProcess "ar" ("r":outfile:infiles) ""
        _ <- readProcess "ranlib" [outfile] ""
        return ()

collectInFiles :: FilePath -> WhatToDo -> IO [FilePath]
collectInFiles tmp_dir_name wtd = do
    old_path <- getCurrentDirectory
    flip finally (setCurrentDirectory old_path) $ do
        setCurrentDirectory tmp_dir_name
        object_files <- execWriterT $ for_ (linkLibraries wtd) $ \lib -> do
            maybe_raw_listing <- liftIO $ tryAllSearchPaths lib $ searchPaths wtd
            case maybe_raw_listing of
                Nothing -> error $ "Cannot find " <> lib
                Just (Left (raw_listing, lib)) -> do
                    _ <- liftIO $ readProcess "ar" ["x", lib] ""
                    tell $ fmap (\x -> tmp_dir_name <> "/" <> x) $
                        lines raw_listing
                Just (Right ob_name) -> tell [ob_name]
        return $ inFiles wtd <> object_files
  where
    tryAllSearchPaths :: FilePath -> [FilePath] -> IO (Maybe (Either (String, FilePath) FilePath))
    tryAllSearchPaths _ [] = return Nothing
    tryAllSearchPaths name (search_path:rest) = do
        let lib_name = search_path <> "/" <> "lib" <> name <> ".a"
            ob_name = search_path <> "/" <> name <> ".o"
        r <- doesFileExist ob_name
        if r
          then return $ Just $ Right ob_name
          else do (code, maybe_raw_listing, _) <-
                      readProcessWithExitCode "ar" ["t", lib_name] ""
                  case (code, maybe_raw_listing) of
                      (ExitSuccess, listing) -> return $ Just $ Left $ (listing, lib_name)
                      _ -> tryAllSearchPaths name rest

processArgs :: [String] -> WhatToDo -> WhatToDo
processArgs ("-static":rest) accum = processArgs' rest accum
processArgs _ _ = error "Expected '-static' as first argument."

processArgs' :: [String] -> WhatToDo -> WhatToDo
processArgs' [] accum = accum
processArgs' (x:rest) accum = accum <> case x of
    "-o" -> case rest of
                (outfile:rest') -> processArgs'
                                   rest'
                                   mempty { output = Just outfile }
                _ -> error "Expected output file after '-o'."
    x | isPrefixOf "-L" x ->
        processArgs' rest
                     mempty { searchPaths = [drop 2 x] }
      | isPrefixOf "-l" x ->
        processArgs' rest
                     mempty { linkLibraries = [drop 2 x] }
      | isPrefixOf "-Wl" x -> processArgs' rest mempty

    infile -> processArgs' rest
                           mempty { inFiles = [infile] }



