{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Trans.Either
-- import qualified Data.Foldable as F
import Data.List
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Error
import System.Posix.Files
import System.Process

data HepNixOverlayTools = HepNixOverlayTools { pkgname :: String
                                             , envname :: String
                                             , resultPkgPath :: FilePath
                                             , resultEnvPath :: FilePath
                                             }
                        deriving (Show, Data, Typeable)  

tools :: HepNixOverlayTools
tools = HepNixOverlayTools { pkgname = def &= argPos 0 &= typ "MAINPKG"
                           , envname = def &= argPos 1 &= typ "ENVPKG"
                           , resultPkgPath = "resultpkg" &= opt "resultpkg"
                           , resultEnvPath = "resultenv" &= opt "resultenv"
                           } 
 


simpleRunCmd :: String -> [String] -> IO (ExitCode,String,String)
simpleRunCmd cmd opts = readProcessWithExitCode cmd opts "" 

nixInstantiate :: [String] -> IO (ExitCode, String, String)
nixInstantiate = simpleRunCmd "nix-instantiate" 

nixStore :: [String] -> IO (ExitCode,String, String)
nixStore = simpleRunCmd "nix-store"

getNixPath :: IO (Maybe FilePath) 
getNixPath = catchIOError (Just <$> getEnv "NIX_PATH") $ \_ -> return Nothing

excodeToEither :: (ExitCode,String,String) -> Either String (String,String)
excodeToEither (excode,sout,serr) = 
  case excode of 
    ExitSuccess   -> Right (sout,serr)
    ExitFailure _ -> Left serr

mkDerivationFromHepNixOverlayAttrib :: String -> IO (Either String (String,String)) 
mkDerivationFromHepNixOverlayAttrib aname = do 
  Just nixpath <- getNixPath 
  res@(excode,sout,serr) <- nixInstantiate [nixpath, "-A", "hepNixOverlay." ++ aname] 
  return (excodeToEither res)


getReferencesFromDeriv :: FilePath -> IO (Either String [String])
getReferencesFromDeriv deriv = do
  res <- nixStore ["--query", "--references", deriv] 
  (return . either Left (Right . lines . fst) . excodeToEither) res


obtainFromAttrib :: (FilePath -> IO (Either String [String])) -> String -> IO [String]
obtainFromAttrib f aname = do 
  Right (sout,serr) <- mkDerivationFromHepNixOverlayAttrib aname
  let act x = do Right lst <- f x
                 return lst
  concat <$> mapM act (lines sout)


getOutputsFromDeriv :: FilePath -> IO (Either String [String]) 
getOutputsFromDeriv deriv = do 
  res <- nixStore ["--query", "--outputs", deriv] 
  (return . either Left (Right . lines . fst) . excodeToEither) res


mkSymbLnks :: FilePath -> [FilePath] -> IO ()
mkSymbLnks fp [] = return ()
mkSymbLnks fp (x:[]) = createSymbolicLink x fp 
mkSymbLnks fp xs = 
  let ys = zip [1..] xs 
  in mapM_ (\(f,s)->createSymbolicLink (fp++show f) s) ys


main :: IO ()
main = do
  opts <- cmdArgs tools
  let pkg = pkgname opts
      env = envname opts
      resultpkg = resultPkgPath opts
      resultenv = resultEnvPath opts
  


  pkgolst <- (nub . sort . concat) <$> mapM (obtainFromAttrib getOutputsFromDeriv) [pkg]
  putStrLn "making links of output path from main package derivation" 
  mapM_ putStrLn pkgolst 
  mkSymbLnks resultpkg pkgolst

  envolst <- (nub . sort) <$> obtainFromAttrib getOutputsFromDeriv env
  putStrLn "making the link of output path from env package derivation" 
  mapM_ putStrLn envolst 
  mkSymbLnks resultenv envolst
 
  pkgdrvlst <- concat <$> mapM (obtainFromAttrib (return . Right . lines)) [pkg]
  let exclusionlst = pkgdrvlst
  putStrLn "************"


  erlst <- obtainFromAttrib getReferencesFromDeriv env
  let erlst' = filter (not . flip elem exclusionlst) erlst
  mapM_ putStrLn erlst'


  {- 

  -- envdrvlst <- obtainFromAttrib (return . Right . lines) env
  --  ++ envolst
  -- putStrLn "drvs = " 
  -- mapM_ putStrLn drvlst
  -- putStrLn "===env ref after=======" 

  putStrLn "========"
  putStrLn "references="
  lst' <- concat <$> mapM (obtainFromAttrib getReferencesFromDeriv) [pkg]
  let rlst' = (nub . sort) lst'
  mapM_ putStrLn rlst'

  putStrLn "===env ref before=======" -}
  -- mapM_ putStrLn erlst

  
  