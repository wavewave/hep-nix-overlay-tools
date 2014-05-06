{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative
import Control.Monad
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

nixShell :: [String] -> IO (ExitCode,String,String)
nixShell = simpleRunCmd "nix-shell"


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


realise :: FilePath -> IO (ExitCode, String, String) 
realise fp = do
  putStrLn $ "realise " ++ fp
  nixStore ["--realise", fp] 

realiseWithShellDryRun :: String -> IO (Either String (String, String))
realiseWithShellDryRun aname = do 
  Just nixpath <- getNixPath 
  res@(excode,sout,serr) <- nixShell [nixpath, "-A", "hepNixOverlay." ++ aname, "--dry-run", "--command", "genericBuild"] 
  return (excodeToEither res)

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

  erlst <- filter (not . flip elem exclusionlst) <$> obtainFromAttrib getReferencesFromDeriv env
  mapM_ realise erlst 

  r <- realiseWithShellDryRun env
  case r of
    Left err -> putStrLn ("ERROR: " ++ err)
    Right (sout,serr) -> putStrLn "SUCCESS" >> putStrLn sout

  

  
  