module Main where

import Control.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Error
import System.Process


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
  {-  case excode of 
    ExitSuccess -> return (Right (sout,serr)) 
    ExitFailure _ -> return (Left serr) -}

    
{- 
showReferences :: FilePath -> IO (Either String [String])
showReferences deriv = do
  (excode nixStore ["--query", "--references", deriv] 
-}

main = do 
  args <- getArgs 
  let name = args !! 0
  -- homedir <- getHomeDirectory 
  Right (sout,serr) <- mkDerivationFromHepNixOverlayAttrib name

  putStrLn "sout = " 
  mapM_ putStrLn (lines sout)
  putStrLn "-------"
  putStrLn "serr = " 
  putStrLn serr 
  putStrLn "-------"

