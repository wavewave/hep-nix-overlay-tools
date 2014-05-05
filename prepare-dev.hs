module Main where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Trans.Either
import qualified Data.Foldable as F
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

getReferences :: FilePath -> IO (Either String [String])
getReferences deriv = do
  res <- nixStore ["--query", "--references", deriv] 
  (return . either Left (Right . lines . fst) . excodeToEither) res


getReferencesFromOneAttrib :: String -> IO [FilePath]
getReferencesFromOneAttrib aname = do 
  Right (sout,serr) <- mkDerivationFromHepNixOverlayAttrib aname
 
  putStrLn "sout = " 
  let act x = do Right lst <- getReferences x
                 return lst
  concat <$> mapM act (lines sout)


  {- 
  mapM_ (mapM_ (F.mapM_ putStrLn) <=< showReferences) (lines sout)
  putStrLn "-------"
  putStrLn "serr = " 
  putStrLn serr 
  putStrLn "-------"
  -}

main = do
  -- homedir <- getHomeDirectory 
  args <- getArgs 
  let name = args !! 0
  lst <- getReferencesFromOneAttrib name
  mapM_ putStrLn lst 
