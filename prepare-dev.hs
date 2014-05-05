module Main where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Trans.Either
-- import qualified Data.Foldable as F
import Data.List
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


getReferencesFromDeriv :: FilePath -> IO (Either String [String])
getReferencesFromDeriv deriv = do
  res <- nixStore ["--query", "--references", deriv] 
  (return . either Left (Right . lines . fst) . excodeToEither) res

{-
getReferencesFromOneAttrib :: String -> IO [FilePath]
getReferencesFromOneAttrib aname = do 
  Right (sout,serr) <- mkDerivationFromHepNixOverlayAttrib aname
  let act x = do Right lst <- getReferencesFromDeriv x
                 return lst
  concat <$> mapM act (lines sout)
-}

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


  

main :: IO ()
main = do
  args <- getArgs 
  let names = [args !! 0] 
      envname = args !! 1
 
  drvlst <- concat <$> mapM (obtainFromAttrib (return . Right . lines)) names 
  putStrLn "drvs = " 
  mapM_ putStrLn drvlst
  putStrLn "************"
  lst <- concat <$> mapM (obtainFromAttrib getOutputsFromDeriv)  names
  let olst = (nub . sort) lst
  putStrLn "outputs=" 
  mapM_ putStrLn olst 
  putStrLn "========"
  putStrLn "references="
  lst' <- concat <$> mapM (obtainFromAttrib getReferencesFromDeriv)  names
  let rlst' = (nub . sort) lst'
  mapM_ putStrLn rlst'

  putStrLn "===env ref before======="
  erlst <- obtainFromAttrib getReferencesFromDeriv envname
  mapM_ putStrLn erlst
  putStrLn "===env ref after=======" 
  let erlst' = filter (not . flip elem drvlst) erlst
  mapM_ putStrLn erlst'
 
  