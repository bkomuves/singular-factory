
-- | Guessing the location of factory\'s gftables

{-# LANGUAGE CPP, BangPatterns #-}
module GFTables where

------------------------------------------------------------------------------------------

import Control.Monad

import Foreign.Ptr
import Foreign.C
import Foreign.C.String

import System.FilePath
import System.Directory
import System.Process

--------------------------------------------------------------------------------
-- * Initialization

tryAndInitGFTables :: IO ()
tryAndInitGFTables = initGFTables Nothing

-- | Set the location of the small finite field table files.
--
-- If you know where they are located, please set it.
-- If you don't know, we try to guess it, but I have no idea how
-- to figure this out in general (pkg-config does not seem to have this information...)
--
initGFTables :: Maybe FilePath -> IO ()
initGFTables mbdir = case mbdir of
  Just fpath -> setGFTablesDir fpath
  Nothing    -> guessGFTablesDir >>= \d -> case d of
    Just fpath -> do 
      -- putStrLn $ "gftables dir = " ++ (fpath </> "gftables")
      setGFTablesDir fpath
    Nothing    -> error "FATAL: cannot find factory's gftables"

setGFTablesDir :: FilePath -> IO ()
setGFTablesDir fpath0 = do
  fpath1 <- canonicalizePath fpath0
  withCString (fpath1 ++ "/") $ \ptr -> set_gftable_dir ptr

-- | Apparently we need to manually find the directory containing the GF tables...
--
-- On my debian install it is at @/usr/share/singular/factory/gftables/@, but how
-- to figure that out???
foreign import ccall "set_gftable_dir" set_gftable_dir :: Ptr CChar -> IO ()

------------------------------------------------------------------------------------------
-- * Guessing

guessGFTablesDir :: IO (Maybe FilePath)
guessGFTablesDir = do

#if defined(linux_HOST_OS)
  guessLinux
#elif defined(darwin_HOST_OS)
  guessHomebrew
#elif defined(mingw32_HOST_OS) || defined(mingw64_HOST_OS) 
  guessCygwin
#else
  return Nothing
#endif
   
------------------------------------------------------------------------------------------

infixr 5 >>>

(>>>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a) 
(>>>) action1 action2 = do 
  mb <- action1 
  case mb of 
    Just x  -> return (Just x)
    Nothing -> action2
  
testDir :: FilePath -> IO (Maybe FilePath)
testDir dir = doesFileExist (dir </> "gftables/361") >>= \b -> if b 
  then return (Just dir) 
  else return Nothing

------------------------------------------------------------------------------------------

-- macOS w/ Homebrew
-- on my machine it looks like this: /usr/local/Cellar/singular/4.1.2p1_2/share/factory
guessHomebrew :: IO (Maybe FilePath)
guessHomebrew = do
  let brew_prefix = "/usr/local"            -- TODO: maybe use "brew config" (but it's rather slow)
  let cellar    = brew_prefix </> "Cellar"
  let sing_root = cellar      </> "singular"
  entries <- map (sing_root </>) <$> listDirectory sing_root
  subdirs <- filterM doesDirectoryExist entries
  print entries
  print subdirs
  foldl (>>>) (return Nothing) [ testDir (sing_root </> d </> "share/factory") | d <- subdirs]

-- generic Linux
-- I guess typically it looks like this:
guessLinux :: IO (Maybe FilePath)
guessLinux =
  testDir "/usr/share/singular/factory" >>>
  testDir "/usr/share/singular"         >>>
  testDir "/usr/share/factory"          >>>
  testDir "/usr/local/share/singular/factory" >>>
  testDir "/usr/local/share/singular"   >>>
  testDir "/usr/local/share/factory" 

-- cygwin
guessCygwin ::IO (Maybe FilePath)
guessCygwin = do
  cygwin_root <- readCreateProcess (shell "cygpath -w /") ""
  let test1 dir = testDir (cygwin_root </> dir)
  id <$>
    test1 "/usr/share/singular/factory" >>>
    test1 "/usr/share/singular"         >>>
    test1 "/usr/share/factory"          >>>
    test1 "/usr/local/share/singular/factory" >>>
    test1 "/usr/local/share/singular"   >>>
    test1 "/usr/local/share/factory" 

------------------------------------------------------------------------------------------
