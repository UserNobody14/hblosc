{-# LANGUAGE CPP #-}
import           Control.Monad                      (unless, when)
import           Data.Maybe                         (fromJust, fromMaybe)
import           Control.Applicative                ((<|>))
import           Data.Char                          (toLower)

import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo (InstallDirs (..),
                                                     LocalBuildInfo (..),
                                                     absoluteInstallDirs,
                                                     localPkgDescr)
import           Distribution.Simple.Program.Find   (defaultProgramSearchPath,
                                                     findProgramOnSearchPath)
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils          (cabalVersion,
                                                     installExecutableFile,
                                                     rawSystemExit,
                                                     rawSystemStdout)
import           Distribution.System
import qualified Distribution.Verbosity             as Verbosity
import           System.Directory                   (doesDirectoryExist,
                                                     doesFileExist,
                                                     getCurrentDirectory)
import qualified System.Info                        as System.Info

#if MIN_VERSION_Cabal(2, 0, 0)
import           Distribution.Version               (mkVersion)
#else
mkVersion :: [Int] -> Version
mkVersion = flip Version []

mkFlagName :: String -> FlagName
mkFlagName = FlagName
#endif

main = defaultMainWithHooks hooksFix
    where
        hooks = simpleUserHooks {
            preConf = makeBlosc
          , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
          , postCopy = copyBlosc
          , postClean = cleanBlosc
        }
        -- Fix for Cabal-1.18 - it does not `copy` on `install`, so we `copy` on
        -- `install` manually. ;)
        hooksFix = if cabalVersion < mkVersion [1, 20, 0]
                       then hooks { postInst = installBlosc }
                       else hooks

execCMake :: Verbosity.Verbosity -> IO ()
execCMake verbosity = do
    -- Fail immediately on Windows
    when (System.Info.os `elem` ["mingw32", "win32", "windows"]) $
        error "Windows is not supported by this build system"
    
    cmakePath <- findProgramOnSearchPath Verbosity.silent defaultProgramSearchPath "cmake"
    let cmakeExec = case cmakePath of
#if MIN_VERSION_Cabal(1, 24, 0)
                      Just (p, _) -> p
#else
                      Just p      -> p
#endif
                      Nothing     -> "cmake"
    
    -- Simple configuration
    let configArgs = ["-S", "c-blosc", "-B", "c-blosc/build", 
                      "-DBUILD_TESTS=OFF", "-DBUILD_BENCHMARKS=OFF",
                      "-DCMAKE_BUILD_TYPE=Release",
                      "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"]
    
    rawSystemExit verbosity cmakeExec configArgs
    
    -- Build
    let buildArgs = ["--build", "c-blosc/build", "--parallel", "4"]
    rawSystemExit verbosity cmakeExec buildArgs

updateBloscVersion :: ConfigFlags -> IO ()
updateBloscVersion flags = do
    let verbosity = fromFlag $ configVerbosity flags
    gitDirExists <- doesDirectoryExist "c-blosc/.git"
    gitFileExists <- doesFileExist "c-blosc/.git"
    verExists <- doesFileExist "c-blosc/VERSION"
    when (gitDirExists || gitFileExists || not verExists) $ do
        ver <- rawSystemStdout verbosity "git" ["-C", "c-blosc", "describe",
            "--abbrev=4", "--dirty", "--always", "--tags"]
        writeFile "c-blosc/VERSION" ver

makeBlosc :: Args -> ConfigFlags -> IO HookedBuildInfo
makeBlosc _ f = do
    let verbosity = fromFlag $ configVerbosity f
        external = getCabalFlag "externalBlosc" f
    unless external $ do
        updateBloscVersion f
        execCMake verbosity
    return emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs lbi
    | getCabalFlag "externalBlosc" $ configFlags lbi = return lbi
    | otherwise = do
        let pkg_descr = localPkgDescr lbi
            lib = fromJust $ library pkg_descr
            libBuild = libBuildInfo lib
            libPref = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
        dir <- getCurrentDirectory
        let bloscDir = dir ++ "/c-blosc/build/blosc"
        return lbi {
            localPkgDescr = pkg_descr {
                library = Just $ lib {
                    libBuildInfo = libBuild {
                        extraLibDirs = bloscDir : libPref : extraLibDirs libBuild
                    }
                }
            }
        }

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib fl lbi libPref = do
    let verb = fromFlag $ configVerbosity fl
        external = getCabalFlag "externalBlosc" fl
        shared = getCabalFlag "sharedBlosc" fl
        ext = if shared then "so" else "a"
    unless external $ do
        let srcLib = "c-blosc/build/blosc/libblosc." ++ ext
            destLib = libPref ++ "/libblosc." ++ ext
        installExecutableFile verb srcLib destLib

copyBlosc :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyBlosc _ flags pkg_descr lbi =
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
        config = configFlags lbi
    in copyLib config lbi libPref

installBlosc :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installBlosc _ flags pkg_descr lbi =
    let libPref = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
        config = configFlags lbi
    in copyLib config lbi libPref

cleanBlosc :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanBlosc _ flags _ _ = do
    let verbosity = fromFlag $ cleanVerbosity flags
    cmakePath <- findProgramOnSearchPath Verbosity.silent defaultProgramSearchPath "cmake"
    let cmakeExec = case cmakePath of
#if MIN_VERSION_Cabal(1, 24, 0)
                      Just (p, _) -> p
#else
                      Just p      -> p
#endif
                      Nothing     -> "cmake"
    buildDirExists <- doesDirectoryExist "c-blosc/build"
    when buildDirExists $ 
        rawSystemExit verbosity cmakeExec ["--build", "c-blosc/build", "--target", "clean"]

#if MIN_VERSION_Cabal(2, 2, 0)
getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookupFlagAssignment (mkFlagName name') allFlags)
    where allFlags = configConfigurationsFlags flags
          name' = map toLower name
#else
getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookup (mkFlagName name') allFlags)
    where allFlags = configConfigurationsFlags flags
          name' = map toLower name
#endif