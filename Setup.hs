{-# LANGUAGE CPP #-}
import           Control.Monad                      (unless, when)
import           Control.Applicative                ((<|>))
import           Data.Char                          (toLower)
import           Data.Maybe                         (fromJust, fromMaybe)
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
import           System.Process                     (readProcess)

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
          , postConf = disablePostConfHooks
          , preBuild = updateLibDirs
          , postCopy = copyBlosc
          , postClean = cleanBlosc
        }
        -- Fix for Cabal-1.18 - it does not `copy` on `install`, so we `copy` on
        -- `install` manually. ;)
        hooksFix = if cabalVersion < mkVersion [1, 20, 0]
                       then hooks { postInst = installBlosc }
                       else hooks

execCMake :: Verbosity.Verbosity -> String -> String -> IO ()
execCMake verbosity build_target target = do
    cmakePath <- findProgramOnSearchPath Verbosity.silent defaultProgramSearchPath "cmake"
    let cmakeExec = case cmakePath of
#if MIN_VERSION_Cabal(1, 24, 0)
                      Just (p, _) -> p
#else
                      Just p      -> p
#endif
                      Nothing     -> "cmake"
    
    -- Platform-specific configuration options
    -- On Windows, use simpler build options to avoid hanging with MSBuild
    let isWindows = System.Info.os == "mingw32" || System.Info.os == "win32" || System.Info.os == "windows"
        configArgs = ["-S", "c-blosc", "-B", "c-blosc/build", 
                      "-DBUILD_TESTS=OFF", "-DBUILD_BENCHMARKS=OFF",
                      "-DCMAKE_BUILD_TYPE=Release",
                      "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"] ++
                     -- On Windows, use static runtime to avoid DLL issues
                     (if isWindows then ["-DCMAKE_MSVC_RUNTIME_LIBRARY=MultiThreaded"] else [])
    
    -- Always configure the build to ensure latest settings are applied
    rawSystemExit verbosity cmakeExec configArgs
    
    -- Then build the specific target with platform-specific optimizations
    let baseArgs = ["--build", "c-blosc/build"]
        -- On Windows, avoid specifying targets as this can cause hanging with MSBuild
        targetArgs = if isWindows then [] 
                     else (if null target then [] else ["--target", target])
        configArgs = if null build_target 
                     then (if isWindows then ["--config", "Release"] else [])
                     else ["--config", build_target]
        -- Add parallel build flags for faster builds, but avoid on Windows to prevent hanging
        parallelArgs = if isWindows then [] else ["--parallel", "4"]
        -- Add verbose flag to see what's happening during build
        verboseArgs = if verbosity >= Verbosity.verbose then ["--verbose"] else []
        buildArgs = baseArgs ++ targetArgs ++ configArgs ++ parallelArgs ++ verboseArgs
    
    rawSystemExit verbosity cmakeExec buildArgs

updateBloscVersion :: ConfigFlags -> IO ()
updateBloscVersion flags = do
    let verbosity = fromFlag $ configVerbosity flags
    gitDirExists <- doesDirectoryExist "c-blosc/.git"
    gitFileExists <- doesFileExist "c-blosc/.git"
    verExists <- doesFileExist "c-blosc/VERSION"
    -- Force-update VERSION so that we always use valid one as `stack clean` does not run cabal
    -- clean and we sometimes pack the wrong `VERSION` file
    when (gitDirExists || gitFileExists || not verExists) $ do
        ver <- rawSystemStdout verbosity "git" ["-C", "c-blosc", "describe",
            "--abbrev=4", "--dirty", "--always", "--tags"]
        writeFile "c-blosc/VERSION" ver

makeBlosc :: Args -> ConfigFlags -> IO HookedBuildInfo
makeBlosc _ f = do
    let verbosity = fromFlag $ configVerbosity f
        external = getCabalFlag "externalBlosc" f
        target = if getCabalFlag "sharedBlosc" f then "blosc_shared" else "blosc_static"
    unless external $ updateBloscVersion f
    unless external $ execCMake verbosity "" target
    return emptyHookedBuildInfo

disablePostConfHooks :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
disablePostConfHooks args flags pd lbi
  | getCabalFlag "externalBlosc" flags = postConf simpleUserHooks args flags pd lbi
  | otherwise = return ()

updateLibDirs :: Args -> BuildFlags -> IO HookedBuildInfo
updateLibDirs _ _ = do
    dir <- getCurrentDirectory
    let bloscDir = dir ++ "/c-blosc/build/blosc"
        bi = emptyBuildInfo { extraLibDirs = [ bloscDir ] }
    return (Just bi, [])

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs lbi
    | getCabalFlag "externalBlosc" $ configFlags lbi = return lbi
    | otherwise = do
        let pkg_descr = localPkgDescr lbi
            lib = fromJust $ library pkg_descr
            libBuild = libBuildInfo lib
            libPref = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
        return lbi {
            localPkgDescr = pkg_descr {
                library = Just $ lib {
                    libBuildInfo = libBuild {
                        extraLibDirs = libPref : extraLibDirs libBuild
                    }
                }
            }
        }

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib fl lbi libPref =
    let verb = fromFlag $ configVerbosity fl
        external = getCabalFlag "externalBlosc" fl
        Platform _ os = hostPlatform lbi
        shared = getCabalFlag "sharedBlosc" fl
        ext = if shared then "so" else "a"
    in unless external $
        if os == Windows
            then do
                installExecutableFile verb
                    "c-blosc/build/blosc/libblosc.a"
                    (libPref ++ "/libblosc.a")
                when shared $ installExecutableFile verb
                    "c-blosc/build/blosc/libblosc.dll"
                    (libPref ++ "/libblosc.dll")
           else
                installExecutableFile verb
                    ("c-blosc/build/blosc/libblosc." ++ ext)
                    (libPref ++ "/libblosc." ++ ext)

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
    -- Clean the CMake build directory
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