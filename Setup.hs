{-# LANGUAGE CPP #-}
import           Control.Exception                  (SomeException, try)
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
import           System.Directory                   (createDirectoryIfMissing,
                                                     doesDirectoryExist,
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

execCMakeWithFlags :: Verbosity.Verbosity -> String -> String -> Bool -> IO ()
execCMakeWithFlags verbosity build_target target usePIC = do
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
        isMacOS = System.Info.os == "darwin"
        isAppleSilicon = System.Info.arch == "aarch64" && isMacOS
        -- Always enable PIC for static libraries to avoid linking issues when used as dependencies
        -- For static builds, we need PIC to link into shared libraries
        picFlag = if target == "blosc_static" || usePIC then "ON" else "OFF"
        baseConfigArgs = ["-S", "c-blosc", "-B", "c-blosc/build", 
                          "-DBUILD_TESTS=OFF", "-DBUILD_BENCHMARKS=OFF",
                          "-DCMAKE_BUILD_TYPE=Release",
                          "-DCMAKE_POSITION_INDEPENDENT_CODE=" ++ picFlag,
                          -- Disable assembly optimizations that cause linking issues
                          "-DDEACTIVATE_AVX2=ON",
                          -- Enable ZSTD but use external version to avoid assembly issues
                          -- If external zstd is not available, this will fall back to internal version with safer settings
                          "-DPREFER_EXTERNAL_ZSTD=OFF",
                          -- Ensure ZSTD is explicitly enabled (overrides any default deactivation)
                          "-DDEACTIVATE_ZSTD=OFF",
                          -- Force MSVC-style compilation to disable ZSTD assembly (even on non-MSVC)
                          -- This triggers the assembly-disabled path in c-blosc's CMakeLists.txt
                          "-DMSVC=ON",
                          -- Keep basic compression algorithms
                          "-DPREFER_EXTERNAL_LZ4=OFF",
                          "-DPREFER_EXTERNAL_ZLIB=OFF"]
        -- macOS specific configuration
        macOSFlags = if isMacOS then [
                          -- Fix threading on macOS
                          "-DCMAKE_THREAD_LIBS_INIT=-lpthread",
                          "-DCMAKE_HAVE_THREADS_LIBRARY=1",
                          "-DCMAKE_USE_WIN32_THREADS_INIT=0",
                          "-DCMAKE_USE_PTHREADS_INIT=1",
                          "-DTHREADS_FOUND=TRUE",
                          "-DThreads_FOUND=TRUE"] ++ 
                          (if isAppleSilicon then [
                              -- Apple Silicon specific flags
                              "-DCMAKE_SYSTEM_PROCESSOR=aarch64",
                              "-DCMAKE_OSX_ARCHITECTURES=arm64"
                          ] else [])
                     else []
        configArgs = baseConfigArgs ++ macOSFlags ++
                     -- On Windows, use static runtime to avoid DLL issues and ensure proper MSVC naming
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

-- Backward compatibility wrapper
execCMake :: Verbosity.Verbosity -> String -> String -> IO ()
execCMake verbosity build_target target = execCMakeWithFlags verbosity build_target target True

updateBloscVersion :: ConfigFlags -> IO ()
updateBloscVersion flags = do
    let verbosity = fromFlag $ configVerbosity flags
    gitDirExists <- doesDirectoryExist "c-blosc/.git"
    gitFileExists <- doesFileExist "c-blosc/.git"
    verExists <- doesFileExist "c-blosc/VERSION"
    -- Force-update VERSION so that we always use valid one as `stack clean` does not run cabal
    -- clean and we sometimes pack the wrong `VERSION` file
    when (gitDirExists || gitFileExists || not verExists) $ do
        -- Only try to use git if we're in a git repository
        if gitDirExists || gitFileExists
        then do
            result <- tryGetVersion verbosity
            case result of
                Just ver -> writeFile "c-blosc/VERSION" ver
                Nothing -> when (not verExists) $ writeFile "c-blosc/VERSION" "1.21.7.dev\n"
        else
            -- If no git and no VERSION file, create a default one
            writeFile "c-blosc/VERSION" "1.21.7.dev\n"

tryGetVersion :: Verbosity.Verbosity -> IO (Maybe String)
tryGetVersion verbosity = do
    result <- try $ rawSystemStdout verbosity "git" ["-C", "c-blosc", "describe",
        "--abbrev=4", "--dirty", "--always", "--tags"]
    case result of
        Right ver -> return (Just ver)
        Left (_ :: SomeException) -> return Nothing

makeBlosc :: Args -> ConfigFlags -> IO HookedBuildInfo
makeBlosc _ f = do
    let verbosity = fromFlag $ configVerbosity f
        external = getCabalFlag "externalBlosc" f
        target = if getCabalFlag "sharedBlosc" f then "blosc_shared" else "blosc_static"
        shared = getCabalFlag "sharedBlosc" f
        usePIC = getCabalFlag "usePIC" f
        ext = if shared then "so" else "a"
    unless external $ do
        updateBloscVersion f
        execCMakeWithFlags verbosity "" target usePIC
        -- Copy the built library to the location expected by cabal
        let srcPath = "c-blosc/build/blosc/libblosc." ++ ext
            destPath = "dist/build/libblosc." ++ ext
        srcExists <- doesFileExist srcPath
        when srcExists $ do
            -- Create the dist/build directory if it doesn't exist
            createDirectoryIfMissing True "dist/build"
            installExecutableFile verbosity srcPath destPath
    return emptyHookedBuildInfo

disablePostConfHooks :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
disablePostConfHooks args flags pd lbi
  | getCabalFlag "externalBlosc" flags = postConf simpleUserHooks args flags pd lbi
  | otherwise = return ()

updateLibDirs :: Args -> BuildFlags -> IO HookedBuildInfo
updateLibDirs _ _ = do
    dir <- getCurrentDirectory
    let bloscDir = dir ++ "/c-blosc/build/blosc"
        -- On Windows with MSVC, check multiple possible output locations
        bloscReleaseDir = dir ++ "/c-blosc/build/blosc/Release"
        bloscDebugDir = dir ++ "/c-blosc/build/blosc/Debug"
        isWindows = System.Info.os == "mingw32" || System.Info.os == "win32" || System.Info.os == "windows"
        allDirs = if isWindows then [bloscReleaseDir, bloscDebugDir, bloscDir] else [bloscDir]
        bi = emptyBuildInfo { extraLibDirs = allDirs }
    return (Just bi, [])

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs lbi
    | getCabalFlag "externalBlosc" $ configFlags lbi = return lbi
    | otherwise = do
        dir <- getCurrentDirectory
        let pkg_descr = localPkgDescr lbi
            lib = fromJust $ library pkg_descr
            libBuild = libBuildInfo lib
            libPref = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
            -- Include both the build directory and install directory
            bloscBuildDir = dir ++ "/c-blosc/build/blosc"
            allExtraLibDirs = [libPref, bloscBuildDir] ++ extraLibDirs libBuild
        return lbi {
            localPkgDescr = pkg_descr {
                library = Just $ lib {
                    libBuildInfo = libBuild {
                        extraLibDirs = allExtraLibDirs
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
                -- On Windows, CMake generates different library names based on the generator
                -- Static libraries: libblosc.lib (MSVC) or libblosc.a (MinGW)
                -- Shared libraries: blosc.dll with blosc.lib import library
                let basePaths = ["c-blosc/build/blosc/Release", "c-blosc/build/blosc/Debug", "c-blosc/build/blosc"]
                    staticNames = ["libblosc.lib", "blosc.lib", "libblosc.a"]
                    sharedDllNames = ["blosc.dll", "libblosc.dll"]
                    sharedLibNames = ["blosc.lib", "libblosc.lib"]
                
                if shared
                then do
                    -- For shared builds, copy both DLL and import library
                    -- Find and copy the DLL
                    dllFound <- findAndCopyFirst verb basePaths sharedDllNames (libPref ++ "/blosc.dll")
                    unless dllFound $
                        error "Could not find blosc DLL in any expected location"
                    
                    -- Find and copy the import library  
                    libFound <- findAndCopyFirst verb basePaths sharedLibNames (libPref ++ "/blosc.lib")
                    unless libFound $
                        error "Could not find blosc import library in any expected location"
                else do
                    -- For static builds, find and copy the static library
                    staticFound <- findAndCopyFirst verb basePaths staticNames (libPref ++ "/blosc.lib")
                    unless staticFound $
                        error "Could not find blosc static library in any expected location"
           else do
                -- On Unix-like systems, find and copy the library
                let srcPath = "c-blosc/build/blosc/libblosc." ++ ext
                    destPath = libPref ++ "/libblosc." ++ ext
                srcExists <- doesFileExist srcPath
                if srcExists
                then installExecutableFile verb srcPath destPath
                else error $ "Could not find blosc library at: " ++ srcPath

-- Helper function to find and copy the first existing file from multiple possible paths/names
findAndCopyFirst :: Verbosity.Verbosity -> [FilePath] -> [String] -> FilePath -> IO Bool
findAndCopyFirst verb basePaths names destPath = do
    let allPaths = [basePath ++ "/" ++ name | basePath <- basePaths, name <- names]
    findAndCopyFromList verb allPaths destPath

findAndCopyFromList :: Verbosity.Verbosity -> [FilePath] -> FilePath -> IO Bool
findAndCopyFromList _ [] _ = return False
findAndCopyFromList verb (srcPath:rest) destPath = do
    exists <- doesFileExist srcPath
    if exists
    then do
        installExecutableFile verb srcPath destPath
        return True
    else findAndCopyFromList verb rest destPath

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