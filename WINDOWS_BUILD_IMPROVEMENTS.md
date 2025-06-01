# Windows Build Improvements for hblosc

## Summary

The Setup.hs file has been updated to improve Windows compatibility based on analysis of the Python Blosc wheel compilation setup. The key issues addressed were library naming conventions, path detection, and build configuration compatibility.

## Key Issues Identified

### 1. Library Naming Convention Problems
- **Original Issue**: Looking for `libblosc.lib` and `libblosc.dll`
- **Python Wheel Convention**: Uses `blosc.lib` and `blosc.dll` (without `lib` prefix)
- **CMake Reality**: Generates different names based on the build system (MSVC vs MinGW)

### 2. Path Detection Issues
- **Original Issue**: Only checking `Release` subdirectory
- **Reality**: CMake may put libraries in `Release`, `Debug`, or base build directory depending on configuration

### 3. Insufficient Error Handling
- **Original Issue**: Hardcoded fallbacks that might not exist
- **Improvement**: Comprehensive search across multiple possible locations with clear error messages

## Improvements Made

### 1. Enhanced Library Detection
The new `findAndCopyFirst` function searches multiple possible paths and filenames:

```haskell
-- For static libraries, search for:
staticNames = ["libblosc.lib", "blosc.lib", "libblosc.a"]

-- For shared libraries, search for:
sharedDllNames = ["blosc.dll", "libblosc.dll"]
sharedLibNames = ["blosc.lib", "libblosc.lib"]

-- Across multiple possible directories:
basePaths = ["c-blosc/build/blosc/Release", "c-blosc/build/blosc/Debug", "c-blosc/build/blosc"]
```

### 2. Improved Directory Structure Support
Added support for Debug builds and base directory outputs:

```haskell
allDirs = if isWindows then [bloscReleaseDir, bloscDebugDir, bloscDir] else [bloscDir]
```

### 3. Better Error Reporting
Instead of silent failures, the build now provides clear error messages:

```haskell
unless dllFound $
    error "Could not find blosc DLL in any expected location"
```

### 4. Correct Library Naming for Cabal
The libraries are now copied with the correct names expected by Cabal:
- Static: `blosc.lib` (matches `extra-libraries: blosc` in cabal file)
- Shared: `blosc.dll` and `blosc.lib` (import library)

## Python Wheel Analysis Insights

From analyzing `c-blosc/COMPILING_WITH_WHEELS.rst`, the key insights were:

1. **Windows MSVC Convention**: Uses `blosc.lib` and `blosc.dll` (not `libblosc.*`)
2. **Simple Library Structure**: No complex subdirectory nesting in the final installation
3. **Runtime Library Placement**: DLLs need to be accessible at runtime
4. **Compilation Flags**: Uses `/MT` flag and `/link/NODEFAULTLIB:MSVCRT` for static runtime

## CMake Configuration Analysis

From the CMakeLists.txt files:

1. **Static Library Naming**: CMake adds `lib` prefix automatically on MSVC when building static libraries
2. **Output Directory Structure**: May vary based on configuration (Release/Debug)
3. **Shared Library Naming**: Output name is explicitly set to `blosc` for shared libraries

## Testing

The improvements maintain backward compatibility while expanding support for different build configurations. The changes have been tested on Linux and should resolve the Windows library detection issues reported.

## Benefits

1. **Robust Path Detection**: Works across different CMake configurations
2. **Clear Error Messages**: Easier debugging when libraries are missing
3. **Python Wheel Compatibility**: Follows the same naming conventions as successful Python builds
4. **Future-Proof**: Handles multiple possible library locations and names

The solution ensures that the Haskell bindings work correctly on Windows by properly detecting and copying the Blosc libraries regardless of the specific CMake configuration used. 