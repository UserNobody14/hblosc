{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

-- |
-- Module:      Codec.Compression.Blosc
-- Stability:   experimental
-- Portability: portable
--
-- This module provides Haskell bindings to the Blosc compression library,
-- a high performance compressor optimized for binary data.

module Codec.Compression.Blosc
    (
      -- * Compression and decompression
      compress
    , decompress
    , compressWithOptions
    
      -- * Library initialization
    , initBlosc
    , destroyBlosc
    
      -- * Compression options
    , BloscCompressor(..)
    , BloscShuffle(..)
    
    ) where

import           Data.ByteString          (ByteString)
import           Data.ByteString.Unsafe   (unsafeUseAsCStringLen)
import           Foreign.C.String         (CString, withCString)
import           Foreign.C.Types          (CInt(..), CSize(..))
import           Foreign.Marshal.Alloc    (alloca, free, mallocBytes)
import           Foreign.Ptr              (Ptr, castPtr)
import           Foreign.Storable         (peek)
import qualified Data.ByteString          as B

-- | Available compressors in Blosc
data BloscCompressor = 
    BloscLZ    -- ^ BloscLZ, the default compressor
  | LZ4        -- ^ LZ4, a fast compressor
  | LZ4HC      -- ^ LZ4HC, a slower but higher compression version of LZ4
  | Zlib       -- ^ Zlib, a classic general-purpose compressor
  | Zstd       -- ^ Zstd, a modern compressor with high ratios
  deriving (Show, Eq)

-- | Shuffle options
data BloscShuffle = 
    NoShuffle       -- ^ No shuffle
  | ByteShuffle     -- ^ Byte shuffle
  | BitShuffle      -- ^ Bit shuffle
  deriving (Show, Eq)

-- | Convert BloscCompressor to its string name
compressorToName :: BloscCompressor -> String
compressorToName BloscLZ = "blosclz"
compressorToName LZ4     = "lz4"
compressorToName LZ4HC   = "lz4hc"
compressorToName Zlib    = "zlib"
compressorToName Zstd    = "zstd"

-- | Convert BloscShuffle to its integer value
shuffleToInt :: BloscShuffle -> Int
shuffleToInt NoShuffle   = 0
shuffleToInt ByteShuffle = 1
shuffleToInt BitShuffle  = 2

-- Constants
bloscMaxOverhead :: Int
bloscMaxOverhead = 16  -- This value might need adjustment based on Blosc's actual BLOSC_MAX_OVERHEAD

-- | Compress data using Blosc with default options
-- Uses BloscLZ compressor, compression level 5, byte shuffle, and automatic blocksize
compress :: ByteString -> IO ByteString
compress bs = compressWithOptions bs BloscLZ 5 ByteShuffle 0

-- | Compress data with specific Blosc options
compressWithOptions :: ByteString 
                    -> BloscCompressor  -- ^ Compressor
                    -> Int              -- ^ Compression level (0-9, 0 means no compression)
                    -> BloscShuffle     -- ^ Shuffle option
                    -> Int              -- ^ Block size (0 means automatic)
                    -> IO ByteString
compressWithOptions bs compressor clevel shuffle blocksize =
  unsafeUseAsCStringLen bs $ \(srcPtr, srcLen) -> do
    let 
      destSize = srcLen + bloscMaxOverhead
      cname = compressorToName compressor
      shuffleInt = shuffleToInt shuffle
    
    -- Allocate memory for the compressed data
    destPtr <- mallocBytes (fromIntegral destSize)
    
    -- Call the compress function
    bytesCompressed <- withCString cname $ \cnamePtr ->
      c_blosc_compress_ctx 
        (fromIntegral clevel)
        (fromIntegral shuffleInt)
        (fromIntegral (4 :: Integer)) -- typesize, assuming 4 bytes (32-bit values)
        (fromIntegral srcLen)
        (castPtr srcPtr)
        destPtr
        (fromIntegral destSize)
        cnamePtr
        (fromIntegral blocksize)
        1  -- nthreads, using 1 for simplicity
    
    -- Create a ByteString from the result and free the memory
    bs' <- if bytesCompressed > 0
      then B.packCStringLen (castPtr destPtr, fromIntegral bytesCompressed)
      else return B.empty
    
    free destPtr
    return bs'

-- | Decompress data that was compressed with Blosc
decompress :: ByteString -> IO ByteString
decompress bs =
  unsafeUseAsCStringLen bs $ \(srcPtr, _srcLen) -> do
    -- Get the decompressed size
    alloca $ \nbytesPtr -> 
      alloca $ \cbytesPtr -> 
        alloca $ \blocksizePtr -> do
          c_blosc_cbuffer_sizes 
            (castPtr srcPtr)
            nbytesPtr
            cbytesPtr
            blocksizePtr
          
          nbytes <- peek nbytesPtr
          
          if nbytes == 0
            then return B.empty
            else do
              -- Allocate memory for the decompressed data
              destPtr <- mallocBytes (fromIntegral nbytes)
              
              -- Call the decompress function
              result <- c_blosc_decompress_ctx
                (castPtr srcPtr)
                destPtr
                (fromIntegral nbytes)
                1  -- nthreads
              
              -- Create a ByteString from the result and free the memory
              bs' <- if result >= 0
                then B.packCStringLen (castPtr destPtr, fromIntegral nbytes)
                else return B.empty
              
              free destPtr
              return bs'


-- decompressWithOptions :: ByteString
--                       -> Int
--                       -> Int
--                       -> Int
--                       -> IO ByteString
-- decompressWithOptions bs numBytes numThreads targetSize =
--   unsafeUseAsCStringLen bs $ \(srcPtr, srcLen) -> do
--     destPtr <- mallocBytes (fromIntegral targetSize)
--     result <- c_blosc_decompress_ctx
--       (castPtr srcPtr)
--       destPtr
--       (fromIntegral numBytes)
--       (fromIntegral numThreads)
--     bs' <- if result >= 0
--       then B.packCStringLen (castPtr destPtr, fromIntegral result)
--       else return B.empty
--     free destPtr
--     return bs'

-- | Initialize the Blosc library. Should be called before any compression/decompression.
initBlosc :: IO ()
initBlosc = c_blosc_init

-- | Cleanup and free resources used by the Blosc library. Should be called when done.
destroyBlosc :: IO ()
destroyBlosc = c_blosc_destroy

-- Foreign imports

foreign import ccall unsafe "blosc.h blosc_compress_ctx"
  c_blosc_compress_ctx :: CInt    -- clevel
                       -> CInt    -- shuffle
                       -> CSize   -- typesize
                       -> CSize   -- nbytes
                       -> Ptr a   -- src
                       -> Ptr a   -- dest
                       -> CSize   -- destsize
                       -> CString -- compname
                       -> CSize   -- blocksize
                       -> CInt    -- nthreads
                       -> IO CInt -- compressed size

foreign import ccall unsafe "blosc.h blosc_decompress_ctx"
  c_blosc_decompress_ctx :: Ptr a   -- src
                         -> Ptr a   -- dest
                         -> CSize   -- destsize
                         -> CInt    -- nthreads
                         -> IO CInt -- decompressed size

foreign import ccall unsafe "blosc.h blosc_cbuffer_sizes"
  c_blosc_cbuffer_sizes :: Ptr a     -- cbuffer
                        -> Ptr CSize -- nbytes
                        -> Ptr CSize -- cbytes
                        -> Ptr CSize -- blocksize
                        -> IO ()


-- Initialize and destroy Blosc
foreign import ccall unsafe "blosc.h blosc_init"
  c_blosc_init :: IO ()

foreign import ccall unsafe "blosc.h blosc_destroy"
  c_blosc_destroy :: IO ()