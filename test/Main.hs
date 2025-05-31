module Main (main) where

import qualified Codec.Compression.Blosc as Blosc
import qualified Data.ByteString          as B
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8 as BC


-- | Example usage of Blosc compression
-- This function demonstrates how to use the Blosc bindings
bloscExample :: ByteString -> IO (ByteString, ByteString)
bloscExample input = do
  -- Initialize Blosc
  Blosc.initBlosc

  -- Compress using default options
  compressed <- Blosc.compress input
  putStrLn $ "Original size: " ++ show (B.length input) ++ " bytes"
  putStrLn $ "Compressed size: " ++ show (B.length compressed) ++ " bytes"
  
  -- Decompress
  decompressed <- Blosc.decompress compressed
  putStrLn $ "Decompressed size: " ++ show (B.length decompressed) ++ " bytes"
  putStrLn $ "Decompression successful: " ++ show (decompressed == input)
  
  -- Clean up
  Blosc.freeResult
  Blosc.destroyBlosc
  
  return (compressed, decompressed) 

main :: IO ()
main = do
  putStrLn "Testing Blosc compression and decompression"
  
  -- Create test data - a repeating pattern to demonstrate compression benefits
  let testData = BC.pack $ concat $ replicate 1000 "This is a test of the Blosc compression library. "
  putStrLn $ "Generated " ++ show (B.length testData) ++ " bytes of test data"
  
  -- Run the example
  (compressed, decompressed) <- bloscExample testData
  
  -- Verify the results
  let compressionRatio = (fromIntegral $ B.length testData) / (fromIntegral $ B.length compressed) :: Double
  putStrLn $ "Compression ratio: " ++ show compressionRatio
  
  if testData == decompressed
    then putStrLn "Test successful! Original and decompressed data match."
    else putStrLn "Test failed! Data does not match after compression/decompression." 

-- 