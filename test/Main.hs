module Main (main) where

import qualified Codec.Compression.Blosc as Blosc
import qualified Data.ByteString          as B
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BC
import           Test.Hspec
import           Control.Exception        (bracket_)


-- | Generate test data of varying patterns for compression testing
generateTestData :: Int -> String -> ByteString
generateTestData repetitions pattern = BC.pack $ concat $ replicate repetitions pattern

-- | Test data samples
smallRepeatingData :: ByteString
smallRepeatingData = generateTestData 100 "Hello World! "

largeRepeatingData :: ByteString  
largeRepeatingData = generateTestData 1000 "This is a test of the Blosc compression library with repeated patterns. "

randomishData :: ByteString
randomishData = BC.pack "abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()[]{}|;:',.<>?/~`"

emptyData :: ByteString
emptyData = B.empty

-- | Helper function to run tests with Blosc initialization
withBlosc :: IO a -> IO a
withBlosc action = bracket_ Blosc.initBlosc Blosc.destroyBlosc action

-- | Test compression and decompression round-trip
testRoundTrip :: ByteString -> Blosc.BloscCompressor -> Blosc.BloscShuffle -> Int -> IO Bool
testRoundTrip original compressor shuffle clevel = do
  compressed <- Blosc.compressWithOptions original compressor clevel shuffle 0
  decompressed <- Blosc.decompress compressed
  return (original == decompressed && not (B.null compressed || (B.length compressed == B.length original && compressed == original && not (B.null original))))

-- | Test basic compression properties
testCompressionProperties :: ByteString -> Blosc.BloscCompressor -> Blosc.BloscShuffle -> Int -> IO (Bool, Bool, Double)
testCompressionProperties original compressor shuffle clevel = do
  compressed <- Blosc.compressWithOptions original compressor clevel shuffle 0
  decompressed <- Blosc.decompress compressed
  let 
    roundTripWorks = original == decompressed
    compressionOccurred = B.length compressed < B.length original || B.null original
    compressionRatio = if B.null original 
                      then 1.0 
                      else fromIntegral (B.length original) / fromIntegral (B.length compressed)
  return (roundTripWorks, compressionOccurred, compressionRatio)

main :: IO ()
main = hspec $ around_ withBlosc $ do
  
  describe "Blosc Compression Library" $ do
    
    describe "Basic compression and decompression" $ do
      it "should compress and decompress small repeating data correctly" $ do
        result <- testRoundTrip smallRepeatingData Blosc.BloscLZ Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should compress and decompress large repeating data correctly" $ do
        result <- testRoundTrip largeRepeatingData Blosc.BloscLZ Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should handle empty data" $ do
        compressed <- Blosc.compress emptyData
        decompressed <- Blosc.decompress compressed
        decompressed `shouldBe` emptyData
      
      it "should handle single byte data" $ do
        let singleByte = BC.pack "A"
        result <- testRoundTrip singleByte Blosc.BloscLZ Blosc.ByteShuffle 5
        result `shouldBe` True

    describe "All compression algorithms" $ do
      let testData = largeRepeatingData
      
      it "should work with BloscLZ compressor" $ do
        result <- testRoundTrip testData Blosc.BloscLZ Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should work with LZ4 compressor" $ do
        result <- testRoundTrip testData Blosc.LZ4 Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should work with LZ4HC compressor" $ do
        result <- testRoundTrip testData Blosc.LZ4HC Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should work with Zlib compressor" $ do
        result <- testRoundTrip testData Blosc.Zlib Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should work with Zstd compressor" $ do
        result <- testRoundTrip testData Blosc.Zstd Blosc.ByteShuffle 5
        result `shouldBe` True

    describe "All shuffle options" $ do
      let testData = largeRepeatingData
      
      it "should work with NoShuffle" $ do
        result <- testRoundTrip testData Blosc.BloscLZ Blosc.NoShuffle 5
        result `shouldBe` True
      
      it "should work with ByteShuffle" $ do
        result <- testRoundTrip testData Blosc.BloscLZ Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should work with BitShuffle" $ do
        result <- testRoundTrip testData Blosc.BloscLZ Blosc.BitShuffle 5
        result `shouldBe` True

    describe "Compression levels" $ do
      let testData = largeRepeatingData
      
      it "should work with compression level 0 (no compression)" $ do
        result <- testRoundTrip testData Blosc.BloscLZ Blosc.ByteShuffle 0
        result `shouldBe` True
      
      it "should work with compression level 1 (fastest)" $ do
        result <- testRoundTrip testData Blosc.BloscLZ Blosc.ByteShuffle 1
        result `shouldBe` True
      
      it "should work with compression level 5 (balanced)" $ do
        result <- testRoundTrip testData Blosc.BloscLZ Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should work with compression level 9 (maximum)" $ do
        result <- testRoundTrip testData Blosc.BloscLZ Blosc.ByteShuffle 9
        result `shouldBe` True

    describe "Compression effectiveness" $ do
      it "should achieve good compression on repeating data" $ do
        (roundTrip, compressed, ratio) <- testCompressionProperties largeRepeatingData Blosc.BloscLZ Blosc.ByteShuffle 5
        roundTrip `shouldBe` True
        compressed `shouldBe` True
        ratio `shouldSatisfy` (> 2.0)  -- Should achieve at least 2:1 compression ratio
      
      it "should handle random-ish data gracefully" $ do
        (roundTrip, _, _) <- testCompressionProperties randomishData Blosc.BloscLZ Blosc.ByteShuffle 5
        roundTrip `shouldBe` True

    describe "All compressor and shuffle combinations" $ do
      let testData = smallRepeatingData
          compressors = [Blosc.BloscLZ, Blosc.LZ4, Blosc.LZ4HC, Blosc.Zlib, Blosc.Zstd]
          shuffles = [Blosc.NoShuffle, Blosc.ByteShuffle, Blosc.BitShuffle]
      
      mapM_ (\compressor ->
        mapM_ (\shuffle ->
            it ("should work with " ++ show compressor ++ " and " ++ show shuffle) $ do
              result <- testRoundTrip testData compressor shuffle 5
              result `shouldBe` True
          ) shuffles
        ) compressors

    describe "Default compress function" $ do
      it "should work correctly with default options" $ do
        let testData = largeRepeatingData
        compressed <- Blosc.compress testData
        decompressed <- Blosc.decompress compressed
        decompressed `shouldBe` testData
        B.length compressed `shouldSatisfy` (< B.length testData)

    describe "Error handling and edge cases" $ do
      it "should handle very small data" $ do
        let tinyData = BC.pack "x"
        result <- testRoundTrip tinyData Blosc.BloscLZ Blosc.ByteShuffle 5
        result `shouldBe` True
      
      it "should be consistent across multiple compressions of the same data" $ do
        let testData = smallRepeatingData
        compressed1 <- Blosc.compressWithOptions testData Blosc.BloscLZ 5 Blosc.ByteShuffle 0
        compressed2 <- Blosc.compressWithOptions testData Blosc.BloscLZ 5 Blosc.ByteShuffle 0
        compressed1 `shouldBe` compressed2