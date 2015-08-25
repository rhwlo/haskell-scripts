import Control.Applicative ((<$>))
import Control.Monad (foldM, liftM2)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.IO (Handle, IOMode(ReadMode), hGetContents, openFile)
import System.Random (getStdRandom, randomR)

main :: IO ()
main = do
    (sampleSize:_) <- getArgs
    dictionaryWords >>= sample (read sampleSize) >>= putStr . unlines
  where
    readWords :: Handle -> IO [String]
    readWords = (fmap lines) . hGetContents
    dictionaryWords :: IO [String]
    dictionaryWords = openFile "/usr/share/dict/words" ReadMode >>= readWords

sample :: Int                       -- the sample size to take
       -> [a]                       -- the list of elements to sample from
       -> IO [a]                    -- a random but non-repeating sample of elements from that list
sample n xs =
    fmap ((!!) xs) <$> indices
  where
    indices :: IO [Int]
    indices = (fromMaybe []) <$> uniqueRandomRs n (0, ((length xs) - 1))

uniqueRandomRs :: Int               -- the number of non-repeating random numbers to generate
               -> (Int, Int)        -- the inclusive range of numbers to pass to randomR
               -> IO (Maybe [Int])  -- a list of random numbers unless sample size exceeds range
uniqueRandomRs sampleSize range
    | sampleSize > rangeSize              = return Nothing
    | otherwise                           = Just <$> foldM foldingFunction [] (take sampleSize [0..])
  where
    (rangeLow, rangeHigh) = range
    rangeSize = rangeHigh - rangeLow + 1
    foldingFunction :: [Int] -> Int -> IO [Int]
    foldingFunction excluded i = let
          decrRange = (flip (-)) i <$> range
          accumWithExcluded = (:excluded)
        in
          accumWithExcluded <$> randomRNotInList decrRange excluded

randomRNotInList :: (Int, Int)      -- the inclusive range in which to generate numbers with randomR
                 -> [Int]           -- the list of numbers to be excluded
                 -> IO Int          -- the new random number not in that list
randomRNotInList range excluded
    | rangeSize >= (length excluded)   = candidate
    | otherwise                       = error("Excluded list is >= range of numbers ")
  where
    (rangeLow, rangeHigh) = range
    rangeSize = rangeHigh - rangeLow + 1
    candidate :: IO Int
    candidate = liftM2 (+) offSet baseCandidate
      where
        baseCandidate :: IO Int
        baseCandidate = getStdRandom (randomR range)
        offSet :: IO Int
        offSet = liftM2 count ((>=) <$> baseCandidate) (return excluded)

count :: (a -> Bool)                -- a predicate function to tell whether an element should count
      -> [a]                        -- a list of elements to potentially count
      -> Int                        -- the number of elements that matched the predicate function
count p = sum . (map (fromEnum . p))
