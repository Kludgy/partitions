import Data.Partitions

main :: IO ()
main = print . length $ partitions "abcdefghijkl"
