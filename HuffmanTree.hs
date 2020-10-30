----
--
-- Name: Yani Kereena Atmakuri
-- Student-ID: 20191378
-- E-mail: ydavid19@student.aau.dk
--

----
-- Status of program: I think assignment is complete, according to my understanding (see solution notes below)
-- Description of external parts: I have used System.IO for reading/writing output and Data.List for list functions.
--                                I have used zvon for searching functions, e.g. http://www.zvon.org/other/haskell/Outputlist/nub_f.html
--                                I have used this video for information on huffman algorithm: https://www.youtube.com/watch?v=dM6us854Jk0
-- Non-completed parts: The optional parts (extending the solution)
--
--

----
--
-- Usage:
--
-- From interpreter:
--  ghci
--  :l solution
--  main
--
-- Compiled:
--   ghc solution.hs
--   solution
--


----
--
-- Solution notes.
--
-- The functions I want to implement (HuffmanTree represents the prefix coding)
--   e) encode: String -> (HuffmanTree, [Boolean])
--   d) decode: HuffmanTree -> [Boolean] -> String
--
-- Problem break-down:
--   1) define the huffman tree data structure
--   2) convert char array to leaves: String -> [HuffmanTreeLeaf(Char, Int)]
--   3) build tree: [HuffmanTreeLeaf(Char, Int)] -> HuffmanTree
--   4) encode, apply tree to input: String -> HuffmanTree -> [Bool]
--   5) decode, use tree to reverse: HuffmanTree -> [Bool] -> String
--   6) a main function that reads text from input and prints encoded and decoded values
--

import System.IO
import Data.List

--
-- (e+d) definitions
--
encode :: String -> ([HuffmanCoding], [Bool])
decode :: [HuffmanCoding] -> [Bool] -> String


----
--
-- (1) huffman tree representation
--
-- Tree is either a leaf or a branch with two tree elements inside
--
data HuffmanTree = HuffmanTreeLeaf Char Int  -- leaf has the character it represents and an integer weight (freq)
                 | HuffmanTreeBranch HuffmanTree HuffmanTree Int  -- branch has two subtrees and weight (sum of freq)
                 deriving(Show)

--
-- The coding table
--
data HuffmanCoding = HuffmanCoding Char [Bool]
  deriving(Show)

-- helper for coding char
codingChar :: HuffmanCoding -> Char
codingChar (HuffmanCoding c bs) = c

-- helper for coding bits
codingBools :: HuffmanCoding -> [Bool]
codingBools (HuffmanCoding c bs) = bs


----
--
-- (2) convert String to list of leaves
--
--

-- helper: count number of occurencies of a char in a string
--  use filter to create list of 'c' charactes in, then length to count how many
occurenciesInString :: Char -> String -> Int
occurenciesInString c s = length (filter (== c) s)

-- helper: get list of unique chars in a string
uniqueCharsInString :: String -> [Char]
uniqueCharsInString s = nub s

-- helper: create a leaf for character given a string
createLeaf :: Char -> String -> HuffmanTree
createLeaf c s =
  let freq = occurenciesInString c s
  in (HuffmanTreeLeaf c freq)

-- helper: produce leaves from string
--  use map to map each unique char into a leaf
stringToLeaves :: String -> [HuffmanTree]
stringToLeaves inputString =
  let uniqueChars = uniqueCharsInString inputString
  in map (\c -> createLeaf c inputString) uniqueChars


----
--
-- (3) Build huffman tree
--
--

-- helper: calculate weight of a tree
--  depending on type take weight value
treeWeight :: HuffmanTree -> Int
treeWeight (HuffmanTreeLeaf _ weight) = weight
treeWeight (HuffmanTreeBranch _ _ weight) = weight

-- helper: create a new branch based on two existing trees
--  create a new branch from two existing trees
createBranch :: HuffmanTree -> HuffmanTree -> HuffmanTree
createBranch leftTree rightTree = HuffmanTreeBranch leftTree rightTree (treeWeight leftTree + treeWeight rightTree)

-- type definition
--  take input a list of trees, and produce a single tree (recursive)
buildTree :: [HuffmanTree] -> HuffmanTree

-- case: no elements - error
buildTree [] = error "Expecting at least one element"

-- case: only one element, tree is complete
buildTree (a:[]) = a

-- case: more than one element
--  call recursively with new branch, calculate new branch by sorting
buildTree xs =
  let xss = sortOn treeWeight xs
      newBranch = createBranch (head xss) (head $ tail xss)
      remaining = drop 2 xss
  in buildTree $ newBranch : remaining

----
--
-- (4) encode, use tree to encode the string as booleans
--
--

-- helper: create a coding table
--  create a coding table for each char
codingTable :: HuffmanTree -> [HuffmanCoding]
codingTable t = codingTable' t []

-- helper: recursive helper for the codingTable function
--  remembers the current location in the tree (boolean list)
codingTable' :: HuffmanTree -> [Bool] -> [HuffmanCoding]
codingTable' (HuffmanTreeLeaf c _) codes = [(HuffmanCoding c codes)]
codingTable' (HuffmanTreeBranch left right _) codes =
  let leftCodes = codingTable' left (codes ++ [False])
      rightCodes = codingTable' right (codes ++ [True])
  in leftCodes ++ rightCodes

-- helper: find a specific coding from the table
--  uses guard for recursive step
codingTableLookup :: [HuffmanCoding] -> Char -> [Bool]
codingTableLookup [] x = error "not found in table"
codingTableLookup ((HuffmanCoding c bs):hs) x
  | c == x = bs
  | otherwise = codingTableLookup hs x

-- helper: encodes a string to a given huffman tree, returns the encoded
--
encode' :: [HuffmanCoding] -> [Char] -> [Bool]
encode' _ [] = []
encode' hc (x:xs) = (codingTableLookup hc x) ++ (encode' hc xs)


--
-- finally the encode
--
encode inputString = (hc, encode' hc inputString)
  where t = buildTree $ stringToLeaves inputString
        hc = codingTable t

----
--
-- (5) decode
--
--

-- helper: check if first elements of a list is the elements of another list
--  e.g. list=[True, True, False, False], startsWith=[True, True, False] is True
-- while list=[True, True, False, False], startsWith=[True, True, True] is False
--  use recursion and guards to evaluate
listStartsWith :: [Bool] -> [Bool] -> Bool
listStartsWith _ [] = True  -- startsWith is at end of list, return True
listStartsWith (list:as) (startsWith:bs)
  | list == startsWith = listStartsWith as bs
  | otherwise = False  -- something is not equal, return False

-- helper: find a matching huffmancoding for current list of bools
--  hcs is sorted list of huffmancodings (longest first) and cbs are the current bools
--  use listcomprehension for finding, head assumes a match
matchingCoding :: [HuffmanCoding] -> [Bool] -> HuffmanCoding
matchingCoding hcs cbs = head [ (HuffmanCoding a bs) | (HuffmanCoding a bs) <- hcs, listStartsWith cbs bs ]

--
-- helper: recursive version of decode, decodes a single char at a time and evaluates
--  on the remaining encoded bools
--
decode' _ [] = ""
decode' hcs bs = (codingChar match) : decode hcs (drop (length $ codingBools match) bs)
  where match = matchingCoding hcs bs

-- helper: sort function for huffman codings by the length of their encodings
--  sorting helps that matching is always done towards longest bits before trying shorter ones
codingLengthSort :: HuffmanCoding -> Int
codingLengthSort (HuffmanCoding c bs) = length bs

--
-- finally the decode
--
decode hc bs = decode' hcs bs
  where hcs = reverse $ sortOn codingLengthSort hc


----
--
-- (6) Main function to test
--

-- helper: display booleans a little nicer
--  for main function
b2s :: Bool -> Char
b2s True = '1'
b2s False = '0'

main = do
  putStrLn "Enter string to encode: "
  plain <- getLine
  let (hc, bits) = encode plain
  putStrLn "Encoded: "
  putStrLn $ "HuffmanCoding " ++ show hc
  putStrLn $ "Encoded value " ++ (show $ map b2s bits)
  putStrLn $ "Encoded length " ++ (show $ length bits `div` 8)
  let decoded = decode hc bits
  putStrLn $ "Decoded value " ++ decoded
  putStrLn $ "Decoded lengt " ++ (show $ length decoded)
