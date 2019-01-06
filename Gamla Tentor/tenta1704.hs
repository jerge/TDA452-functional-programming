import Prelude hiding (splitAt)
import Test.QuickCheck

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Eq,Show)

t :: Tree Int
t = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 10))

sumTree :: Num a => Tree a -> a
sumTree tree = opTree (+) tree

minTree :: Ord a => Tree a -> a
minTree tree = opTree min tree

opTree :: (a -> a -> a) -> Tree a -> a
opTree _ (Leaf l) = l
opTree f (Branch l r) = f (opTree f l) (opTree f r)

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs | n <= 0           = ([],xs)
             | n >= length xs   = (xs,[])
splitAt _ [] = ([],[])
splitAt n (x:xs) = (x:ys,zs)
    where (ys,zs) = splitAt (n-1) xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs | n >= length xs = [xs]
              | otherwise      = ys:(chunksOf n zs)
    where (ys,zs) = splitAt n xs

prop_length_chunksOf :: Int -> [a] -> Property
prop_length_chunksOf n xs = n > 0 ==> length (chunksOf n xs) == (length xs + (n-1)) `div` n

-- fa :: a -> [a]
-- fb :: Num a => Bool -> a -> a
-- fc :: ((a -> b), (c -> d)) -> (a,c) -> (b,d)
{-
rHand :: Gen Hand
rHand = do size <- choose (1,6)
           cards <- vectorOf size rCard
           return (toHand (nub cards))
        where toHand :: [Card] -> Hand
              toHand [] = Empty
              toHand (x:xs) = Add x (toHand xs)
-}
framed :: String -> String
framed s = stars ++ "\n*" ++ s ++ "*\n" ++ stars ++ "\n"
    where stars = replicate (length s + 2) '*'

type TagName = String         -- A few lowercase letters ’a’..’z’
data XML = Text String        -- Arbitrary text
         | Elem TagName [XML] -- A tagged element <t>...</t>
    deriving (Eq,Show)

tableToXML :: [[String]] -> XML
tableToXML xss = Elem "table" (map tableRow xss)
    where 
        tableRow xs = Elem "tr" (map tableData xs)
        tableData x = Elem "td" [Text x]

showXML :: XML -> String
showXML (Text txt)      = replaceIllegalSigns txt
showXML (Elem tag xs)   = "<" ++ tag ++ ">" ++ concat (map showXML xs) ++ "</" ++ tag ++ ">"

replaceIllegalSigns :: String -> String
replaceIllegalSigns [] = []
replaceIllegalSigns ('<':xs) = "&lt" ++ replaceIllegalSigns xs
replaceIllegalSigns ('&':xs) = "&amp" ++ replaceIllegalSigns xs
replaceIllegalSigns (_:xs) = replaceIllegalSigns xs

renderTable :: Show a => [[a]] -> String
renderTable = showXML . tableToXML . map (map show)