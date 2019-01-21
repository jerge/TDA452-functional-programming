import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe

chat :: (Eq b,Num b) => b -> (a->a) -> [a] -> [a]
chat 0 f (x:xs) = f x : xs
chat _ _ []     = []
chat n f (x:xs) = x:chat (n-1) f xs

chat' _ _ [] = []
chat' n f xs | length xs <= n = xs
             | otherwise = l ++ f r : rs
    where (l,(r:rs)) = splitAt n xs

prop_chatChat' :: Int -> [Int] -> Property
prop_chatChat' n xs = n >= 0 ==> chat n (+7) xs == chat' n (+7) xs

findIn :: Eq a => [a] -> [a] -> Maybe Int
findIn a b = findin' a b 0

findin' :: Eq a => [a] -> [a] -> Int -> Maybe Int
findin' a [] n = Nothing
findin' a (x:xs) n | a `isPrefixOf` (x:xs) = Just n
                   | otherwise = findin' a xs (n+1)

prop_findIn0 = findIn "Hell" "Hello"      == Just 0
            && findIn "ell" "Hello Jello" == Just 1
            && findIn "Hell" "Helan"      == Nothing

prop_qcFindIn :: (Eq a) => [a] -> [a] -> Int -> Property
prop_qcFindIn xs ys n | n > length xs = not (null ys) ==> not $ isNothing (findIn xs (ys++xs))
                      | otherwise = not (null ys) ==> not $ isNothing (findIn xs (ly++xs++ry))
    where (ly,ry) = splitAt n ys

type Journey = [Leg]

type Place = String

data Leg = Train (Place, Place) | Flight (Place, Place) | Bus (Place, Place)
    deriving (Eq,Show)

connected :: Journey -> Bool
connected j = and $ map (startIsEnd) ends
    where ends = zip (init j) (tail j)
          startIsEnd (Train (l1,l2),Train (r1,r2)) = l2 == r1

data Map = Map PlaceName [(Dir,Map)]
data Dir = N | S | E | W    deriving (Eq,Show)
type PlaceName = String

hogwarts = Map "Castle" [(N,forest),(S,lake)]
forest  = Map "Forest" [(S,hogwarts)]
lake    = Map "Lake"  [(N,hogwarts)]

travel :: Map -> [Dir] -> Maybe Map
travel m [] = Just m
travel (Map p dm) (d:[]) = lookup d dm
travel (Map p dm) (d:ds) | isNothing m  = Nothing
                         | otherwise    = travel (fromJust m) ds
    where m = lookup d dm

-- we will recursively print forest -> hogwarts -> forest and so on



instance Show Map where
    show (Map place dm) = "You are at the " ++ place ++ ". " ++ sdm dm  ++ concatMap (showMap' [place]) (map snd dm)

showMap' prevPlaces (Map place dm)
    | elem place prevPlaces = ""
    | otherwise = place ++ ". " ++ sdm dm ++ concatMap (showMap' (place : prevPlaces)) (map snd dm)

sdm dm = concat (intersperse ", " (map showDirMap dm)) ++ "\n"

showDirMap (d, (Map place m)) = "Go " ++ show d ++ " to " ++ place


for_ :: [a] -> (a -> IO()) -> IO()
for_ [] _ = return ()
for_ (x:xs) f = do f x
                   for_ xs f
                   return ()

for :: [a] -> (a -> IO b) -> IO [b]
for (x:[]) f = do r <- f x
                  return ([r])
for (x:xs) f = do r <- f x
                  rs <- for xs f
                  return (r:rs)

join :: FilePath -> Int -> IO ()
join f n = do files <- for [1..n] (rf f)
              writeFile f (concat files)
              return ()
    where rf f i = readFile (f ++ ".part" ++ show i)

