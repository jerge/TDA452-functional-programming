import Test.QuickCheck

exchangeRate = 10.316

toEUR sek = sek / exchangeRate

toSEK eur = eur * exchangeRate

prop_exchange eur = toEUR (toSEK eur) ~== eur

x ~== y = abs(x-y) < 1e-5

examplePair = (2,"hello")
exampleTriple = (0,42,"Hello")
exampleFunction (b,n,s) = if b then show n else s

dinner = ["Fish", "Chips", "Pudding", "Tomato"]

summary :: [String] -> String
summary [] = "Nothing"
summary [x] = "Only "++x
summary [x,y] = x++" and "++y
summary (x:xs) = x++" followed by more stuff and then "++last xs


len :: [a] -> Integer
len [] = 0
len (_:xs) = 1 + len xs

last' []        = error "last': empty list"
last' [x]       = x
last' (x:xs)    = last' xs

ex1 = [ x*x | x<-[1..10] ]

doubles xs = [ 2*x | x<-xs ]

ex2 = [ x | x <- [1..20], odd x]

pythag n = [(a,b,c) | a<-[1..n], b<-[a..n], c <-[b..n], a^2+b^2 == c^2]
