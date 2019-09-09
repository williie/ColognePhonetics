module Main where 
import Data.Char

{--Buchstabe 	Kontext 	Code
A, E, I, J, O, U, Y 		0
H 		                –
B 		                1
P 	nicht vor H
D, T 	nicht vor C, S, Z 	2
F, V, W 		        3
P 	vor H
G, K, Q 		        4
C 	im Anlaut vor A, H, K, L, O, Q, R, U, X
vor A, H, K, O, Q, U, X außer nach S, Z
X 	nicht nach C, K, Q 	48
L 		5
M, N 		6
R 		7
S, Z 		8
C 	nach S, Z
im Anlaut außer vor A, H, K, L, O, Q, R, U, X
nicht vor A, H, K, O, Q, U, X
D, T 	vor C, S, Z
X 	nach C, K, Q 

  1.  Buchstabenweise Kodierung von links nach rechts entsprechend der Umwandlungstabelle.
  2.  Entfernen aller mehrfach nebeneinander vorkommenden Ziffern.
  3.  Entfernen aller Codes „0“ außer am Anfang.

--}


-- $ is right associative
cologne :: String -> String
cologne [] = []
cologne x = firstLetter $ lowerLetters x

--lower the first letter in the string 
lowerLetters :: String -> String
lowerLetters [] = []
lowerLetters (x:xs) =
      if isUpper x 
         then toLower x:xs
         else x:xs

-- checks if first letter is 0
firstLetter :: String -> String
firstLetter (x:xs) 
   | x == 'a' = '0' : rules xs
   | x == 'o' = '0' : rules xs
   | x == 'e' = '0' : rules xs
   | x == 'i' = '0' : rules xs
   | x == 'u' = '0' : rules xs
   | x == 'y' = '0' : rules xs
   | x == 'j' = '0' : rules xs
   | x == 'ö' = '0' : rules xs
   | x == 'ä' = '0' : rules xs
   | x == 'ü' = '0' : rules xs
   | x == 'ß' = '0' : rules xs
   | otherwise =  rules ([x] ++ xs)

rules :: String -> String
rules [] = []
rules (x:xs) 
    | x == 'h' =  rules xs
    | (x == 'b' || x == 'p') = '1' : rules xs
    | (x == 'd' || x == 't') = '2' : rules xs
    | (x == 'f' || x == 'v' || x == 'w') = '3' : rules xs
    | (x == 'g' || x == 'k' || x == 'q') = '4' : rules xs
    | x == 'l' = '5' : rules xs
    | (x == 'm'|| x == 'n') = '6' : rules xs
    | x == 'r' = '7' : rules xs
    | ( x == 's' || x == 'z') = '8' : rules xs
    | (x == 'a' || x == 'o' || x == 'e' || x == 'i' || x == 'u' || x == 'y' || x == 'j' || x == 'ö' || x == 'ä' || x == 'ü' || x =='ß') = '0' : rules xs
    | otherwise = "error or not implemented yet"
