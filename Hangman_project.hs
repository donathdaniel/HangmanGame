import Data.Char
import Data.List

--initialazing the necessary types
type ABC = [Char]
abc :: ABC
abc = ['A'..'Z']

type Riddle       = String
type RightGuesses = [Char]
type WrongGuesses = [Char]
type State        = (Riddle, RightGuesses, WrongGuesses)

--necessarly things for printing the hangman
hangMan1="    ______"
hangMan2="   |      |"
hangMan3="   |      O"
hangMan4="   |     /|\\"
hangMan5="   |     / \\" 
hangMan6="___|___"

hangMan7=""
hangMan8="   |"
hangman9="   |      |"

hangmanFigures0 :: [String]
hangmanFigures0 = [hangMan7,hangMan7,hangMan7,hangMan7,hangMan7,hangMan6]
hangmanFigures1 :: [String]
hangmanFigures1 = [hangMan7,hangMan8,hangMan8,hangMan8,hangMan8,hangMan6]
hangmanFigures2 :: [String]
hangmanFigures2 = [hangMan1,hangman9,hangMan8,hangMan8,hangMan8,hangMan6]
hangmanFigures3 :: [String]
hangmanFigures3 = [hangMan1,hangman9,hangMan3,hangMan8,hangMan8,hangMan6]
hangmanFigures4 :: [String]
hangmanFigures4 = [hangMan1,hangman9,hangMan3,hangman9,hangMan8,hangMan6]
hangmanFigures5 :: [String]
hangmanFigures5 = [hangMan1,hangman9,hangMan3,hangMan4,hangMan8,hangMan6]
hangmanFigures6 :: [String]
hangmanFigures6 = [hangMan1,hangman9,hangMan3,hangMan4,hangMan5,hangMan6]

allFigures :: [[String]]
allFigures = [hangmanFigures0,hangmanFigures1,hangmanFigures2,hangmanFigures3,hangmanFigures4,hangmanFigures5,hangmanFigures6]

--actually prints the hangman
printFigure :: [String] -> IO (Maybe a)
printFigure [] = return Nothing
printFigure (k: ve) = do
    putStrLn $ id k
    printFigure ve

--checking that the specified letter is valid or not
--isValidLetter 'E' ['a'..'z'] -> True
isValidLetter :: Char -> ABC -> Bool
isValidLetter letter abc = any ((toUpper letter)==) abc || any ((toLower letter)==) abc

--create a tree element tuple, first is the riddle word, second is the right guessed letters, and the third is the wrong guessed letters(initially the second and the third element is empty)
--startState abc "SOS" -> ("SOS", [], [])
startState :: ABC -> String -> State
startState abc word
    | aux abc word == True = (toUpperString word, [], [])
    | otherwise = error "the riddle contains invalid letters"
        where
            aux :: ABC -> String -> Bool
            aux abc [] = True
            aux abc (k:ve)
                | isValidLetter k abc = aux abc ve
                | k == ' ' = aux abc ve
                | otherwise = False

--make capitalization with a string
--toUpperString "sos" -> "SOS"
toUpperString :: String -> String
toUpperString [] = []
toUpperString (k: ve) = [toUpper k] ++ toUpperString ve 

--after a guessed letter modify the game
--guessLetter abc 'v' ("SOS", "A", []) -> ("SAVE OUR SOULS", "VA", [])
guessLetter :: ABC -> Char -> State -> State
guessLetter abc letter currentState
    | isValidLetter letter abc == False = error "invalid guessed letter"
    | isValidLetter letter (getRightGuesses currentState) || isValidLetter letter (getWrongGuesses currentState) = currentState
    | isValidLetter letter (getRiddle currentState) = ((getRiddle currentState),[toUpper letter] ++ (getRightGuesses currentState) ,(getWrongGuesses currentState))
    | otherwise = ((getRiddle currentState),(getRightGuesses currentState),[toUpper letter] ++ (getWrongGuesses currentState))

--get the elements of the tuple
getRiddle :: State -> Riddle
getRiddle (a,_,_) = a

getRightGuesses :: State -> RightGuesses
getRightGuesses (_,a,_) = a

getWrongGuesses :: State -> WrongGuesses
getWrongGuesses (_,_,a) = a

--show the riddle of the word
--showRiddle ("SOS", "SO", "AL") -> "SOS"
showRiddle :: State -> String
showRiddle currentState = aux (getRiddle currentState) (getRightGuesses currentState)
    where
        aux :: Riddle -> RightGuesses -> String
        aux [] rightGuesses = []
        aux (k: ve) rightGuesses
            | isValidLetter k rightGuesses = [k] ++ (aux ve rightGuesses)
            | k == ' ' = [k] ++ (aux ve rightGuesses)
            | otherwise = ['_'] ++ (aux ve rightGuesses)

--showState ("SOS", [], []) -> ("___", [], [])
showState :: State -> State
showState currentState = (showRiddle currentState, (getRightGuesses currentState), (getWrongGuesses currentState))

--checks that the player guessed the riddle word
--isRiddleComplete ("SOS", "SALO", []) -> True
isRiddleComplete :: State -> Bool
isRiddleComplete currentState = aux (getRiddle currentState) (getRightGuesses currentState)
    where
        aux :: Riddle -> RightGuesses -> Bool
        aux [] rightGuesses = True
        aux (k: ve) rightGuesses
            | isValidLetter k rightGuesses = aux ve rightGuesses
            | k == ' ' = aux ve rightGuesses
            | otherwise = False

--checks that if the game is over, if the player guessed the riddle word or the the player guessed wrong more than five letter 
--isGameOver ("SOS", [], "LKHJIG") -> True
isGameOver :: State -> Bool
isGameOver currentState = isRiddleComplete currentState || length (getWrongGuesses currentState) > 5

--show the available letters
--getAvailableLetters abc ("SOS", [], "LKHJIG") -> "ABCDEFMNOPQRSTUVWXYZ"
getAvailableLetters :: ABC -> State ->  [Char]
getAvailableLetters abc currentState = minusArray abc (removeDuplicates $ mergeArray (getRightGuesses currentState) (getWrongGuesses currentState))

--merge 2 array
mergeArray :: Ord a => [a] -> [a] -> [a]
mergeArray [] x = x
mergeArray x [] = x
mergeArray (x:xs) (y:ys) | y < x     = y : mergeArray (x:xs) ys
mergeArray (x:xs) (y:ys) | otherwise = x : mergeArray xs (y:ys)

--remove duplicates from an array
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

--make minus operation between 2 array
minusArray :: String -> String -> String
minusArray [] guessLetters = []
minusArray (k: ve) guessLetters
    | any (k==) guessLetters == False = [k] ++ minusArray ve guessLetters
    | otherwise = minusArray ve guessLetters

--with the letters and the start state of the game, play 
--play abc "SAK" ("SOS",[],[]) -> ("S_S","S","KA")
play :: ABC -> [Char] -> State -> State
play abc [] currentState = showState currentState
play abc (k: ve) currentState
    | isGameOver currentState = showState currentState
    | otherwise = play abc ve (guessLetter abc k currentState) 

--make feedback from the current state of the game
--evaluatePlay abc "SAK" ("SOS",[],[]) -> "The game is pending! Current state is: (\"S_S\",\"S\",\"KA\")"
evaluatePlay :: ABC -> [Char] -> State -> String
evaluatePlay abc guessLetters currentState
    | isRiddleComplete $ play abc guessLetters currentState = "The player completed the word. Congratulations! The solutions was: " ++ (getRiddle currentState)
    | isGameOver $ play abc guessLetters currentState = "Game Over. Try a new game!"
    | otherwise = "The game is pending! Current state is: " ++ (show $ play abc guessLetters currentState)


--actual main for the game with a word from stdin
mainFromStdin :: IO (Maybe a)
mainFromStdin = do
    putStr "Insert the word: "
    theWord <- getLine
    readLetters abc "" $ startState abc theWord
    return Nothing

readLetters :: ABC -> [Char] -> State -> IO (Maybe a)
readLetters abc guessLetters currentState = do
    print $ evaluatePlay abc guessLetters currentState
    printFigure $ allFigures !! (length (getWrongGuesses $ play abc guessLetters currentState))
    if isGameOver $ play abc guessLetters currentState then return Nothing
    else do 
        putStr "Give a letter: "
        letter <- getLine
        if letter == []
        then readLetters abc guessLetters currentState
        else do readLetters abc (guessLetters ++ [stringToChar $ take 1 letter]) currentState

stringToChar :: String -> Char
stringToChar [c] = c

--actual main for the game with a word from a txt 
mainFromFile :: IO (Maybe a)
mainFromFile = do
    theWord <- readFile "hangman_in.txt"
    readLetters abc "" $ startState abc theWord

