

module Proj1 (initialGuess, nextGuess, GameState) where

    import Data.List

    {-The deleteFirstsBy function takes a predicate and two lists
    and returns the first list with the first occurrence of each element of the second list removed.-}

    {-deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    deleteFirstsBy eq = foldl (flip (deleteBy eq))-}

    {-If judgeElement returns false, deleteFirstsBy shows the different element in the guess.
      If judgeElement returns true, deleteFirstsBy shows [].-}

    {-The description of the remaining possible chrods.-}
    data GameState = GameState { chords :: [[String]] }

    {-All the possible chords.-}
    possibleChords :: [[String]]
    possibleChords = [ [a]++[b]++[c] | a <- pitch, b <- pitch, c <- pitch, a/=b, b/=c, a/=c ]
                    where pitch = [a++b| a<-["A","B","C","D","E","F","G"], b<-["1","2","3"] ]

    {-First guess Initialisation, and update the GameState to remove the first guess.-}
    initialGuess :: ([String],GameState)
    initialGuess = (initialisation, (GameState (filterGuess initialisation possibleChords)))
            where initialisation = ["A1","B1","C1"]

    {-A filter for removing a current guess from possible chords.-}
    filterGuess :: [String] -> [[String]] -> [[String]]
    filterGuess currentGuess [] = []
    filterGuess currentGuess (g:gs) =
        if currentGuess == g then filterGuess currentGuess gs
        else g:filterGuess currentGuess gs

    {-Guess the next chord and collect the feedback. Return the remaining chords.-}
    nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
    nextGuess (previousGuess, (GameState possibleChords)) (pitch, note, octave) =
        (middleGuess, GameState (filterGuess middleGuess restChords))
        where restChords = [remaining | remaining <- possibleChords, targetGuess previousGuess remaining pitch note octave == True]
              {-Find the remaining possible chords.-}
              middleGuess = restChords !!((length restChords) `div` 2) {- Binary search for sorted possible chrods.-}

    {-Compare the pitch of previousGuess and remaining, then return the result.-}
    comparePitch :: [String] -> [String] -> Int
    comparePitch [] _ = 0
    comparePitch (g:gs) target =
        if g `elem` target then 1 + comparePitch gs target
        else comparePitch gs target

    {-Compare the note of previousGuess and remaining, then return the result.-}
    compareNote  :: [String] -> [String] -> Int
    compareNote [] _ = 0
    compareNote gs target = (length gs) - (length (deleteFirstsBy (judgeElement 0) gs target)) - comparePitch gs target

    {-Compare the octave of previousGuess and remaining, then return the result.-}
    compareOctave  :: [String] -> [String] -> Int
    compareOctave [] _ = 0
    compareOctave gs target = (length gs) - (length (deleteFirstsBy (judgeElement 1) gs target)) - comparePitch gs target


    {-Judge whether the ath elements in the two strings are equal, then return a boolean value to the deleteFirstsBy. -}
    judgeElement :: Eq a => Int -> [a] -> [a] -> Bool
    judgeElement a bs cs =
        if (bs !! a) == (cs !! a) then True
        else False

    {-Judge whether the guess is the target or filter the irrelevant chrods based on the feedback.-}
    targetGuess :: [String] -> [String] -> Int -> Int -> Int -> Bool
    targetGuess previousGuess remaining pitch note octave =
        if (comparePitch previousGuess remaining == pitch)
            && (compareNote previousGuess remaining == note)
            && (compareOctave previousGuess remaining == octave) then True
        else False