{-
   Alec Webb
   
   Musical Chairs:
   There are N players and (N-1) chairs. There is also an emcee who determines who loses a round. 
   Music is broadcast (just starting and stopping signals to some shared state will suffice), and once 
   the music stops each player can try to sit in a chair (which is an individual resource). Only one 
   person can sit in a chair at a time, so a player will keep attempting to sit in the available chairs 
   via some algorithm of your choosing. Once all seats are occupied, the emcee somehow informs that player 
   that they have lost (via shared state, or (eek!) killing the thread). The players may not attempt to 
   sit before the music has stopped.

   Can be compiled with:
      ghc -o MusicalChairs MusicalChairs.hs
   Run with:
	  ./MusicalChairs <optional #_players>
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import System.Random
import System.Environment

-- Crashing on BoundedChan? Download the hackage package or comment out that segment.
import qualified Control.Concurrent.BoundedChan as BC
import Control.Concurrent.Broadcast
import qualified Control.Concurrent.Broadcast as Broadcast

player :: Broadcast Int -> Int -> BC.BoundedChan Int -> BC.BoundedChan String ->  IO ()
player music tNum c outputs = do
    msg <- listen music

    if msg == 2
        then do
            BC.writeChan outputs $ "\nP" ++ (show tNum) ++ " wins!"
            return ()
        else do
            interval <- getStdRandom (randomR (1,1000))
            threadDelay (interval * 1000)
            v <- BC.tryReadChan c
            if v == Nothing
                then do
                    BC.writeChan outputs $ "\t\tP" ++ (show tNum) ++ " Loses"
                    return ()
                else do
                    BC.writeChan outputs $ "\t\tP" ++ (show tNum) ++ " found a seat."
                    silence music
                    player music tNum c outputs

-- sets up chairs
produce :: String -> Int -> Int -> BC.BoundedChan Int -> BC.BoundedChan String -> IO ()
produce name next max c outputs | next > max = return ()
                                | otherwise = do
    BC.writeChan c next
    produce name (next+1) max c outputs

-- displays messages
announcer :: Int -> BC.BoundedChan String -> IO ()
announcer 0 _ = return ()
announcer n c = do
    v <- BC.readChan c
    putStrLn $ v
    announcer (n-1) c

------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs

    --if args == [] then putStrLn $ "Usage ./Project3 <#_players>\n" else putStrLn $ ""

    if args == [] 
    then do
    let nPlayers = 10
    let nChairs = nPlayers - 1

    chairs <- BC.newBoundedChan nChairs
    outputs  <- BC.newBoundedChan nPlayers
    putStrLn $ "Num. players = "++(show nPlayers) ++ "\nNum. chairs = "++(show nChairs)++"\n"

    music <- Broadcast.new

    silence music

    spawnThreads music nPlayers chairs outputs
    playRound music 1 nPlayers nChairs chairs outputs

    broadcast music 2    -- announce that game is over to surviving thread
    announcer 1 outputs

    else do
    let nPlayers = read (args!!0) :: Int
    let nChairs = nPlayers - 1

    chairs <- BC.newBoundedChan nChairs
    outputs  <- BC.newBoundedChan nPlayers
    putStrLn $ "Num. players = "++(show nPlayers) ++ "\nNum. chairs = "++(show nChairs)++"\n"

    music <- Broadcast.new

    silence music

    spawnThreads music nPlayers chairs outputs
    playRound music 1 nPlayers nChairs chairs outputs

    broadcast music 2    -- announce that game is over to surviving thread
    announcer 1 outputs

-- creates player threads
spawnThreads :: Broadcast Int -> Int -> BC.BoundedChan Int -> BC.BoundedChan String ->  IO ()
spawnThreads _ 0 _ _ = return ()
spawnThreads music n chairs outputs = do
    forkIO $ player music n chairs outputs
    spawnThreads music (n-1) chairs outputs

-- runs each round
playRound :: Broadcast Int -> Int -> Int -> Int -> BC.BoundedChan Int -> BC.BoundedChan String -> IO ()
playRound _ _ _ 0 _ _ = return ()
playRound music round nPlayers nChairs chairs outputs = do
    putStrLn $ "Round " ++ (show round)
    putStrLn $ "Starting the music ..."
    produce "MC" 1 nChairs chairs outputs
    putStrLn $ "Chairs available: " ++ (show nChairs)
    putStrLn $ "Stopping the music ..."
    broadcast music 1
    announcer nPlayers outputs
    playRound music (round + 1) (nPlayers - 1) (nChairs - 1) chairs outputs

