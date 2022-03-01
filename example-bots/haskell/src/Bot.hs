{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot
  where

import GHC.Generics
import Data.Text(Text)
import Data.Aeson(ToJSON, FromJSON)
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import Data.Vector(Vector)
import Data.JsonStream.Parser(Parser, value, runParser', ParseOutput(ParseYield, ParseNeedData, ParseFailed, ParseDone))
import System.IO(hPutStrLn, hFlush, stderr, stdin, stdout)

import qualified Data.ByteString as B

import RenameFields

data Coord = Coord {
  x :: Int,
  y :: Int
  } deriving (Generic, Show)

instance ToJSON Coord
instance FromJSON Coord

data Player = Player {
  money :: Int,
  base :: Coord,
  health :: Int
  } deriving (Generic, Show)

instance ToJSON Player
instance FromJSON Player

data MapEntry = MapEntry {
  block_type :: Text,
  position :: Coord,
  team :: Maybe Text
  } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = mapEntryFieldRename } ''MapEntry)

data State = State {
  you :: Maybe Text,
  map :: Vector MapEntry,
  turns_remaining :: Int,
  player1 :: Player,
  player2 :: Player
} deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = stateFieldRename } ''State)

runBot :: IO ()
runBot = do
  putStrLn "Ready"
  hFlush stdout
  processTurns nextChunkFromStdIn blockingByteFromStdIn

processTurns :: IO (B.ByteString) -> IO (B.ByteString) -> IO ()
processTurns nextChunk nextByteBlocking = do
  nextInput <- nextByteBlocking
  iter $ runParser' stateParser nextInput
  where
    iter :: ParseOutput State -> IO ()
    iter (ParseDone unused) = do
      nextInput <- nextByteBlocking
      iter $ runParser' stateParser $ B.append unused nextInput
    iter (ParseYield state nextResult) = do
      putStrLn $ computeMove state
      hFlush stdout
      iter nextResult
    iter (ParseFailed parseError) = do
      hPutStrLn stderr parseError
      nextInput <- nextByteBlocking
      iter $ runParser' stateParser nextInput
    iter (ParseNeedData processNextChunk) = do
      nextInput <- nextChunk
      iter $ processNextChunk nextInput

stateParser :: Parser State
stateParser = value

nextChunkFromStdIn :: IO (B.ByteString)
nextChunkFromStdIn = B.hGetNonBlocking stdin 5000

blockingByteFromStdIn :: IO (B.ByteString)
blockingByteFromStdIn = B.hGet stdin 1

computeMove :: State -> String
computeMove state =
  if money myPlayer >= 10
  then let (Coord myX myY)       = base myPlayer
           (Coord otherX otherY) = base otherPlayer
       in "Build Scout (" ++
          show myX ++
          ", " ++
          show (myY + 1) ++
          ") (" ++
          show otherX ++
          ", " ++
          show otherY ++
          ") Down"
  else "No-op"
  where
    myPlayer = (if you state == Just "player1" then player1 else player2) state
    otherPlayer = (if you state == Just "player1" then player2 else player1) state
