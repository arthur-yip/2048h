module ConsoleUI where

import Game
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Data.Char
import Text.Printf

styles = 
  convertStyles [
    Style WhiteF BlackB,
    Style BlackF WhiteB
  ]

nthStyle n = do
  ss <- styles
  return $ ss !! n

basicStyle = nthStyle 0
cellStyle = nthStyle 1

boardX = 5
boardY = 2
cellWidth = 7
cellHeight = 4

drawCell :: Int -> Int -> Int -> IO ()
drawCell x y num = do
  style <- cellStyle
  setStyle style
  mvWAddStr stdScr ((y * cellHeight) + boardY) ((x * cellWidth) + boardX) "      "
  mvWAddStr stdScr ((y * cellHeight) + boardY + 1) ((x * cellWidth) + boardX) (printf "%5d " num)
  mvWAddStr stdScr ((y * cellHeight) + boardY + 2) ((x * cellWidth) + boardX) "      "

drawBoard :: [Int] -> IO ()
drawBoard game = do
  erase
  let seq = [ drawCell x y (game !! (y * 4 + x)) | x <- [0..3], y <- [0..3] ]
  sequence_ seq
  refresh

startGame = do
  game <- initGame
  initCurses
  keypad stdScr True
  echo False
  gameLoop game
  endWin

gameLoop game = do
  drawBoard game
  if lose game
    then drawMessage "Game Over"
    else if win game
      then drawMessage "Win"
      else do
        result <- takeInput game
        case result of
          Nothing -> drawMessage "Bye"
          Just g -> if game == g
                      then gameLoop game
                      else do
                        g' <- place g
                        gameLoop g'

drawMessage msg = do
  s <- basicStyle
  setStyle s
  mvWAddStr stdScr 4 40 msg
  wMove stdScr 20 0
  refresh

takeInput :: [Int] -> IO (Maybe [Int])
takeInput game = do
  k <- getCh
  case k of
    KeyUp -> return (Just $ shiftUp game)
    KeyLeft -> return (Just $ shiftLeft game)
    KeyDown -> return (Just $ shiftDown game)
    KeyRight -> return (Just $ shiftRight game)
    KeyChar c -> if (toLower c) == 'q' then return Nothing else takeInput game
    _ -> takeInput game
