module Main  where
import Lib (codeImage, decodeImage, combineWordWithFileName, extractWordFromFileName)

main :: IO ()
main = do
  let bmpPath = "c:\\Users\\LOBAN\\Desktop\\Hs\\lab3\\src\\Ljapunov.bmp"
  let textPath = "c:\\Users\\LOBAN\\Desktop\\Hs\\lab3\\src\\Lyapunov.txt"
  putStrLn "Введите слово для шифра: "
  word <- getLine
  --let word = read wordStr :: Int
  putStrLn "Введите количество значащих бит для записи бит (от 1 до 8):"
  bitsStr <- getLine
  let bits = read bitsStr :: Int
  codeImage bmpPath textPath word bits
  putStrLn "\n Декодирование сообщения из изображения..."
  decodeImage bmpPath (combineWordWithFileName bmpPath word) textPath bits

{-
module Main where

import Lib (encodeImage, decodeImage, addShiftToFileName)

main :: IO ()
main = do
  -- putStrLn "Введите путь к изображению (.bmp):"
  -- bmpPath <- getLine
  -- putStrLn "Введите путь к текстовому файлу:"
  -- textPath <- getLine
  let bmpPath = "image.bmp"
  let textPath = "bio.txt"
  putStrLn "Введите смещение для шифра Цезаря:"
  shiftStr <- getLine
  let caesarShift = read shiftStr :: Int
  putStrLn "Введите количество значащих бит для стеганографии (от 1 до 8):"
  bitsStr <- getLine
  let bits = read bitsStr :: Int
  encodeImage bmpPath textPath caesarShift bits

  putStrLn "\nДекодирование сообщения из изображения..."
  decodeImage (addShiftToFileName bmpPath caesarShift) bits
-}