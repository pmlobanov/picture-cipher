module Lib
    (codeImage, decodeImage, combineWordWithFileName, extractWordFromFileName) where

import qualified Data.ByteString.Char8 as BC
import Data.Bits
import Data.Char (toUpper, toLower, isAlpha, isLower, isUpper)
import System.FilePath (takeBaseName, takeExtension, replaceFileName)
import Data.List(nub, elemIndex)
import Data.Maybe(fromJust)

{-# LANGUAGE OverloadedStrings #-}

data Register = High | Low deriving(Show, Eq)

--получаем кодированный алфавит, сцепелнный с индексом
getReplacemnetAlphabet:: String -> Register ->  String
getReplacemnetAlphabet keyWord reg | reg == High = nub(map toUpper keyWord ++ ['A'..'Z'])
                                   | reg == Low = nub(map toLower keyWord ++ ['a'..'z'])
                                   | otherwise = " "

--кодируем
encode:: String -> String -> String
encode text keyWord =  map getsymbol text
    where 
        highAlphabet = getReplacemnetAlphabet keyWord High
        lowAlphabet = getReplacemnetAlphabet keyWord Low
        getsymbol c | isAlpha c && isUpper c  = highAlphabet !! fromJust (elemIndex (toUpper c) ['A'..'Z'] ) 
                    | isAlpha c && isLower c  = lowAlphabet !! fromJust (elemIndex (toLower c) ['a'..'z'] )
                    | otherwise = c

--декодируем
decode :: String->String ->String
decode text keyWord = map getdecodeSymbol text
    where 
        getdecodeSymbol c | isAlpha c && isUpper c  = ['A'..'Z'] !! fromJust (elemIndex (toUpper c) (getReplacemnetAlphabet keyWord High) ) 
                          | isAlpha c && isLower c  = ['a'..'z'] !! fromJust (elemIndex (toLower c) (getReplacemnetAlphabet keyWord Low) )
                          | otherwise = c
--делаем из чара биты
charToBits :: Char -> [Int]
charToBits char = [if testBit (fromEnum char) i then 1 else 0 | i <- [7,6..0]]

bitsToChar :: [Int] -> Char
bitsToChar bits
  | length bits /= 8 = ' '--error "Invalid number of bits: expected exactly 8"
  | otherwise        = toEnum (sum [b * 2^bitIndex | (b, bitIndex) <- zip bits [7,6..0]])

--делаем из нашего текста биты.
stringToBits :: [Char] -> [[Int]]
stringToBits = map charToBits

bitsToString :: [[Int]] -> String
bitsToString = map bitsToChar

replaceBits :: Int -> [[Int]] -> [Int] -> [[Int]]
replaceBits _ bytes [] = bytes  -- Если codedBytes пустой, возвращаем исходный список bytes
replaceBits _ [] _ = []          -- Если bytes пустой, возвращаем пустой список
replaceBits 0 bytes _ = bytes         -- Если n равно 0, ничего не заменяем
replaceBits n (b:bs) codedBytes = newByte : replaceBits n bs (drop n codedBytes)
    where 
        newByte = changeBitsInByte b (numerateBits(take n codedBytes))  -- Изменяем 

numerateBits:: [Int] -> [(Int, Int)]
numerateBits bytes = zip (reverse bytes) [1..]
{--}
changeBitsInByte::  [Int] -> [(Int, Int)] ->[Int]
changeBitsInByte iBytes [ ]  = iBytes
changeBitsInByte [ ] _  = [ ]
changeBitsInByte iBytes rBytes = changeBitsInByte next (tail rBytes)
    where 
        (newBit, position) = head rBytes
       -- l = length iBytes
        next = mconcat[take (8 - position) iBytes, [newBit], drop (8+1 -position) iBytes]

-- Функция для объединения слова с именем файла
combineWordWithFileName :: FilePath -> String -> FilePath
combineWordWithFileName filePath word =
    let baseName = takeBaseName filePath  -- Получаем имя файла без расширения
        ext = takeExtension filePath       -- Получаем расширение файла
        newFileName = word++ "_" ++ baseName ++ ext  -- Создаем новое имя файла
    in replaceFileName filePath newFileName  -- Заменяем имя файла в исходном пути

extractWordFromFileName :: FilePath -> FilePath -> Maybe String
extractWordFromFileName originalPath newPath =
    let originalBaseName = takeBaseName originalPath  -- Имя файла без расширения
        originalExt = takeExtension originalPath      -- Расширение файла
        newBaseName = takeBaseName newPath            -- Имя нового файла без расширения
        newExt = takeExtension newPath                 -- Расширение нового файла
    in if newExt == originalExt && newBaseName /= originalBaseName
        then Just (takeWhile (/= '_')  newBaseName) -- Извлекаем слово
        else Nothing  -- Если условия не выполняются, возвращаем Nothing

--пишем в картинку
codeImage:: FilePath -> FilePath -> String-> Int ->IO()
codeImage imagePath textPath word bits = do
    -- cчитали картинку
    imageFile <- BC.readFile imagePath
    let (headerImageFile, tailImageFile) = BC.splitAt 54 imageFile
    let dataBytes = BC.unpack tailImageFile
    -- считываем текст 
    textFile <- BC.readFile textPath 
    let encryptedText = encode (BC.unpack textFile) word
    --let decryptedText = decode encryptedText word
    -- перевод текста в биты
    print (encryptedText)
    let textBits =  mconcat (stringToBits encryptedText)
    let maxBits = (length dataBytes) * bits
    let maxBit2 = (length textBits)*8
 --   print ( maxBits, maxBit2 , length dataBytes)
    if maxBit2 > maxBits then
        putStrLn
          "Ошибка: сообщение слишком длинное для данного изображения."
    else do
        let newImageDataBytes = replaceBits bits (stringToBits dataBytes) (textBits)
      --  print (drop (length dataBytes-5) (stringToBits dataBytes))
       -- print (drop (length dataBytes-5) (newImageDataBytes))
        let newImageFile = BC.concat [headerImageFile, BC.pack (map bitsToChar newImageDataBytes)]
        let newImagefileName = combineWordWithFileName imagePath word
        BC.writeFile newImagefileName newImageFile
        putStrLn $ "Искаженное изображение сохранено в файл:" ++ newImagefileName
      --  print (drop (length dataBytes-5) (dataBytes))
     --   print (drop (length dataBytes-5) (bitsToString newImageDataBytes))
      --  print (take (5) (dataBytes))
     --   print (take (5) (bitsToString newImageDataBytes))
extractBits:: Int ->[[Int]] ->  [Int]
extractBits n [ ] = [ ] 
extractBits n bytes  = drop (8-n) (head bytes) ++ extractBits n (tail bytes) 

getBytes:: [Int] -> [[Int]]
getBytes [] = []
getBytes bytes = take 8 bytes : getBytes (drop 8 bytes)

decodeImage :: FilePath-> FilePath-> FilePath-> Int-> IO ()
decodeImage originalbmpPath bmpPath textPath bits = do
    text <-readFile textPath
    let lengthText = (length text) * 8--Длина сообщения в битах
  --  print (length text)
    imageFile <- BC.readFile bmpPath
    --Разделение на заголовок и данные изображения
    let (header, imageData) = BC.splitAt 54 imageFile
    let numMessageByte = (lengthText + bits -1) `div` bits --Берем только байты, в которых текст
   -- print (numMessageByte)
    let (messageByte, other) = BC.splitAt numMessageByte imageData
   
    --Преобразуем данные изображения в список байт
    let imageDataBytes = BC.unpack messageByte 
   -- print (length imageDataBytes)
    --Извлекаем биты из данных изображения
    let extractedBits = bitsToString (getBytes (extractBits bits (stringToBits imageDataBytes)))
   -- print (drop (length extractedBits-1 ) extractedBits)
   -- print (take 1 extractedBits)
  --  print (length  (extractBits bits (stringToBits imageDataBytes)))
    --Дешифровываем строку 
    let word = extractWordFromFileName originalbmpPath bmpPath 
   -- print (word)
    --print(length extractedBits)
    --let decodedText = decode (take (length extractedBits-1) extractedBits)(fromJust word)
    let outputPath = combineWordWithFileName textPath "decoded"
    --print (decodedText){- -}
    writeFile outputPath (decode (take (length extractedBits-1) extractedBits)(fromJust word))
    putStrLn $ "Декодированное сообщение сохранено в файл: " ++ outputPath

