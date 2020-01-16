module Main where

import Lib (readData, deleteData, addSchema, updateData, insertData)

main = do 
  putStrLn "Enter your operation "
  command <- getLine
  result <- case command of
    "Read" -> readData
    "Delete" -> deleteData
    "AddSchema" -> addSchema
    "Update" -> updateData
    "Insert" -> insertData
    whatever -> return 1
  
  case result of 
    0 -> main
    x -> return 1
  putStrLn "exit from function"
  return 1