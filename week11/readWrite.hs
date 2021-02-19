import System.IO
import System.Directory
import Data.List


main = do 
	contents <- readFile "todo.txt"
	(tempName, tempHandle) <- openTempFile "." "temp"
	let todoTasks = lines contents
	    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
	putStrLn "These are your TO-DO items:"
	putStr (unlines numberedTasks)
	putStrLn "Which one to delete?"
	numberedString <- getLine
	let number = read numberedString
	    newTodoItems = delete (todoTasks !! number) todoTasks
	hPutStr tempHandle (unlines newTodoItems)
	hClose tempHandle
	removeFile "todo.txt"
	renameFile tempName "todo.txt"

{-
	main = do     
        contents <- readFile "girlfriend.txt"     
        writeFile "girlfriendcaps.txt" (map toUpper contents) }
-}