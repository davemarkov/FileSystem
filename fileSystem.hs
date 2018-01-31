import System.IO

type FileName = String
type FileData = String
type DirName = String
data FileSystem = Empty | FILE (FileName, FileData) | DIR DirName [FileSystem]
    deriving (Eq,Ord,Show)

filesystem :: FileSystem
filesystem = DIR "/" 
                    [DIR "topSecret" 
                            [FILE ("file1.txt", "This is the content for file1"),
                             FILE ("file2.txt", "This is the content for file2")],
                    DIR "moreTopSecret" 
                            [DIR "src"  
                                    [DIR "hot" 
                                            [FILE ("cold.f","prise is not here! go back :)"),
                                             DIR "cold" 
                                                    [FILE ("tooo.cold","its freasing man?!")],
                                             DIR "wormer" 
                                                    [FILE ("lava.b","sorry you burned")]]],
                            DIR "dev" 
                                    [FILE ("main.c","#include <stdio.h>\n int main(int argc,char* argv[])\n{\n  printf('hello world\n');\n}\n  exit(0);"),
                                     FILE ("usefull.h","empty;")],
                            FILE ("DONOTOPEN.txt","theEnd")],                        
                    FILE ("do_not_open_this.forbid", "Didn't tell you something!?!?")]

getDir :: [String] -> FileSystem -> FileSystem
getDir [name] (DIR dirname ls)
    | name == dirname = DIR dirname ls
    | otherwise       = Empty
getDir _ (FILE _) = Empty
getDir _ Empty = Empty
getDir (name:rest) (DIR dirname ls)
    | name == dirname = checkChildren ls
    | otherwise       = Empty
        where checkChildren [] = Empty
              checkChildren (x:xs) = let dir = getDir rest x 
                                         in if dir == Empty then checkChildren xs else dir

getFileData :: [String] -> FileSystem -> String
getFileData [] _ = []
getFileData [name] (FILE (filename,filedata))
    | name == filename = filedata
    | otherwise        = []
getFileData _ (FILE _) = []
getFileData _ Empty = []
getFileData (name:rest) (DIR dirname ls)
    | name == dirname = checkChildren ls
    | otherwise       = []
        where checkChildren [] = []
              checkChildren (x:xs) = let fd = getFileData rest x 
                                         in if null fd then checkChildren xs else fd  

readFromFiles :: [String] -> [String] -> FileSystem -> String
readFromFiles [] _ _ = []
readFromFiles (filename:files) path fs = readFromFiles files path fs ++ getFileData (checkPath filename path) fs

ifDir :: FileSystem -> Bool
ifDir (DIR _ _) = True
ifDir _ = False

hasDIRs :: [FileSystem] -> Bool
hasDIRs ls = not (and [ not(ifDir x) | x<-ls ])

removePathDir :: [String] -> FileSystem -> FileSystem
removePathDir [name] (DIR dirname ls)
    | name == dirname && not(hasDIRs ls) = Empty
    | otherwise       = DIR dirname ls
removePathDir _ Empty = Empty
removePathDir _ (FILE (filename,filedata)) = FILE (filename,filedata) 
removePathDir (name:rest) (DIR dirname ls)
    | name == dirname = DIR dirname (removeFromList ls)
    | otherwise       = DIR dirname ls
    where removeFromList [] = [] 
          removeFromList (x:xs) = let s = removePathDir rest x 
                                      in if s == Empty then removeFromList xs
                                         else s : removeFromList xs

removePathFile :: [String] -> FileSystem -> FileSystem
removePathFile [name] (FILE (filename,filedata))
    | name == filename = Empty
    | otherwise        = FILE (filename,filedata)
removePathFile _ Empty = Empty
removePathFile _ (FILE (filename,filedata)) = FILE (filename,filedata) 
removePathFile (name:rest) (DIR dirname ls)
    | name == dirname = DIR dirname (removeFromList ls)--[ removePathFile rest node | node <- ls]
    | otherwise       = DIR dirname ls
        where removeFromList [] = [] 
              removeFromList (x:xs) = let s = removePathFile rest x 
                                          in if s == Empty then removeFromList xs
                                             else s : removeFromList xs

separateWords :: String -> Char -> [String]
separateWords [] _ = []
separateWords (x:xs) separator 
    | x == separator = [] : rest
    | otherwise      = separate rest
        where rest = separateWords xs separator
              separate [] = [[x]]  
              separate (prevWord:words) = (x : prevWord) : words

separateDirectories :: String -> [String]
separateDirectories [] = []
separateDirectories (x:xs)
    | x == '/'  = ["/"] ++ separateWords xs '/'
    | otherwise = separateWords (x:xs) '/'  

createDir :: [String] -> String -> FileSystem -> FileSystem
createDir _ [] fs = fs
createDir _ _ Empty = Empty
createDir _ _ (FILE (name,fileData)) = FILE (name,fileData)
createDir [x] newdirname (DIR dirname ls)
    | x == dirname = DIR dirname (DIR newdirname []:ls)
    | otherwise    = DIR dirname ls
createDir (x:xs) newdirname (DIR dirname ls)
    | x == dirname = DIR dirname [ createDir xs newdirname child | child <- ls ]
    | otherwise    = DIR dirname ls 

createFile :: String -> [String] -> String -> FileSystem -> FileSystem
createFile _ _ _ Empty = Empty 
createFile _ _ _ (FILE (name,fileData)) = FILE (name,fileData)
createFile filedata [x] fileName (DIR dirname ls)
    | x == dirname = DIR dirname (FILE (fileName,filedata) : ls)
    | otherwise    = DIR dirname ls
createFile filedata (x:xs) fileName (DIR dirname ls)
    | x == dirname = DIR dirname [ createFile filedata xs fileName child | child <- ls ]
    | otherwise    = DIR dirname ls 

writeTo :: [String] -> [String] -> String -> FileSystem -> FileSystem
writeTo path dest input fs = createFile input (init dest) (last dest) (removePathFile dest fs)

parseCatArgs :: [String] -> ([String],String)
parseCatArgs args = parse args ([],[])
    where parse [] res = res
          parse (x:xs) (files,dest)
            | x == ">"  = (files,(head xs))
            | otherwise = parse xs ([x] ++ files,dest) 

readChars :: IO String
readChars = do 
    line <- getLine
    if line == "." then return ""
        else do currStr <- readChars
                if currStr == "" then return line
                else do let newStr = line ++ "\n" ++ currStr
                        return newStr

removePrevDir :: [String] -> [String]
removePrevDir [] = []
removePrevDir [el] = ["/"]
removePrevDir ls = init ls

checkPath :: String -> [String] -> [String]
checkPath args1 path 
            | null args1        = path
            | head args1 == '/' = changeDir (separateDirectories args1) []
            | otherwise         = changeDir (path ++ separateDirectories args1) []
            where changeDir [] res = res
                  changeDir (x:xs) res
                    | x == ".." = changeDir xs (removePrevDir res) 
                    | x == "."  = changeDir xs res
                    | otherwise = changeDir xs (res ++ [x])

cd :: String -> [String] -> FileSystem -> (String,[String])
cd arg path fs
    | null arg  = ("",["/"])
    | otherwise = let newPath = checkPath arg path in if getDir newPath fs == Empty then ("No such directory", path) else ("",newPath)

getChildren :: FileSystem -> [FileSystem]
getChildren Empty = []
getChildren (FILE _) = []
getChildren (DIR _ ls) = ls

ls :: String -> [String] -> FileSystem -> String
ls newPath path fs = let level = getDir (checkPath newPath path) fs in if level == Empty then [] else getNames (getChildren level)
    where getNames [] = []
          getNames (Empty:rest) = getNames rest
          getNames (FILE (filename,_):rest) = filename ++ " " ++ getNames rest
          getNames (DIR dirname _ :rest) = dirname ++ " " ++ getNames rest

pwd :: [String] -> String
pwd [a] = a
pwd path = getPWD (tail path)
    where getPWD [] = []
          getPWD (x:xs) = "/" ++ x ++ getPWD xs

rm :: [String] -> [String] -> FileSystem -> FileSystem
rm [] _ fs = fs
rm (x:xs) path fs = rm xs path (removePathFile (checkPath x path) fs)

mkdir :: [String] -> [String] -> FileSystem -> FileSystem
mkdir [] _ fs = fs
mkdir (x:xs) path fs = let dirPath = checkPath x path 
                           in mkdir xs path (createDir (init dirPath) (last dirPath) fs)

rmdir :: [String] -> [String] -> FileSystem -> FileSystem
rmdir [] _ fs = fs
rmdir (x:xs) path fs = rmdir xs path (removePathDir (checkPath x path) fs)

getFirstArg :: [String] -> String
getFirstArg []   = []
getFirstArg [[]] = []
getFirstArg [a]  = a

cdIO :: [String] -> [String] -> FileSystem -> IO ([String],FileSystem)
cdIO args path fs = do
    let (output,newPath) = cd (getFirstArg args) path fs
    --putStrLn output
    return (newPath,fs)

lsIO :: [String] -> [String] -> FileSystem -> IO ([String],FileSystem)
lsIO args path fs = do
    let output = ls (getFirstArg args) path fs
    if null output then return (path,fs)
    else do putStrLn output
            return (path,fs)

pwdIO :: [String] -> [String] -> FileSystem -> IO ([String],FileSystem)
pwdIO args path fs = do
    let output = pwd path
    putStrLn output
    return (path,fs)

rmIO :: [String] -> [String] -> FileSystem -> IO ([String],FileSystem)
rmIO args path fs = do
    let newfs = rm args path fs
    return (path,newfs)

catIO :: [String] -> [String] -> FileSystem -> IO ([String],FileSystem)
catIO args path fs = do
    let(files,dest) = parseCatArgs args 
    if null files && null dest then do input <- readChars
                                       putStrLn input
                                       return (path,fs)
    else if null files then do input <- readChars
                               let newfs = writeTo path (checkPath dest path) input fs
                               return (path,newfs)
    else if null dest then do let newfile = readFromFiles files path fs
                              if null newfile then return (path,fs)
                              else do putStrLn newfile
                                      return (path,fs)
    else do let newfile = readFromFiles files path fs
            let newfs = writeTo path (checkPath dest path) newfile fs
            return (path,newfs)

mkdirIO :: [String] -> [String] -> FileSystem -> IO ([String],FileSystem)
mkdirIO args path fs = do
    let newfs = mkdir args path fs
    return (path,newfs)            

rmdirIO :: [String] -> [String] -> FileSystem -> IO ([String],FileSystem)
rmdirIO args path fs = do
    let newfs = rmdir args path fs
    return (path,newfs)

execute :: [String] -> [String] -> FileSystem -> IO ([String],FileSystem)
execute (x:xs) path fs
    | x == "cd"    = cdIO xs path fs
    | x == "ls"    = lsIO xs path fs
    | x == "pwd"   = pwdIO xs path fs
    | x == "rm"    = rmIO xs path fs
    | x == "cat"   = catIO xs path fs
    | x == "mkdir" = mkdirIO xs path fs
    | x == "rmdir" = rmdirIO xs path fs
    | otherwise    = return (path,fs)

cmdLine :: FileSystem -> [String] -> IO FileSystem
cmdLine fs path = do
    putStr "$ " 
    hFlush stdout
    line <- getLine
    let words = separateWords line ' '
    if null words then cmdLine fs path
    else if "exit" == head words then return fs
    else do (newpath,newfs) <- execute words path fs
            cmdLine newfs newpath

main :: IO ()
main = do
    cmdLine filesystem ["/"]
    return ()