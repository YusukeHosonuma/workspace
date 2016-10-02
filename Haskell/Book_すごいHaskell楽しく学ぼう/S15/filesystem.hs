import Data.List (break)

type Name = String
type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

x -: f = f x

fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs):bs) =
    (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewItem :: FSItem -> FSZipper -> FSZipper
fsNewItem item (Folder folderName items, bs) = (Folder folderName (item:items), bs)

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "a.wmv" "baaaa"
        , File "popetime.avi" "god bless"
        , Folder "pics"
            [ File "a.jpg" "aaa"
            , File "b.gif" "bbb"
            , File "c.bmp" "ccc"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "f.exe" "10gotofart"
            , File "o.dmg" "mov eax, h00t"
            , File "n.exe" "really"
            , Folder "source code"
                [ File "best.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]
