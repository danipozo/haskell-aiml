{-# LANGUAGE OverloadedStrings #-}

import GHC.Exts( IsString(..) )

-- Tipos para las etiquetas y la información asociada
data Tag = DTag DoubleTag | STag SimpleTag | Text String
data DoubleTag = DoubleTag { name :: String,
                             children :: [Tag],
                             attributes :: [(String, String)]
                           }
data SimpleTag = SimpleTag { s_name :: String,
                             s_attributes :: [(String, String)]
                           }

-- Junto con OverloadedStrings, hace que en las expresiones
-- en las que se espere Tag, una cadena de caracteres se convierta
-- la variante Text de Tag
instance IsString Tag where
  fromString cs = Text cs

quote :: String -> String
quote = ("\""++).(++"\"")

-- Mostrar un atributo en forma de XML
renderAttribute :: (String,String) -> String
renderAttribute (k,v) = k++"="++(quote v)

renderAttributes :: [(String,String)] -> String
renderAttributes a = foldl (++) " " ((++" ") . renderAttribute <$> a)

unlines' = drop 1 . foldr ((++).("\n"++)) ""

-- Muestra una etiqueta y sus hijos de forma recursiva como AIML,
-- con indentación empezando desde el nivel l
renderTag :: Tag -> Int -> String
renderTag (DTag (DoubleTag n c a)) l = "<" ++ n ++ (renderAttributes a)++">\n" ++ (unlines' $ (concat ["  " | r <- [0..l] ]++) <$> (lines $ renderTags c (l+1))) ++ "\n<"++n++"/>\n"
renderTag (STag (SimpleTag n a)) _ = "<" ++ n ++ (foldl (++) " " ((++" ") . renderAttribute <$> a)) ++ "/>"
renderTag (Text s) _ = s
renderTag _ _ = ""

renderTags :: [Tag] -> Int -> String
renderTags ts l = foldl (++) "" (map ((flip renderTag) (l+1)) ts)

-- Funciones auxiliares que construyen funciones para crear etiquetas
doubleTagBuilder :: String -> [(String,String)] -> [Tag] -> Tag
doubleTagBuilder n a c = DTag DoubleTag { name = n, attributes = a, children = c }

simpleTagBuilder :: String -> [(String,String)] -> Tag
simpleTagBuilder n a = STag SimpleTag { s_name = n, s_attributes = a }

index_ n = ("index", show n)
name_ n = ("name", n)
var_ n = ("var", n)

cat = doubleTagBuilder "category"
pat = doubleTagBuilder "pattern"
temp = doubleTagBuilder "template"
global n v = doubleTagBuilder "set" [name_ n] [v]
local n v = doubleTagBuilder "set" [var_ n] [v]
think = doubleTagBuilder "think" []
srai = doubleTagBuilder "srai" []
mapget m k = doubleTagBuilder "map" [name_ m] [k]

star = simpleTagBuilder "star"
getglobal n = simpleTagBuilder "set" [name_ n]
getlocal n = simpleTagBuilder "set" [var_ n]

myTags = cat [] [
              pat [] ["Hola *"],
              temp [] ["Hola,", star [index_ 1], " que tal"]
                ]
