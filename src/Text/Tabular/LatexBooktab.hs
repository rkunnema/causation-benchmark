module Text.Tabular.LatexBooktab where

import Data.List (intersperse)
import Text.Tabular

render :: (rh -> String)
       -> (ch -> String)
       -> (a -> String)
       -> Table rh ch a -> String
render = renderUsing (repeat "r")

renderUsing :: [String] -- ^ column header specifications including label (l,h,p{3cm},etc)
            -> (rh -> String)
            -> (ch -> String)
            -> (a -> String) -> Table rh ch a -> String
renderUsing cols fr fc f (Table rh ch cells) =
 unlines $ ( "\\begin{tabular}{" ++ hspec ++ "}")
         : [ addTableNl header
           , toprule
           , (concatMap (either vAttr addTableNl) $
              flattenHeader $ fmap row $ zipHeader [] cells $ fmap fr rh)
           , "\\bottomrule\\end{tabular}" ]
 where
  ch2 = Group NoLine [(Header ""),fmap fc ch]
  hspec  = concatMap (either hAttr fst) $ flattenHeader
         $ zipHeader "" cols ch2
  header = texCols . map label . headerContents $ ch2
  --
  row (cs,h) = texCols $ label h : map f cs
  texCols  = concat . intersperse " & "
  texRows  = map addTableNl
  rhStrings = headerContents rh


hline :: String
hline = "\\hline"

toprule :: String
toprule = "\\toprule"

midrule :: String
midrule = "\\midrule"

heavymidrule :: String
heavymidrule = "\\midrule[\\heavyrulewidth]"

bottomrule :: String
bottomrule = "\\bottomrule"

addTableNl :: String -> String
addTableNl = (++ "\\\\\n")

label :: String -> String
label s = "{"++s++"}" -- "\\textbf{" ++ s ++ "}"

hAttr :: Properties -> String
hAttr NoLine     = ""
hAttr SingleLine = "|"
hAttr DoubleLine = "||"

vAttr :: Properties -> String
vAttr NoLine     = ""
vAttr SingleLine = midrule
vAttr DoubleLine = heavymidrule

