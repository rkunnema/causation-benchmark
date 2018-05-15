import Text.Tabular
import qualified Text.Tabular.AsciiArt as A
import qualified Text.Tabular.LatexBooktab as L
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set

-- the parser
import Options.Applicative hiding (empty)
import Data.Semigroup ((<>))

import Causes
import Examples.Name

import qualified Examples.MForestConjunctive as M1
import qualified Examples.MForestDisjunctive as M2
import qualified Examples.MForestDisjunctiveExtended as M3
import qualified Examples.MBogus as M4
import qualified Examples.MBogusHalpern as M5
import qualified Examples.MBogusCtl as M6
import qualified Examples.MVote as M7
import qualified Examples.MLate as M8
import qualified Examples.MHopkinsPearl as M9
import qualified Examples.MSwitch as M10
import qualified Examples.MTrain as M11
import qualified Examples.MTrainHall as M12
import qualified Examples.MBogusHalpernReversed as M13
import qualified Examples.MTrainCtl as M14
import qualified Examples.MAgreement as M15
import qualified Examples.MBackup as M16
import qualified Examples.MBoS as M17
import qualified Examples.MEarlyPreemption as M18
import qualified Examples.MBackupCtl as M19
import qualified Examples.MRanch as M20
import qualified Examples.MHalpern15Vote as M21
import qualified Examples.MPollution80 as M22
import qualified Examples.MPollution50 as M23
import qualified Examples.MPollution120 as M24
import qualified Examples.MCommand as M25
import qualified Examples.MCarefulPoisoning as M26
import qualified Examples.MEarlyPreemptionCtlTree as M27
import qualified Examples.MEarlyPreemptionCtl as M28
import qualified Examples.MCarefulPoisoningCtl as M29
import qualified Examples.MCombinationLamp as M30
import qualified Examples.MShock as M31
import qualified Examples.MPushA as M32
import qualified Examples.MPushB as M33
import qualified Examples.MPushCtl as M34
import qualified Examples.MFancyLamp as M35
import qualified Examples.MFancyLampCtl as M36

data Options = Options
  { tex      :: Bool
  , expensive:: Bool
  , full:: Bool
  , debug    :: Bool } -- not used atm.

options :: Parser Options
options = Options
      <$> switch
          ( long "tex"
         <> short 't'
         <> help "output a TeX-Table" )
      <*> switch
          ( long "expensive"
         <> short 'e'
         <> help "compute expensive benchmarks" )
      <*> switch
          ( long "full"
         <> short 'f'
         <> help "compute full table" )
      <*> switch
          ( long "debug"
         <> short 'd'
         <> help "print debug output (currently not supported)" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "outputs a table right away."
     <> header "cause-benchmark - benchmark suite for causal definitions" )

greet :: Options -> IO ()
greet (Options True e f debug) = do
            putStr $ L.renderUsing (columns f) printCol id printCauseListLatex (benchmark f)
            if e then
                putStr $ L.renderUsing ("p{3cm}":(repeat "p{2.5cm}")) printCol id printCauseListLatex (expensive_benchmark f)
            else return ()
    where printCol Name{name=name, cite=Footnote s} = prefix++ name ++ "\\footnote{"++s++"}"
          printCol Name{name=name, cite=Example s} = prefix++ name ++ ", Ex.~\\ref{"++s++"}"
          printCol Name{name=name, cite=Citation Nothing s} = prefix++ name ++ " \\cite{"++s++"}"
          printCol Name{name=name, cite=Citation (Just p) s} = prefix++ name ++ " \\cite["++p++"]{"++s++"}"
          prefix = "\\raggedright "
          columns True = ["p{4cm}","p{4cm}","p{3.5cm}","p{2.5cm}","p{3.2cm}","p{2.5cm}"]
          columns False = ["p{6.5cm}","p{4cm}","p{4cm}","p{2.5cm}"]

greet (Options False e f debug) = do
            -- putStrLn $ show $ actual_causes_2015 m2 u1 phi
            -- putStrLn $ show $ necessary_causes M1.m M1.u M1.phi
            -- putStrLn $ show $ sufficient_causes_open M1.m M1.u M1.phi
            -- putStrLn $ show $ actual_causes_2015 M1.m M1.u M1.phi
            -- putStrLn $ show $ cfpSufficient M4.r M4.m M4.u M4.phi
            -- putStrLn $ show $ necessary_causes M5.m M5.u M5.phi
            -- putStrLn $ show $ cfpSufficient M15.r M15.m M15.u M15.phi
            putStr $ A.render name id printCauseListAscii (benchmark f)
            if e then do
                putStr $ A.render name id printCauseListAscii (expensive_benchmark f)
                -- putStrLn $ show $ eval_defs f M7.m M7.u M7.phi M7.r
                -- putStrLn $ show $ eval_defs f M20.m M20.u M20.phi M20.r
                -- putStrLn $ show $ eval_defs f M27.m M27.u M27.phi M27.r
            else return ()

printCauseListLatex ll = Data.List.intercalate ", " (fmap showCause ll)
    where showCause l = "$("++ (Data.List.intercalate ", " (fmap g l))++")$"
          g s = "\\mathit{" ++ s ++ "}"

printCauseListAscii ll = Data.List.intercalate "," (fmap showCause ll)
    where showCause l = "("++ (Data.List.intercalate "," l)++")"


table_header True = 
  empty ^..^ colH "necessary causes" 
        ^..^ colH "sufficient causes"
        ^..^ colH "sufficient causes (restr.)" 
        ^..^ colH "actual causes"
        ^..^ colH "CFPSC" 
table_header False = 
  empty ^..^ colH "Definition~\\ref{def:sufficient-cause}"
        ^..^ colH "Definition~\\ref{def:actual-cause}"
        ^..^ colH "Definition~\\ref{def:sufficient-actual-cause}"

eval_defs f m u phi r = map (map ((fmap show) . Set.elems))
  (if f then
  [
      necessary_causes m u phi
    , sufficient_causes_open m u phi
    , sufficient_causes r m u phi
    , actual_causes_2015 m u phi
    , cfpSufficient r m u phi
  ]
  else 
  [
      sufficient_causes_open m u phi
    , actual_causes_2015 m u phi
    , cfpSufficient r m u phi
  ])

m7_precomputed = [[["V1","V2","V3","V4","O"],["V1","V2","V3","V5","O"],["V1","V2","V4","V5","O"],["V1","V3","V4","V5","O"],["V2","V3","V4","V5","O"]],[["V1","V2"],["V1","V3"],["V1","V4"],["V1","V5"],["V2","V3"],["V2","V4"],["V2","V5"],["V3","V4"],["V3","V5"],["V4","V5"],["O"]],[["V1","V2","V3","V4"],["V1","V2","V3","V5"],["V1","V2","V4","V5"],["V1","V3","V4","V5"],["V2","V3","V4","V5"]]]
m20_precomputed = [[["A_1","A_2","M_1","O"]],[["A_1"],["A_2"],["M_1"],["O"]],[["A_1","A_2"]]]

expensive_benchmark f = 
  table_header f
  -- +.+ row M7.n (eval_defs f M7.m M7.u M7.phi M7.r)
  -- +.+ row M20.n (eval_defs f M20.m M20.u M20.phi M20.r) 
  +.+ row M27.n (eval_defs f M27.m M27.u M27.phi M27.r)

benchmark f =
  table_header f
  +.+ row M1.n (eval_defs f M1.m M1.u M1.phi M1.r)
  +.+ row M2.n (eval_defs f M2.m M2.u M2.phi M2.r)
  +.+ row M3.n (eval_defs f M3.m M3.u M3.phi M3.r)
  +----+
      row M8.n (eval_defs f M8.m M8.u M8.phi M8.r)
  +.+ row M18.n (eval_defs f M18.m M18.u M18.phi M18.r)
  +.+ row M28.n (eval_defs f M28.m M28.u M28.phi M28.r)
  +----+
  row M4.n (eval_defs f M4.m M4.u M4.phi M4.r)
  +.+ row M5.n (eval_defs f M5.m M5.u M5.phi M5.r)
  +.+ row M6.n (eval_defs f M6.m M6.u M6.phi M6.r)
  +.+ row M13.n (eval_defs f M13.m M13.u M13.phi M13.r)
  +.+ row M26.n (eval_defs f M26.m M26.u M26.phi M26.r)
  +.+ row M29.n (eval_defs f M29.m M29.u M29.phi M29.r)
  +----+
      row M11.n (eval_defs f M11.m M11.u M11.phi M11.r)
  +.+ row M12.n (eval_defs f M12.m M12.u M12.phi M12.r)
  +.+ row M14.n (eval_defs f M14.m M14.u M14.phi M14.r)
  +----+
      row M9.n (eval_defs f M9.m M9.u M9.phi M9.r)
  +.+ row M16.n (eval_defs f M16.m M16.u M16.phi M16.r)
  +.+ row M19.n (eval_defs f M19.m M19.u M19.phi M19.r)
  +.+ row M25.n (eval_defs f M25.m M25.u M25.phi M25.r)
  +----+
      row M15.n (eval_defs f M15.m M15.u M15.phi M15.r)
  +.+ row M17.n (eval_defs f M17.m M17.u M17.phi M17.r)
  +----+
      row M10.n (eval_defs f M10.m M10.u M10.phi M10.r)
  +.+ row M30.n (eval_defs f M30.m M30.u M30.phi M30.r)
  +.+ row M31.n (eval_defs f M31.m M31.u M31.phi M31.r)
  +.+ row M32.n (eval_defs f M32.m M32.u M32.phi M32.r)
  +.+ row M34.n (eval_defs f M34.m M34.u M34.phi M34.r)
  +.+ row M33.n (eval_defs f M33.m M33.u M33.phi M33.r)
  +.+ row M35.n (eval_defs f M35.m M35.u M35.phi M35.r)
  -- +.+ row M36.n (eval_defs f M36.m M36.u M36.phi M36.r)
  +----+
      row M21.n (eval_defs f M21.m M21.u M21.phi M21.r)
  +.+ row M20.n m20_precomputed
  +.+ row M7.n m7_precomputed
  +----+
      row M22.n (eval_defs f M22.m M22.u M22.phi M22.r)
  +.+ row M23.n (eval_defs f M23.m M23.u M23.phi M23.r)
  +.+ row M24.n (eval_defs f M24.m M24.u M24.phi M24.r)
