module Main

import Data.List
import Data.List.Lazy
import System.File

import Converter

%hide Data.List.group
%hide Data.List.groupBy
%hide Data.List.sort
%hide Data.List.sortBy
%hide Prelude.on


main : IO ()
main = do
  putStrLn "Bienvenue au TP  Table."

  -- Quand vous aurez complété toutes les définitions dans le module Converter,
  -- vous pourrez décommenter la ligne suivante pour convertir le fichier
  -- `marks.csv` en un fichier `marks.html`. Le fichier `marks.html` pourra
  -- alors être visualisé dans un navigateur Web tel que Firefox.

  -- processFile process "marks.csv" "marks.html"

  -- ----------------------------------------------------------
  -- Affichage des exemples fournis dans le module Converter 
  -- ----------------------------------------------------------
  --
  -- Décommentez-les au fur et à mesure de vos définitions :
  
  
  -- printLn (groupBy (<=) [1,2,2,3,1,2,0,4,5,2])

  -- printLn (groupBy (<=) [1,2,2,3,1,2,0,4,5,2])

  -- printLn (groupBy (==) ['a', 'a', 'a', 'b', 'c', 'c', 'c', 'd', 'd', 'a'])

  -- printLn (sortBy (\(c1,n1) => \(c2,n2) => n1 <= n2) [('a', 3), ('b', 5), ('c', 1)])

  -- printLn (sortBy (\x => \ y =>  x `mod` 10 <= y `mod` 10) [1045, 47, 999671])
  
  -- let ys: List Int = [-1] -- défini séparément pour forcer le type 

  -- printLn (((+) `on` length) [1, 2, 3] ys)

  -- printLn (((,) `on` (*2)) 2 3)

  -- let nss : List (List Int) = [[0, 1, 2], [0, 1], [], [0]] -- défini séparément pour forcer le type 
  -- printLn (sortBy ((<=) `on` length) nss)

  -- printLn (group [1,1,1,0,0,2,1,1])

  printLn (groupOn (`mod` 2) [1,3,2,0,5,4,6,8,7])

  -- printLn (zipWithIndex ['a' .. 'd'])

  -- printLn (zipWithRank ['a' .. 'd'])

  -- let table1 : Table = [["1","c"],["2","a"],["3","b"]]

  -- printLn (sortTable (<=) 1 table1)

  -- printLn (readDouble " 4.5 ")

  -- let table2: Table = [["1","a"],["2","b"],["3","c"]]
  -- printLn (copyDownFirstColumn table2)

  -- let row1 : Row = ["Haskell Curry", "18"] -- DONE
  -- printLn (prepend 5 row1) -- DONE

  -- printLn (prepend' (5, row1))  -- DONE

  -- let table3: Table = [ ["A","2"], ["B","2"], ["C","0"], ["D","0"] ]
  -- printLn (addRowNumbers table3)

  -- let table4: Table = [ ["A","2"], ["B","2"], ["C","0"], ["D","0"], ["E","0"], ["F","2"], ["G","2"], ["H","1"], ["I","1"] ]
  -- printLn (groupOnColumn 1 table4)

  let table5 : Table = [ ["Alan Turing",       "19.5"],
                         ["Alonzo Church",     "20"  ],
                         ["Bertrand Russell",  "18"  ],
                         ["Dana Scott",        "19"  ],
                         ["Haskell Curry",     "20"  ],
                         ["Kurt Godel",        "17.5"],
                         ["Per Martin-Lof",    "18"  ],
                         ["Thierry Coquand",   "18"  ],
                         ["Wilhelm Ackermann", "16"  ] ]

  printLn (ranks 1 table5)

  -- let row2 : Row = ["5", "Charles Darwin", "20"]

  -- putStrLn (row2html row2)
 
  -- let table6 : Table = [ ["5", "Charles Darwin", "20"],
  --                        ["8", "Russel Wallace", "18"] ]

  -- putStrLn (table2html table6)

  -- printLn (wordsBy ';' "5;Charles Darwin;20")

  -- printLn (csv2table ';' """      
  --                        Alan Turing;19.5
  --                        Alonzo Church;20
  --                        Bertrand Russell;18
  --                        Dana Scott;19
  --                        Haskell Curry;20
  --                        Kurt Godel;17.5
  --                        Per Martin-Lof;18
  --                        Thierry Coquand;18
  --                        Wilhelm Ackermann;16
  --                        """)

  
