module Converter

import Data.List
import Data.String
import Data.List.Lazy
import System.File

%hide Data.List.groupBy
%hide Data.List.sort
%hide Data.List.sortBy
%hide Prelude.on



-- Le but pédagogique de ce projet est, entre autre, de vous entraîner à définir
-- des fonctions en les composant avec (.) et de démontrer la puissance de cette
-- technique. D'autres méthodes sont d'abord révisées, en particulier, bien sûr,
-- les définitions par récursion et pattern-matching. 

-- Le but matériel de ce projet est de convertir un fichier CSV de notes
-- d'étudiants en une page HTML présentant les notes et le rang de chaque
-- note obtenue dans un tableau.

-- -----------------------------------------------------------------------------
-- Présentation du projet
-- -----------------------------------------------------------------------------

||| Type des chaines de caractères au format 
||| Comma Separated Values 
||| https://fr.wikipedia.org/wiki/Comma-separated_values
CSV : Type
CSV = String


-- Voici un exemple de valeur de type CSV.
-- C'est un extrait du fichier `marks.csv` dans le dossier du présent projet.
export
csv_cc1_marks : CSV
csv_cc1_marks = """      
                Alan Turing;19.5
                Alonzo Church;20
                Bertrand Russell;18
                Dana Scott;19
                Haskell Curry;20
                Kurt Gödel;17.5
                Per Martin-Löf;18
                Thierry Coquand;18
                Wilhelm Ackermann;16
                """

-- Si vous êtres étonnés par les trois guillemets, 
-- cf. https://ipf.istic.univ-rennes1.fr/idris/prelude.html#litt%C3%A9raux


-- ATTENTION : le séparateur utilisé dans le fichier marks.csv est un
-- point-virgule (le caractère ';').

-- Pour les chaînes de caractères au format HTML.
HTML = String

-- Voici par exemple une fonction dont vous vous servirez dans ce projet :

export
-- Renvoie une page complète HTML avec le titre `title` et le corps `body`
-- donnés.
--
-- @ title le titre de la page HTML (entre autre utilisé pour nommer l'onglet
--                                   affichant la page dans un navigateur web)
--
-- @ body le code HTML à insérer dans la balise `body` de la page HTML
--
-- Remarque : 
--
-- cette définition utilise l'interpolation de chaînes qui permet d'insérer la
-- valeur d'une expression de type String dans une chaîne avec la syntaxe
-- '\{<expression>}'.
--
-- Cf.
-- https://idris2.readthedocs.io/en/latest/reference/strings.html#interpolated-strings
html : String -> HTML -> HTML
html title body = """
  <?xml version="1.0" encoding="UTF-8" ?>
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="fr" lang="fr" dir="ltr">
    <head>
      <title>\{title}</title>
    </head>
    <style>
    table, th, td {
      border: 1px solid black;
      border-collapse: collapse;
    }
    </style>
    <body>
      \{body}
    </body>
  </html>
  """

-- Type des chemin d'accès à un fichier, absolue ou bien relatif au présent projet.
FilePath = String

||| Fichier CSV des notes obtenues au CC de FUN au Collège du Monde.
csvSource : FilePath
csvSource = "marks.csv"

||| Fichier cible où enregistrer les page HTML produite dans ce projet
htmlTarget : FilePath
htmlTarget = "marks.html"

export
||| Lit un fichier CSV et crée un fichier HTML en utilisant le convertisseur f
||| pour transformer le contenu CSV en HTML.
|||
||| @ f une fonction transformant du code CSV en code HTML
|||
||| @ source chemin d'accès au fichier CSV à lire
|||
||| @ target chemin d'accès au fichier HTML à écrire
processFile : (CSV -> HTML) -> FilePath -> FilePath -> IO ()
processFile f source target = 
  do (Right content) <- readFile source  
                      | Left err => printLn err
     (Right out) <- writeFile target (f content) 
                      | Left err => printLn err
     printLn "OK"



-- ==============================================================================
-- Partie I - Révisions de fonctions classiques définies par récursion et
--            pattern-matching
-- ==============================================================================
--
-- Dans cette partie, vous devez définir les fonctions spécifiées en utilisant
-- le pattern-matching et la récursion.
--
-- Vous allez (re-)définir quelques fonctions des bibliothèques Idris 2 ou
-- Haskell qui, sans être tout à fait basiques, reflètent des schémas récurrents
-- en programmation.
--
-- Ces fonctions ont leurs équivalents dans d'autres langages de
-- programmation de haut niveau et s'avèrent souvent utiles pour écrire des
-- définitions concises et compréhensibles (à conditions de les connaître !). 

export
||| Regroupe des éléments adjacents selon une relation. 
|||
||| groupBy op xs = la liste de listes ys telles que
|||
|||     - ys est n'est pas vide
|||     - concat (group op xs) = xs
|||     - si y1 et y2 sont adjacents dans ys, alors  op y1 y2  est vrai
|||     - group op xs est de longueur minimale   
|||
||| groupBy (==) ['a','a','a',   'b',  'c','c','c',  'd','d',  'a']
|||            = [['a','a','a'],['b'],['c','c','c'],['d','d'],['a']]
|||
||| groupBy (<=) [1,2,2,3,1,2,0,4,5,2] = [[1,2,2,3],[1,2],[0,4,5],[2]]
|||                                      
||| Conseil : si vous trouvez cela difficile, faites d'abord l'exercice
|||           suivant avec solution guidée, fait en L1 :
|||
|||           https://ipf.istic.univ-rennes1.fr/idris/tp/group.html
|||
-- groupBy : (a -> a -> Bool) -> List a -> List (List a) -- TODO
-- groupBy _ [] = []  -- Base case: if the list is empty, return an empty list of groups
-- groupBy op (x:xs) = go op [x] xs  -- Start the grouping process with the first element
--   where
--     go :: (a -> a -> Bool) -> [a] -> [a] -> [[a]]
--     go _ current [] = [current]  -- If no more elements, return the current group
--     go op current (y:ys)
--       | op (last current) y = go op (current ++ [y]) ys  -- If y satisfies the predicate, add to the group
--       | otherwise = current : go op [y] ys  -- Otherwise, start a new group with y

groupBy : (a -> a -> Bool) -> List a -> List (List a) -- TODO
groupBy _ [] = []
groupBy op (x :: xs) = buildGroup x xs []
  where
    buildGroup : a -> List a -> List a -> List (List a)
    buildGroup last [] acc = [acc ++ [last]]
    buildGroup last (x :: xs) acc = 
      if op last x then buildGroup x xs (acc ++ [last])
      else (acc ++ [last]) :: groupBy op (x :: xs)


export
||| zippe une liste paresseuse avec une liste stricte.
|||
||| Vous connaissez déjà
|||
|||    zip : List List a -> List b -> List (a,b)
||| et zip : LazyList a -> LazyList b -> LazyList (a,b)
|||
||| La présente version de zip permet de zipper une liste infinie avec une liste finie
||| stricte pour obtenir une liste finie stricte. Cette fonction est utile pour
||| être certain d'avoir suffisamment d'éléments dans la première liste pour
||| obtenir une liste de même longueur que la seconde. Elle permet par exemple de
||| définir zipWithIndex de façon astucieuse (cf. ci-après).
zip' : LazyList a -> List b -> List (a,b) -- TODO
zip' _ [] = []
zip' [] _ = []
zip' (x :: xs) (y :: ys) = (x, y) :: zip' xs ys

export
||| Tri une liste selon la relation d'ordre donnée.
|||
||| Exemples :
|||
|||   - Tri selon la deuxième composante
||| 
|||     sortBy (\(c1,n1) => \(c2,n2) => n1 <= n2) [('a', 3), ('b', 5), ('c', 1)]
|||       = [('c', 1), ('a', 3), ('b', 5)]
|||
|||   - Tri selon le chiffre des unités 
|||
|||     sortBy (\x = \ y =>  x `mod` 10 <= y `mod` 10) [1045, 47, 999671] = [999671, 1045, 47]
|||
||| Inspirez vous de l'algorithme de tri rapide simpifié suivant :
|||
|||    sort : Ord a => List a -> List a
|||    sort Nil     = Nil
|||    sort (p::xs) = sort (filter (<=p) xs) ++ p :: sort (filter (>p) xs)
|||
sortBy : (a -> a -> Bool)  -> List a -> List a -- TODO
sortBy _ [] = []
sortBy op (p :: xs) = 
  let lesser = filter (\x => op x p) xs
      greater = filter (\x => not (op x p)) xs
  in sortBy op lesser ++ [p] ++ sortBy op greater

-- Déclaration de l'opérateur d'indexation définie ci-dessous
-- Pas d'associativité, niveau de priorité 8 
export infix 8 !!

export
||| Renvoie l'élément d'indice donné de la liste donnée, ou bien la chaîne vide
||| si l'indice n'est pas valide. Le premier élément est celui d'incide 0.
(!!) : List String -> Nat -> String -- TODO
[] !! _ = ""  -- Return an empty string for invalid indices
(x :: xs) !! Z = x
(x :: xs) !! (S n) = xs !! n


-- ============================================================================
-- Partie III : composition 
-- ============================================================================

-- ATTENTION aux consignes suivantes :
--
--    - PAS DE RÉCURSION EXPLICITE (utilisez des fonctions d'ordre supérieur,
--      dont (.), `on`, map, uncurry, ...), (sauf exception signalée).
--
--    - PAS DE LAMBDA ABSTRACTION
--
--    - NE DÉFINISSEZ PAS DE NOUVELLES FONCTIONS
--
--    - PAS DE pliages (foldr, foldl, ... INTERDITS) 
--
--    - MINIMISEZ LE NOMBRE D'ARGUMENTS EXPLICITES DANS LA PARTIE GAUCHE DES
--      CLAUSES. C'est-à-dire, efforcez-vous de donner des définitions
--      pointless, tant que cela reste raisonnable. 
--
--    - UNE SEULE CLAUSE par définition (sauf exception signalée). Nos solutions
--      tiennent toutes sur une ligne de moins de 80 caractères, exceptée celle
--      de ranks qui fait plus de 100 caractères.
--
--    - AUCUNES PARENTHÈSES INUTILES. Cette consigne est pour vous forcer à
--      réfléchir à la signification des priorités des opérateurs. Ne vous
--      contentez pas d'essayer différents parenthésages pour voir ce que
--      le compilateur accepte. Vous devez COMPRENDRE les règles de parenthésage.

-- ----------------------------------------------------------------------------
-- Fonction `on` 
-- ----------------------------------------------------------------------------
--
-- C'est un combinateur classique de fonctions à ranger avec (.), id, curry,
-- uncurry, flip, ...
--
-- Exemples (repris de la bibliothèque Haskell
-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Function.html#v:on)
--
--    ((+) `on` length) [1, 2, 3] [-1] = 4
--
--    ((,) `on` (*2)) 2 3 = (4,6)    où (,) est l'opérateur binaire qui
--                                          construit une paire
--
--    sortBy ((<=) `on` length) [[0, 1, 2], [0, 1], [], [0]] =
--         [[],[0],[0,1],[0,1,2]] 
--
--    où sortBy est la fonction définie précédemment dans ce projet.

export 
||| op `on` f  est l'opérateur binaire op' qui applique op à des arguments en
||| transformant d'abord ceux-ci avec f.
|||
||| @ op une fonction binaire (souvent un opérateur infixe)
|||
||| @ f une fonction unaire
|||
||| Donnez une définition de la forme  (op `on` f) x y = ...
|||
||| La solution est donnée dans l'extrait du Prélude de la page du cours (à la fin).
on : (b -> b -> c) -> (a -> b) -> (a -> a -> c) -- TODO
on op f x y = op (f x) (f y)

-- ----------------------------------------------------------------------------
-- Fonctions group et zip'
-- ----------------------------------------------------------------------------
export
||| group xs est une liste de listes non-vides d'entiers telle que :
||| 
|||   - les éléments d'une liste intérieure sont tous égaux
|||   - deux listes intérieures successives ont des éléments distincts
|||   - xs est la concaténation de toutes les listes intérieures
|||
||| @ xs une liste d'entiers
||| 
||| Exemple :
|||
|||   group [1,1,1,0,0,2,1,1] = [ [1,1,1], [0,0], [2], [1,1] ]
|||
||| Consigne : utilisez groupBy
group : Eq a => List a -> List (List a) -- TODO
group = groupBy (==)

export
||| Groupe les éléments d'une liste en les comparant via une fonction donnée.
|||
||| groupOn (`mod` 2) [1,3,2,0,5,4,6,8,7] = [[1,3],[2,0],[5],[4,6,8],7]
groupOn : Eq k => (a ->  k) -> List a -> List (List a) -- TODO


export
||| Liste infinie des entiers naturels à partir du nombre donné
|||
||| natsFrom 0 est la liste de tous les entiers naturels
|||
||| Indication : vous devez ici utiliser un appel récursif explicite, comme dans le cours 
|||              sur les listes paresseuses (dans lequel se trouve la solution !).
natsFrom : Nat -> LazyList Nat -- TODO
natsFrom n = n :: natsFrom (n + 1)

export
||| zippe une liste avec ses indices-- ----------------------------------------------------------
|||
||| zipWithIndex ['a' .. 'd'] = [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd')]
zipWithIndex : List a -> List (Nat, a) -- TODO
zipWithIndex [] = []
zipWithIndex xs = zip' (natsFrom 0) xs


export
||| zippe une liste avec ses rangs, où le rang d'un élément est le successeur
||| de son indice.
|||
||| zipWithRank ['a' .. 'd'] = [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]
|||
||| Copiez-Coller la définition de zipIndex et modifier ce qui doit l'être. C'est
||| de cette fonction dont vous aurez besoin par la suite. Nous avons présenté
||| zipWithIndex parce que c'est celle qui est souvent présente dans les
||| bibliothèques des langages de programmation.
zipWithRank : List a -> List (Nat, a) -- TODO
zipWithRank [] = []
zipWithRank xs = zip' (natsFrom 1) xs


-- ------------------------------------------------------------------------------
-- Tableaux
-- ------------------------------------------------------------------------------
-- Nous utiliserons les types suivants pour représenter les tableaux
-- de données. Pour simplifier, les cellules d'un tableau sont toutes
-- de type String.


public export
||| Type des lignes d'un tableau
Row : Type
Row = List String

public export
||| Un tableau est une liste de lignes
Table : Type
Table = List Row

-- ------------------------------------------------------------------------------
-- Tri d'un tableau selon une colonne
-- ------------------------------------------------------------------------------
export
||| sortTable lte j table  = le tableau obtenu à partir du tableau table en
||| réordonnant ses lignes selon la relation d'ordre lte, sur la colonne j.
|||
||| @ lte une relation d'ordre (Less Than or Equal) sur les valeurs 
|||       de la colonne j
|||
||| @ j l'indice de la colonne selon laquelle les lignes du tableau 
|||     doivent être réordonnées
|||
||| Exemple :
|||
|||   sortTable (<=) 1 [["1","c"],["2","a"],["3","b"]] 
|||     = [["2", "a"], ["3", "b"], ["1", "c"]]
|||
||| Indication : écrire une définition de la forme 
|||
|||   sortTable lte j = ...

sortTable : (String -> String -> Bool) -> Nat -> Table -> Table -- TODO



export
||| Lit une valeur Double depuis une chaîne de caractères comportant
||| possiblement des espaces superflus au début et à la fin.
||| 
||| Exemple :   readDouble " 4.5 " = 4.5
||| 
||| Vous pouvez utiliser :
||| 
||| cast : String -> Double  qui convertit une chaîne de caractères représentant
|||                          un nombre en un Double.
||| trim : String -> String, défini dans Data.List, qui supprime les espaces au 
|||                          début et à la fin d'une chaîne de caractères. 
readDouble : String -> Double -- TODO
readDouble str = cast (trim str)

export
||| Relation d'ordre décroissant sur des cellules d'un tableau 
||| représentant des doubles.
|||
||| Exemples :
|||
|||  descDoubleOrd " 4.5 " "6"  = True
|||  descDoubleOrd "9 "  "6"    = False
|||  descDoubleOrd "-4"  "-4"   = True
|||
||| Consigne : utilisez `on`
|||
descDoubleOrd : String -> String -> Bool -- TODO
descDoubleOrd = (>=) `on` readDouble


-- ------------------------------------------------------------------------------
-- Calcul des rangs des lignes d'un tableau selon une colonne
-- ------------------------------------------------------------------------------
export
||| Rend toutes les cellules de la première column égales à sa première cellule.
|||
|||    copyDownFirstColumn [["1","a"],["2","b"],["3","c"]] 
|||      = [["1", "a"], ["1", "b"], ["1", "c"]]
|||
||| Cette définition peut être définie en deux clauses, par pattern-matching sur
||| la liste reçue en argument, mais ne doit pas être explicitement récursive.
|||
||| Commencez par une définition avec  map  et une lambda-abstraction, puis 
||| transformer la lambda-abstraction en une composition de fonctions. 
|||
||| Rappel : drop 1 renvoie le reste d'une liste non-vide ou la liste vide.
|||
copyDownFirstColumn : Table -> Table -- TODO
copyDownFirstColumn [] = []
copyDownFirstColumn (row :: rows) = 
  let firstCell = head row
  in map (\r => firstCell :: tail r) (row :: rows)



export
||| Ajoute une cellule de contenu donné au début d'une ligne de tableau.
|||
||| Exemple :
|||
|||   prepend 5 ["Haskell Curry", "18"] = ["5", "Haskell Curry", "18"]
|||
prepend : Show a =>  a ->  Row -> Row -- TODO
prepend x row = show x :: row

export
||| Ajoute une cellule de contenu donné au début d'une ligne de talbeau.
|||
||| Exemple :
|||
|||   prepend' (5, ["Haskell Curry", "18"]) = ["5", "Haskell Curry", "18"]
|||
||| Aide : utilisez la fonction précédente et un combinateur de fonctions
|||        (cf. https://ipf.istic.univ-rennes1.fr/idris/prelude.html#combinateurs-de-fonctions-%C3%A0-conna%C3%AEtre-en-l2-et-l3)
prepend' : Show a => (a,  Row) -> Row -- TODO
prepend' = uncurry prepend

export
||| Ajoute une colonne à gauche d'un tableau, contenant le numéro de chaque ligne
||| (la première ligne étant numérotée 1).
|||
||| addRowNumbers [ ["A","2"], 
|||                 ["B","2"], 
|||                 ["C","0"], 
|||                 ["D","0"] ]
||| 
|||                =[["1", "A", "2"], 
|||                  ["2", "B", "2"], 
|||                  ["3", "C", "0"], 
|||                  ["4", "D", "0"]]
|||
||| Indice : utilisez prepend' et zipWithRank
addRowNumbers : Table -> Table -- TODO

export
||| Scinde un tableau en plusieurs tableaux en regroupant les lignes adjacentes
||| ayant la même valeur dans la colonne donnée.
||| 
||| groupOnColumn 1 [ ["A","2"], ["B","2"], ["C","0"], ["D","0"], ["E","0"], ["F","2"], ["G","2"], ["H","1"], ["I","1"] ]
|||
|||               = [ [["A", "2"], ["B", "2"]], 
|||                   [["C", "0"], ["D", "0"], ["E", "0"]], 
|||                   [["F", "2"], ["G", "2"]], 
|||                   [["H", "1"], ["I", "1"]] ]           
|||
||| Indice 1 : utilisez groupOn et (!!)
||| 
||| Indice 2 :
|||       Donnez d'abord une définition de la forme groupOnColumn n tab = ... 
|||       puis essayez ensuite de la rendre pointless en utilisant (.) et flip.
|||
groupOnColumn : Nat -> Table -> List Table -- TODO
groupOnColumn n table = groupOn (\row => row !! n) table

export
||| En supposant que la colonne n°j du tableau ait des cellules contenant des
||| nombres double précision, renvoie le tableau trié dans l'odre décroissant de
||| ces valeurs, avec une colonne des rangs de ses valeurs ajoutée à gauche du
||| tableau.
|||
||| @ j un numéro de colonne du tableau (0 pour la première) contenant des Double
|||
||| Exemple ranks 1 [ ["Alan Turing",       "19.5"],
|||                   ["Alonzo Church",     "20"  ],
|||                   ["Bertrand Russell",  "18"  ],
|||                   ["Dana Scott",        "19"  ],
|||                   ["Haskell Curry",     "20"  ],
|||                   ["Kurt Gödel",        "17.5"],
|||                   ["Per Martin-Löf",    "18"  ],
|||                   ["Thierry Coquand",   "18"  ],
|||                   ["Wilhelm Ackermann", "16"  ] ]
|||         =                        
|||             [["1", "Haskell Curry",     "20"  ], 
|||              ["1", "Alonzo Church",     "20"  ], 
|||              ["3", "Alan Turing",       "19.5"], 
|||              ["4", "Dana Scott",        "19"  ], 
|||              ["5", "Thierry Coquand",   "18"  ], 
|||              ["5", "Per Martin-Lof",    "18"  ], 
|||              ["5", "Bertrand Russell",  "18"  ], 
|||              ["8", "Kurt G\246del",     "17.5"], 
|||              ["9", "Wilhelm Ackermann", "16"  ]]
|||
||| Votre définition doit être de la forme 
|||
|||   ranks j = ...
|||
||| et utiliser la composition de fonctions.
|||
||| Indice 1 : que feriez-vous « manuellement » dans un tableur qui n'a pas la
||| fonction rank ?
|||
|||          1.       Trier la colonne j dans l'ordre décroissant
|||
|||          2. PUIS, numéroter les lignes du haut vers le bas dans une nouvelle
|||                   colonne à gauche
|||
|||          3. PUIS, scinder le tableau en tableaux de valeur constante dans la
|||                   colonne (anciennement) numérotée j
|||
|||          4. PUIS, pour chacun de ces sous-tableaux, copier le numéro de ligne
|||                   de la première ligne vers le bas (c'est le rang de la valeur 
|||                   commune à la colonne j !).
|||
|||          5. PUIS, recoller les sous-tableaux en un seul
|||
||| Indice 2 : entrée du dictionnaire :
|||              « composer », verbe transitif direct : 
|||              Former un tout harmonieux, en combinant différents éléments.
|||
||| Indice 3 : sortTable, puis addRowNumbers, puis groupOnColumn, puis
|||            copyDownFirstColumn, puis concat. Le mot `puis` est la composition
|||            la composition de fonctions lue de droite à gauche !
||| 
ranks : Nat -> Table -> Table -- TODO
ranks j = concat . map copyDownFirstColumn . groupOnColumn j . addRowNumbers . sortTable descDoubleOrd j

-- ------------------------------------------------------------------------------
-- Export d'un tableau au format HTML
-- ------------------------------------------------------------------------------

export
||| Insert du code HTML entre les balises de nom donné
||| 
||| tag "p" "Hello World !" = "<p>Hello World !</p>"
|||
tag : String -> String -> String
tag tagName content = "<\{tagName}>\{content}</\{tagName}>"

export
||| Convertit une ligne de tableau au format HTML
|||
||| Exemple : row2html ["5", "Charles Darwin", "20"]
|||           = "<tr><td>5</td>Charles Darwin<td></td><td>20</td></tr>"
|||              
||| Pour plus d'info : https://www.w3schools.com/html/html_tables.asp
row2html : Row -> String -- TODO
row2html row = "<tr>" ++ concatMap (\cell => "<td>" ++ cell ++ "</td>") row ++ "</tr>"

export
||| Convertit un tableau au format HTML (sans en-têtes)
|||
||| Exemple : table2html [ ["5", "Charles Darwin", "20"] ,
|||                        ["8", "Russel Wallace", "18"] ]
|||           = "<table><tr><td>5</td>Charles Darwin<td></td><td>20</td></tr><tr><td>8</td>Russel Wallace<td></td><td>18</td></tr></table>"
|||
||| Pour plus d'info : https://www.w3schools.com/html/html_tables.asp
table2html : Table -> String -- TODO
table2html table = "<table>" ++ concatMap row2html table ++ "</table>"

-- ------------------------------------------------------------------------------
-- Import d'un tableau depuis le format CSV
-- ------------------------------------------------------------------------------
export
||| Liste des « mots » d'une chaîne de caractères, ces mots étant séparés
||| par le séparateur donné (pas forcément des espaces, donc).
|||
||| Exemple :  wordsBy ';' "5;Charles Darwin;20"
|||                = ["5", "Charles Darwin", "20"]
|||
||| Indication 1 : utiliser les fonctions de conversion
|||
|||                unpack : String -> List Char
|||                pack : List Char -> String
|||        
|||            ainsi que les fonctions groupOn et filter
|||
||| Indication 2 : dessayez de donner une définition de la forme
|||
|||                wordsBy sep = ...
|||
wordsBy: Char -> String -> List String -- TODO

export
||| Convertit une chaîne au format CSV en un tableau, en se servant du séparateur
||| donné pour séparer les champs.
|||
||| Exemple :
||| 
|||      csv2table ';' """      
|||                    Alan Turing;19.5
|||                    Alonzo Church;20
|||                    Bertrand Russell;18
|||                    Dana Scott;19
|||                    Haskell Curry;20
|||                    Kurt Gödel;17.5
|||                    Per Martin-Löf;18
|||                    Thierry Coquand;18
|||                    Wilhelm Ackermann;16
|||                    """
|||             =   [ ["Alan Turing",       "19.5"],
|||                   ["Alonzo Church",     "20"  ],
|||                   ["Bertrand Russell",  "18"  ],
|||                   ["Dana Scott",        "19"  ],
|||                   ["Haskell Curry",     "20"  ],
|||                   ["Kurt Gödel",        "17.5"],
|||                   ["Per Martin-Löf",    "18"  ],
|||                   ["Thierry Coquand",   "18"  ],
|||                   ["Wilhelm Ackermann", "16"  ] ]
|||
||| Votre définition doit être de la forme 
|||
|||   csv2table sep = ...
|||
csv2table : Char -> String -> Table -- TODO
csv2table sep csv = map (wordsBy sep) (lines csv)

-- ------------------------------------------------------------------------------
-- Conversion d'un tableau de notes au format CSV en un tableau HTML, 
-- ordonné par notes décroissantes avec une colonne indiquant les rangs.
-- ------------------------------------------------------------------------------

export
||| Transforme un tableau CSV à deux colonnes (nom du candidat et note obtenue)
||| en un tableau HTMl trié par notes décroissante avec une colonne insérée à
||| gauche, indiquant le rang du candidat selon sa note.
|||
||| Utilisez la fonction html définie au début de ce document, ainsi que les
||| fonctions ranks, csv2table et table2html.
process : CSV -> HTML -- TODO
process csv = table2html (ranks 1 (csv2table ';' csv))