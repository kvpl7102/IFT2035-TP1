-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use concatMap" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use putStr" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use isNothing" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr
---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre language         --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire "Lambda"                                 --
---------------------------------------------------------------------------

type Var = String

-- Type Haskell qui décrit les types Psil.
data Ltype = Lint
           | Larw Ltype Ltype   -- Type "arrow" des fonctions.
           deriving (Eq)

-- Customize displau of Larw arrow functions
instance Show Ltype where
    showsPrec p Lint = showString "Int"
    showsPrec p (Larw t1 t2) = showParen (p > 0) $
        showsPrec 1 t1 . showString " -> " . shows t2

-- Type Haskell qui décrit les expressions Psil.
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lhastype Lexp Ltype -- Annotation de type.
          | Lapp Lexp Lexp      -- Appel de fonction, avec un argument.
          | Llet Var Lexp Lexp  -- Déclaration de variable locale.
          | Lfun Var Lexp       -- Fonction anonyme.
          deriving (Show, Eq)

-- Type Haskell qui décrit les déclarations Psil.   
data Ldec = Ldec Var Ltype      -- Déclaration globale.
          | Ldef Var Lexp       -- Définition globale.3
          deriving (Show, Eq)


-- Conversion de Sexp à Lambda --------------------------------------------

s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint

-- ¡¡COMPLÉTER ICI!!
s2t (Scons Snil t ) = s2t t
s2t (Scons (Scons (Scons Snil t1) (Ssym "->") ) t2 ) =  Larw (s2t t1) (s2t t2)

s2t (Scons (Scons t1s (Ssym "->") ) t2 )  = unfoldLarw [t1s] t2
  where
    unfoldLarw [] _ = error "Invalid type expression: Missing type before arrow"
    unfoldLarw [t1'] t2' = Larw (s2t t1') (s2t t2')
    unfoldLarw (t1':ts) t2' = Larw (s2t t1') (unfoldLarw ts t2')
s2t s = error ("Invalid type " ++ show s)

s2t se = error ("Type Psil inconnu: " ++ (showSexp se))

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s

-- ¡¡COMPLÉTER ICI!!
-- `s2l` for Lhastype:
s2l (Scons ( (Scons (Scons Snil (Ssym ":")) e)  ) t) = Lhastype (s2l e) (s2t t)

-- `s2l` for Llet:
s2l (Scons (Scons (Scons Snil (Ssym "let")) (Scons Snil (Scons (Scons Snil (Ssym var)) binding))) body)
  = Llet var (s2l binding) (s2l body)

-- `s2l` for Lfun:
s2l (Scons (Scons (Scons Snil (Ssym "fun")) (Ssym arg)) body) = Lfun arg (s2l body)

-- `s2l` for Lapp:
s2l (Scons (Scons Snil e1) e2) = Lapp (s2l e1) (s2l e2)         -- Base case w/ 1 argument
s2l (Scons e1 e2) = foldl Lapp (s2l e1) (map s2l (toList e2))   -- Recursive case
  -- The helper function `toList` convert the remaining arguments
  -- of the expression into list then apply `foldl` on Lapp and 
  -- the list of arguments.
  where
    toList :: Sexp -> [Sexp]
    toList (Scons Snil e) = [e]
    toList (Scons e1 e2) = e1 : toList e2
    toList e = [e]

-- Other cases (Error):
s2l se = error ("Expression Psil inconnue: " ++ (showSexp se))

s2d :: Sexp -> Ldec
-- `s2d` for Ldef:
s2d (Scons (Scons (Scons Snil (Ssym "def")) (Ssym v)) e) = Ldef v (s2l e)

-- `s2d` for Ldec:
s2d (Scons (Scons (Scons Snil (Ssym "dec")) (Ssym v)) t) = Ldec v (s2t t)

-- Other cases (Error):
s2d se = error ("Déclaration Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

-- Type des tables indexées par des `α` qui contiennent des `β`.
-- Il y a de bien meilleurs choix qu'une liste de paires, mais
-- ça suffit pour notre prototype.
type Map α β = [(α, β)]

-- Transforme une `Map` en une fonctions (qui est aussi une sorte de "Map").
mlookup :: Map Var β -> (Var -> β)
mlookup ((x,v) : xs) x' = if x == x' then v else mlookup xs x'
mlookup [] x = error ("Unknown variable: " ++ show x)

minsert :: Map Var β -> Var -> β -> Map Var β
minsert m x v = (x,v) : m

type TEnv = Map Var Ltype
type TypeError = String

-- L'environment de typage initial.
tenv0 :: TEnv
tenv0 = [("+", Larw Lint (Larw Lint Lint)),
         ("-", Larw Lint (Larw Lint Lint)),
         ("*", Larw Lint (Larw Lint Lint)),
         ("/", Larw Lint (Larw Lint Lint)),
         ("if0", Larw Lint (Larw Lint (Larw Lint Lint)))]

-- `check Γ e t` vérifie que `e` a type `t` dans le contexte `Γ`.
check :: TEnv -> Lexp -> Ltype -> Maybe TypeError

-- ¡¡COMPLÉTER ICI!!

-- `check` for Lnum:
check tenv (Lnum _) Lint = Nothing

-- `check` for Lvar:
check tenv (Lvar x) t =
  let t' = mlookup tenv x
  in if t == t' then Nothing
  else Just ("Erreur de type: " ++ show t ++ " ≠ " ++ show t')

-- `check` for Lhastype:
check tenv (Lhastype e t) t' =
  if t == t'
    then check tenv e t
    else Just ("Erreur de type: " ++ show t ++ " ≠ " ++ show t')

-- `check` for Llet:    
check tenv (Llet x e1 e2) t =
  case synth tenv e1 of
    t1 -> check (minsert tenv x t1) e2 t

-- `check` for Lapp:
check tenv (Lapp e1 e2) t =
  case synth tenv e1 of
    Larw t1 t2 ->
      if t1 == synth tenv e2 && t2 == t
        then Nothing
        else
          Just
            ( "Erreur de type dans l'application: "
                ++ show t1
                ++ " -> "
                ++ show t2
                ++ " ≠ "
                ++ show t
            )
    _ -> Just ("Erreur de type: Application impossible sur l'expression " ++ show e1)



-- `check` for Lfun:
check tenv (Lfun x e) (Larw t1 t2) = check (minsert tenv x t1) e t2

check tenv (Lapp e1 e2) t = case synth tenv e1 of
    Larw t1 t2 -> case check tenv e2 t1 of
        Nothing -> if t2 == t
            then Nothing
            else Just "Type mismatch in function application"
        Just err -> Just err
    _ -> Just "Invalid function type in application"

check tenv (Lfun _ _) _ = Just "Invalid function type"
-- ¡¡COMPLÉTER ICI!!

-- Default case:
check tenv e t
  -- Essaie d'inférer le type et vérifie alors s'il correspond au
  -- type attendu.
  = let t' = synth tenv e
    in if t == t' then Nothing
       else Just ("Erreur de type: " ++ show t ++ " ≠ " ++ show t')

-- `synth Γ e` vérifie que `e` est typé correctement et ensuite "synthétise"
-- et renvoie son type `t`.
synth :: TEnv -> Lexp -> Ltype
synth _    (Lnum _) = Lint
synth tenv (Lvar v) = mlookup tenv v
synth tenv (Lhastype e t) =
    case check tenv e t of
      Nothing -> t
      Just err -> error err

-- ¡¡COMPLÉTER ICI!!

-- `synth` for Lapp:
synth tenv (Lapp e1 e2) =
  case synth tenv e1 of
    Larw t1 t2 ->
      case check tenv e2 t1 of
        Nothing -> t2
        Just err -> error ("Type initial incorrect...")
    t -> error ("Ltype donné incorrect"++(show t))

-- `synth` for Llet:
synth tenv (Llet x e1 e2) =
  let t1 = synth tenv e1 in
    synth (minsert tenv x t1) e2

-- `synth` for Lfun:
synth tenv (Lfun x e) =
  case mlookup tenv x of
    t1 -> Larw t1 (synth (minsert tenv x t1) e)
    _ ->  error ("Variable non liée: " ++ show x)

synth tenv e = error ("Incapable de trouver le type de: " ++ (show e))

-- ¡¡COMPLÉTER ICI!!

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vfun VEnv Var Lexp
           | Vop (Value -> Value)

type VEnv = Map Var Value

instance Show Value where
    showsPrec p  (Vnum n) = showsPrec p n
    showsPrec _p (Vfun _ _ _) = showString "<fermeture>"
    showsPrec _p (Vop _) = showString "<fonction>"

-- L'environnement initial qui contient les fonctions prédéfinies.
venv0 :: VEnv
venv0 = [("+", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x + y)))),
         ("-", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x - y)))),
         ("*", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x * y)))),
         ("/", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x `div` y)))),
         ("if0", Vop (\ (Vnum x) ->
                       case x of
                         0 -> Vop (\ v1 -> Vop (\ _ -> v1))
                         _ -> Vop (\ _ -> Vop (\ v2 -> v2))))]

-- La fonction d'évaluation principale.
eval :: VEnv -> Lexp -> Value
eval venv (Lnum n) = Vnum n
eval venv (Lvar x) = mlookup venv x

-- ¡¡COMPLÉTER ICI!!

-- `eval` for Lhastype:
eval venv (Lhastype e _) =  eval venv e

-- `eval` for Lapp:
eval venv (Lapp e1 e2 ) =
  case eval venv e1 of

    Vfun venv' x body ->
      let argVal = eval venv e2
          venv'' = minsert venv' x argVal
      in eval venv'' body

    Vop op ->
      let argVal = eval venv e2
      in op argVal
    _ -> error ("Valeur attendue dans l'application: " ++ show e1)

-- `eval` for Llet:
eval venv (Llet x e1 e2) =
  let v1 = eval venv e1
      venv' = minsert venv x v1
   in eval venv' e2

-- `eval` for Lfun:
eval venv (Lfun x body) = Vfun venv x body

-- État de l'évaluateur.
type EState = ((TEnv, VEnv),       -- Contextes de typage et d'évaluation.
               Maybe (Var, Ltype), -- Déclaration en attente d'une définition.
               [(Value, Ltype)])   -- Résultats passés (en ordre inverse).

-- Évalue une déclaration, y compris vérification des types.
process_decl :: EState -> Ldec -> EState
process_decl (env, Nothing, res) (Ldec x t) = (env, Just (x,t), res)
process_decl (env, Just (x', _), res) (decl@(Ldec _ _)) =
    process_decl (env, Nothing,
                  error ("Manque une définition pour: " ++ x') : res)
                 decl

process_decl ((tenv, venv), Nothing, res) (Ldef x e) =
    -- Le programmeur n'a *pas* fourni d'annotation de type pour `x`.
    let ltype = synth tenv e
        tenv' = minsert tenv x ltype
        val = eval venv e
        venv' = minsert venv x val
    in ((tenv', venv'), Nothing, (val, ltype) : res)

-- ¡¡COMPLÉTER ICI!!

process_decl ((tenv, venv), Just (x', t'), res) (Ldef x e) =
    case check tenv e t' of
        Nothing ->
            let val = eval venv e
                venv' = minsert venv x val
            in ((tenv, venv'), Just (x', t'), (val, t') : res)
        Just err -> error ("Type incorrect dans la définition de " ++ x ++ ": " ++ err)




---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

process_sexps :: EState -> [Sexp] -> IO ()
process_sexps _ [] = return ()
process_sexps es (sexp : sexps) =
    let decl = s2d sexp
        (env', pending, res) = process_decl es decl
    in do (hPutStr stdout)
            (concat
             (map (\ (val, ltyp) ->
                   "  " ++ show val ++ " : " ++ show ltyp ++ "\n")
              (reverse res)))
          process_sexps (env', pending, []) sexps

-- Lit un fichier contenant plusieurs Sexps, les évalue l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename
  = do filestring <- readFile filename
       let sexps = case parse pSexps filename filestring of
                     Left err -> error ("Parse error: " ++ show err)
                     Right es -> es
       process_sexps ((tenv0, venv0), Nothing, []) sexps

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = synth tenv0 . lexpOf

valOf :: String -> Value
valOf = eval venv0 . lexpOf
