-- {-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Beyond_Effects.ImpLang where
import Beyond_Effects.Effects
import Linear_Algebra.MiniParsec
import Control.Applicative
import Data.Either
import Data.Map


-- data AExp where
--     N :: Int -> AExp
--     Var :: String -> AExp
--     Sub :: AExp -> AExp -> AExp
--     Plus :: AExp -> AExp -> AExp
--     Times :: AExp -> AExp -> AExp

data ITok where
    NumT :: Int -> ITok
    VarT :: String -> ITok
    SubT :: ITok
    PlusT :: ITok
    TimesT :: ITok
    TrueT :: ITok
    FalseT :: ITok
    EqT :: ITok
    LET :: ITok
    AndT :: ITok
    OrT :: ITok
    NotT :: ITok
    IfT :: ITok
    ThenT :: ITok
    ElseT :: ITok
    WhileT :: ITok
    DoT :: ITok
    DefT :: ITok
    WalrusT :: ITok
    SemiColT :: ITok
    SkipT :: ITok
    deriving (Show, Eq)


{-
-- ifT = fmap (const IfT) (string "if")
-- thenT = fmap (const ThenT) (string "then")
-- elseT  = fmap (const ElseT) (string "else")
-- whileT = fmap (const WhileT) (string "while")
-- doT = fmap (const DoT) (string "do")
-- defT = fmap (const DefT) (string "def")
-- walT = fmap (const WalrusT) (string ":=")
-- semiT = fmap (const SemiColT) (char ';')
-- skipT = fmap (const SkipT) (string "skip") 
-}


tokenizer :: Parser Char [ITok]
tokenizer = fmap reverse (chain1 (fmap (:[]) token) token (fmap (const (flip (:))) ((many . char) ' '))) <|> pure []
    where
        symbols :: Parser Char ITok
        symbols = (\case
                "+" -> PlusT
                "-" -> SubT
                "*" -> TimesT
                "=" -> EqT
                ";" -> SemiColT
                "<=" -> LET
                ":=" -> WalrusT
                _ -> undefined
            ) <$> (fmap (:[]) (oneOf "+-*=;")
            <|> string "<="
            <|> string ":=")

        keywords :: Parser Char ITok
        keywords = trie [
            ("tt", const TrueT),
            ("ff", const FalseT),
            ("and", const AndT),
            ("or", const OrT),
            ("not", const NotT),
            ("if", const IfT),
            ("then", const ThenT),
            ("else", const ElseT),
            ("while", const WhileT),
            ("do", const DoT),
            ("def", const DefT),
            ("skip", const SkipT)]

        number :: Parser Char ITok
        number = NumT . toInt . fmap toDigit <$> some (oneOf "0123456789")

        toInt :: [Int] -> Int
        toInt x = go x id
            where
                go :: [Int] -> (Int -> Int) -> Int
                go [i] f = f i
                go (i: is) f = go is (10 * f i +)

        toDigit :: Char -> Int
        toDigit x = case x of
            '0' -> 0
            '1' -> 1
            '2' -> 2
            '3' -> 3
            '4' -> 4
            '5' -> 5
            '6' -> 6
            '7' -> 7
            '8' -> 8
            '9' -> 9

        name :: Parser Char ITok
        name = fmap VarT (some (oneOf "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"))


        token :: Parser Char ITok
        token = choice [number, symbols, atomic keywords, name]


runParse :: Parser a b -> [a] -> Either (Err, [a]) (b, [a])
runParse (Parser p) inp = p (0, inp) (\x stateAfter -> Right (x, dump stateAfter)) (Right . (, inp)) (\err state -> Left (err, dump state)) (\err -> Left (err, inp))


data AExpS where
    NumS :: Int -> AExpS
    VarS :: String -> AExpS
    PlusS :: AExpS -> AExpS -> AExpS
    SubS :: AExpS -> AExpS -> AExpS
    TimesS :: AExpS -> AExpS -> AExpS
    deriving Show


data BExpS where
    TrueS :: BExpS
    FalseS :: BExpS
    EqS :: AExpS -> AExpS -> BExpS
    LeS :: AExpS -> AExpS -> BExpS
    AndS :: BExpS -> BExpS -> BExpS
    OrS :: BExpS -> BExpS -> BExpS
    NotS :: BExpS -> BExpS
    deriving Show

data CommS where
    -- if then else
    ITES :: BExpS -> CommS -> CommS -> CommS
    -- while do
    WDS :: BExpS -> CommS -> CommS
    -- def :=
    DefS :: String -> AExpS -> CommS
    -- ;
    ConS :: CommS -> CommS -> CommS
    -- skip
    SkipS :: CommS
    deriving Show

varS :: Parser ITok AExpS
-- varS = fmap (\(VarT x) -> VarS x) (satisfy (\case VarT x -> True;_ -> False))
varS = do
    VarT x <- satisfy (const True)
    return (VarS x)

aExp :: Parser ITok AExpS
aExp = plusTerms
    where
        -- num = fmap (\(NumT x) -> NumS x) (satisfy (\case NumT x -> True;_ -> False))
        num = do
            NumT x <- satisfy (const True)
            return (NumS x)

        -- TODO: look up bridge?
        plusTerms :: Parser ITok AExpS
        plusTerms = chain1 timesTerms timesTerms (fmap (\case PlusT -> PlusS;SubT -> SubS) (oneOf [PlusT, SubT]))

        timesTerms :: Parser ITok AExpS
        timesTerms = chain1 elements elements (fmap (const TimesS) (is TimesT))
        -- plusOrSubOrTimes = do 
        --     a1 <- aExp
        --     op <- oneOf [PlusT, SubT, TimesT]
        --     (case op of 
        --         PlusT -> PlusS
        --         SubT -> SubS
        --         TimesT -> TimesS) a1 <$> aExp

        -- plus = do
        --     a1 <- aExp
        --     is PlusT
        --     PlusS a1 <$> aExp
        -- sub = do
        --     a1 <- aExp
        --     is SubT
        --     SubS a1 <$> aExp
        -- times = do
        --     a1 <- aExp
        --     is TimesT
        --     TimesS a1 <$> aExp

        elements = choice [num, varS]


bExp :: Parser ITok BExpS
bExp =  chain1 elements elements (fmap (\case
                AndT -> AndS
                OrT -> OrS) (oneOf [AndT, OrT]))
    -- andOrOr = do 
    --     b1 <- bExp
    --     op <- oneOf [AndT, OrT]
    --     (case op of
    --         AndT -> AndS
    --         OrT -> OrS) b1 <$> bExp
    where
        not' = is NotT >> fmap (NotS) (bExp)
        eqOrLeq = do
            a1 <- aExp
            op <- oneOf [EqT, LET]
            (case op of
                EqT -> EqS
                LET -> LeS) a1 <$> aExp
        elements = choice [fmap (const TrueS) (is TrueT), fmap (const FalseS) (is FalseT),not', eqOrLeq]


comm :: Parser ITok CommS
comm = chain1 elements elements (fmap (const ConS) (is SemiColT))
    where
        elements = choice [ite, whileDo, def, skip]
        ite = do
            bexp <- is IfT *> bExp <* is ThenT
            c1 <- comm <* is ElseT
            ITES bexp c1 <$> comm

        whileDo = do
            bexp <- is WhileT *> bExp <* is DoT
            WDS bexp <$> comm

        def = do
            VarS s <- is DefT *> varS <* is WalrusT
            DefS s <$> aExp

        skip = is SkipT *> pure SkipS


freeAExp :: (Functor f, ExpSe a < f) => AExpS -> Free f a
freeAExp (VarS s) = var s
freeAExp (NumS i) = num i
freeAExp (PlusS a b) = do
    a <- freeAExp a
    b <- freeAExp b
    add a b
freeAExp (SubS a b) = do
    a <- freeAExp a
    b <- freeAExp b
    sub a b
freeAExp (TimesS a b) = do
    a <- freeAExp a
    b <- freeAExp b
    mul a b

-- freeBExp :: forall c f.(Functor f, ExpSe c < f) => BExpS -> Free f Bool
freeBExp TrueS = true
freeBExp FalseS = false
freeBExp (EqS a b) = do
    a <- freeAExp a
    b <- freeAExp b
    equals a b
freeBExp (LeS a b) = do
    a <- freeAExp a
    b <- freeAExp b
    lessThanEquals a b
freeBExp (NotS b) = do 
    b <- freeBExp b
    not'' b



data ExpSe c k where
    Var :: String -> (c -> k) -> ExpSe c k
    Num :: Int -> (c -> k) -> ExpSe c k
    Add :: c -> c -> (c -> k) -> ExpSe c k
    Sub :: c -> c -> (c -> k) -> ExpSe c k
    Mul :: c -> c -> (c -> k) -> ExpSe c k
    TT :: (Bool -> k) -> ExpSe c k
    FF :: (Bool -> k) -> ExpSe c k
    Equals :: c -> c -> (Bool -> k) -> ExpSe c k
    LessThanEquals :: c -> c -> (Bool -> k) -> ExpSe c k
    Not :: Bool -> (Bool -> k) -> ExpSe c k
    deriving Functor

instance Show (ExpSe c k) where
    show (Var s k) = "Var " ++ s
    show (Num i k) = "Num " ++ show i

-- instance Functor (ExpSe c) where
--     fmap :: (a -> b) -> ExpSe c a -> ExpSe c b
--     fmap f (Var s k) = Var s (f . k)
--     fmap f (Num i k) = Num i (f . k)
--     fmap f (Add a b k) = Add a b (f . k)


var s = Op (inj (Var s pure))

num i = Op (inj (Num i pure))

-- add :: (Functor f, ExpSe a < f) => a -> a -> Free f a
add a b = Op (inj (Add a b pure))

sub a b = Op (inj (Sub a b pure))

mul a b = Op (inj (Mul a b pure))

true :: forall c f. (Functor f, ExpSe c < f) => Free f Bool
true = Op ((inj :: forall {k} . (Functor f, ExpSe c < (ExpSe c + f)) => ExpSe c k -> f k) (TT pure))

false :: forall c f. (Functor f, ExpSe c < f) => Free f Bool
false = Op ((inj:: forall {k} . (Functor f, ExpSe c < (ExpSe c + f)) => ExpSe c k -> f k) (FF pure))

equals :: (Functor f, ExpSe c < f) => c -> c -> Free f Bool
equals a b = Op (inj (Equals a b pure))

lessThanEquals :: (Functor f, ExpSe c < f) => c -> c -> Free f Bool
lessThanEquals a b = Op (inj (Equals a b pure))

not'' :: forall c f. (Functor f, ExpSe c < f) => Bool -> Free f Bool
not'' b = Op ((inj:: forall {k} . (Functor f, ExpSe c < (ExpSe c + f))=> ExpSe c k -> f k) (Not b pure))
---

{-
Three problems

if then else

expressions
    flatten like ssa

    v+3
    |
    v
    Add (VarS "v") (NumS 3)
    |
    v
    a <- var "v"
    b <- num 3
    Add a b
    |
    v
    (add <$> var "v") <$> num 3
    |
    v
    Var "v" (Op (\a -> Num 3 (Op (\b -> Add a b))))




-}

-- Examples

main = do
    print "Hello world"


-- "if tt or ff and not 0 = 0  then skip else def v :=2 + 4 "
everything = runParse comm (fst . fromRight ([], "") $ runParse tokenizer "if tt or ff and not 0 = 0  then skip else def v :=2 + 4 ")

ifs = runParse comm (fst . fromRight ([], "") $ runParse tokenizer "if tt then skip else skip")

plus = fst . fromRight (NumS 0, []) $ runParse aExp (fst . fromRight ([], "") $ runParse tokenizer "v + 3")

intoAST targ def inp = fst . fromRight (def, []) $ runParse targ (fst . fromRight ([], "") $ runParse tokenizer inp)

--- Free Monad

emulatedPlus :: (Functor f, ExpSe a < f) => Free f a
emulatedPlus = do
    a <- var "v"
    b <- num 3
    add a b

expSeHandler :: (Ord a, Num a) => Handler_ (ExpSe a) b (Map (Either String Int) a) f' b
expSeHandler = Handler_{
    ret_ = \a s -> Pure a,
    hdlr_ = \fs s -> case fs of
        (Var v k) -> k (s!(Left v)) s
        (Num i k) -> k (s!(Right i)) s
        (Add a b k) -> k (a + b) s
        (Mul a b k) -> k (a * b) s
        (Sub a b k) -> k (a - b) s
        (TT k) -> k True s
        (FF k) -> k False s
        (Equals a b k) -> k (a == b) s
        (LessThanEquals a b k) -> k (a <= b) s
        (Not b k) -> k (not b) s
}

visualHandler :: Handler (ExpSe String) a f' a
visualHandler = Handler{
    ret = Pure,
    hdlr = \case
        (Var v k) -> k v
        (Num i k) -> k (show i)
        (Add a b k) -> k (a ++ " + " ++ b)
}

-- instance Num String where
--     (+) = (++)
-- "Hello world" == un $ handle_ expSeHandler emulatedPlus 
--     (fromList [(Left "v", "Hello "), (Right 3, "world")])

comp :: Int
comp = un $ handle_ (expSeHandler :: Handler_ (ExpSe Int) Int (Map (Either String Int) Int) f' Int) emulatedPlus 
    (fromList [(Left "v", 3), (Right 3, 3)])

visual :: String
visual = un $ handle visualHandler emulatedPlus

-- emulatedEq :: (Functor f, ExpSe a < f, Ord a) => Free f a
emulatedEq = do
    a <- num 0
    b <- num 0
    equals a b
    -- return a

boolean :: Bool
boolean = un $ handle_ (expSeHandler :: Handler_ (ExpSe Int) Bool (Map (Either String Int) Int) f' Bool) ((freeBExp :: BExpS -> Free (ExpSe Int + End) Bool) $ intoAST bExp TrueS "0=0") (fromList [(Right 0, 0)])

comp' = un $ handle_ (expSeHandler  :: Handler_ (ExpSe Int) Bool (Map (Either String Int) Bool) f' Int) emulatedEq (fromList [(Right 0, 0)])
