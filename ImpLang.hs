-- {-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}

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

-- class Freeable a b where
--     toFree :: (Functor f, ExpSe c < f) => b -> Free f a


data AExpS where
    NumS :: Int -> AExpS
    VarS :: String -> AExpS
    PlusS :: AExpS -> AExpS -> AExpS
    SubS :: AExpS -> AExpS -> AExpS
    TimesS :: AExpS -> AExpS -> AExpS
    deriving Show

-- instance Freeable Int AExpS where
--     toFree (NumS i) = num i


data BExpS where
    TrueS :: BExpS
    FalseS :: BExpS
    EqS :: AExpS -> AExpS -> BExpS
    LeS :: AExpS -> AExpS -> BExpS
    AndS :: BExpS -> BExpS -> BExpS
    OrS :: BExpS -> BExpS -> BExpS
    Not :: BExpS -> BExpS
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

        elements = choice [num, varS]

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

bExp :: Parser ITok BExpS
bExp =  chain1 elements elements (fmap (\case
                AndT -> AndS
                OrT -> OrS) (oneOf [AndT, OrT]))
    where
        not' = is NotT >> bExp
        eqOrLeq = do
            a1 <- aExp
            op <- oneOf [EqT, LET]
            (case op of
                EqT -> EqS
                LET -> LeS) a1 <$> aExp
        elements = choice [fmap (const TrueS) (is TrueT), fmap (const FalseS) (is FalseT),not', eqOrLeq]

        -- andOrOr = do 
        --     b1 <- bExp
        --     op <- oneOf [AndT, OrT]
        --     (case op of
        --         AndT -> AndS
        --         OrT -> OrS) b1 <$> bExp

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

data ExpSe c k where
    Var :: String -> (c -> k) -> ExpSe c k
    Num :: Int -> (c -> k) -> ExpSe c k
    Add :: c -> c -> (c -> k) -> ExpSe c k

instance Show (ExpSe c k) where
    show (Var s k) = "Var " ++ s
    show (Num i k) = "Num " ++ show i

instance Functor (ExpSe c) where
    fmap :: (a -> b) -> ExpSe c a -> ExpSe c b
    fmap f (Var s k) = Var s (f . k)
    fmap f (Num i k) = Num i (f . k)
    fmap f (Add a b k) = Add a b (f . k)


var s = Op (inj (Var s pure))

num i = Op (inj (Num i pure))

add :: (Functor f, ExpSe a < f, Num a) => a -> a -> Free f a
add a b = Op (inj (Add a b pure))
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

-- emulatedPlus :: (Functor f, ExpSe c < f, ExpSe (Free f c) < f) => Free f c
emulatedPlus :: (Functor f, ExpSe a < f, Num a) => Free f a
emulatedPlus = do
    a <- var "v"
    b <- num 3
    add a b

expSeHandler :: Num a => Handler_ (ExpSe a) a (Map (Either String Int) a) f' a
expSeHandler = Handler_{
    ret_ = \a s -> Pure a,
    hdlr_ = \fs s -> case fs of
        (Var v k) -> k (s!(Left v)) s
        (Num i k) -> k (s!(Right i)) s
        (Add a b k) -> k (a + b) s
}

instance Num String where
    (+) = (++)
temp = un $ handle_ expSeHandler emulatedPlus (fromList [(Left "v", "Hello "), (Right 3, "world")])
