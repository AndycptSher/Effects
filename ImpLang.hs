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


freeAExp :: (Functor f, ExpSe < f) => AExpS -> Free f Value
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
-- freeBExp :: (Functor f, ExpSe c < f) => BExpS -> Free f Bool
freeBExp :: (Functor f, ExpSe < f) => BExpS -> Free f Bool
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
-- auto generated free structures do not know what the expression/effects will emmit


data ExpSe k where
    Var :: String -> (Value -> k) -> ExpSe k
    Num :: Int -> (Value -> k) -> ExpSe k
    Add :: Value -> Value -> (Value -> k) -> ExpSe k
    Sub :: Value -> Value -> (Value -> k) -> ExpSe k
    Mul :: Value -> Value -> (Value -> k) -> ExpSe k
    TT :: (Bool -> k) -> ExpSe k
    FF :: (Bool -> k) -> ExpSe k
    Equals :: Value -> Value -> (Bool -> k) -> ExpSe k
    LessThanEquals :: Value -> Value -> (Bool -> k) -> ExpSe k
    Not :: Bool -> (Bool -> k) -> ExpSe k
    deriving Functor

instance Show (ExpSe k) where
    show (Var s k) = "Var " ++ s
    show (Num i k) = "Num " ++ show i

-- instance Functor (ExpSe c) where
--     fmap :: (a -> b) -> ExpSe c a -> ExpSe c b
--     fmap f (Var s k) = Var s (f . k)
--     fmap f (Num i k) = Num i (f . k)
--     fmap f (Add a b k) = Add a b (f . k)

data Value
    = Number Int
    | Str String
    | Chr Char
    deriving (Show, Eq)

instance Ord Value where
    (Number a) <= (Number b) = a <= b
    (Str a) <= (Str b) = length a <= length b


var :: (Functor f, ExpSe < f) => String -> Free f Value
var s = Op (inj (Var s pure))

num :: (Functor f, ExpSe < f) => Int -> Free f Value
num i = Op (inj (Num i pure))

add :: (Functor f, ExpSe < f) => Value -> Value -> Free f Value
add a b = Op (inj (Add a b pure))

sub :: (Functor f, ExpSe < f) => Value -> Value -> Free f Value
sub a b = Op (inj (Sub a b pure))

mul :: (Functor f, ExpSe < f) => Value -> Value -> Free f Value
mul a b = Op (inj (Mul a b pure))

true :: (Functor f, ExpSe < f) => Free f Bool
true = Op (inj (TT pure))

false :: (Functor f, ExpSe < f) => Free f Bool
false = Op (inj (FF pure))

equals :: (Functor f, ExpSe < f) => Value -> Value -> Free f Bool
equals a b = Op (inj (Equals a b pure))

lessThanEquals :: (Functor f, ExpSe < f) => Value -> Value -> Free f Bool
lessThanEquals a b = Op (inj (Equals a b pure))

not'' :: (Functor f, ExpSe < f) => Bool -> Free f Bool
not'' b = Op (inj (Not b pure))
---

{-
Three problems

if then else
    continuations

types

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

emulatedPlus :: (Functor f, ExpSe < f) => Free f Value
emulatedPlus = do
    a <- var "v"
    b <- num 3
    add a b

-- expSeHandler :: (Ord a, Num a) => Handler_ (ExpSe a) b (Map (Either String Int) a) f' b
-- expSeHandler = Handler_{
--     ret_ = \a s -> Pure a,
--     hdlr_ = \fs s -> case fs of
--         (Var v k) -> k (s!(Left v)) s
--         (Num i k) -> k (s!(Right i)) s
--         (Add a b k) -> k (a + b) s
--         (Mul a b k) -> k (a * b) s
--         (Sub a b k) -> k (a - b) s
--         (TT k) -> k True s
--         (FF k) -> k False s
--         (Equals a b k) -> k (a == b) s
--         (LessThanEquals a b k) -> k (a <= b) s
--         (Not b k) -> k (not b) s
-- }

-- visualHandler :: Handler (ExpSe String) a f' a
-- visualHandler = Handler{
--     ret = Pure,
--     hdlr = \case
--         (Var v k) -> k v
--         (Num i k) -> k (show i)
--         (Add a b k) -> k (a ++ " + " ++ b)
-- }

-- instance Num String where
--     (+) = (++)
-- "Hello world" == un $ handle_ expSeHandler emulatedPlus 
--     (fromList [(Left "v", "Hello "), (Right 3, "world")])

-- comp :: Value
-- comp = un $ handle_ (expSeHandler :: Handler_ (ExpSe) Value (Map (Either String Int) Value) f' Value) emulatedPlus 
--     (fromList [(Left "v", Number 3), (Right 3, Number 3)])

-- visual :: Value
-- visual = un $ handle visualHandler emulatedPlus

-- emulatedEq :: (Functor f, ExpSe a < f, Ord a) => Free f a
-- emulatedEq :: Free (ExpSe Int + End) Bool
emulatedEq = do
    a <- num 0
    b <- num 0
    equals a b
    -- return a

simpleHdlr = Handler{
    ret = Pure,
    hdlr = \case
        (Num i k) -> k (Number i)
        (Equals a b k) -> k (a == b)
        (Add (Number a) (Number b) k) -> k (Number (a + b))
}

w = un $ handle simpleHdlr emulatedEq

-- boolean :: Bool
-- boolean = un $ handle_ (expSeHandler :: Handler_ (ExpSe Int) Bool (Map (Either String Int) Int) f' Bool) (((freeBExp) $ intoAST bExp TrueS "0=0") :: Free (ExpSe Int + End) Bool) (fromList [(Right 0, 0)])

-- comp' = un $ handle_ (expSeHandler  :: Handler_ (ExpSe) Bool (Map (Either String Int) Int) f' Bool) emulatedEq (fromList [(Right 0, 0)])

confusedOperation :: Free (ExpSe + End) (Value, Value, Value)
confusedOperation = do
    (a) <- var "e"
    (b) <- var "r"
    (c) <- var "c"
    return (a, b, c)

conCharHdlr = Handler{
    ret = Pure,
    hdlr = \case
        (Var v k) -> k (Chr 'a')
}

conStrHdlr = Handler{
    ret = Pure,
    hdlr = \(Var _ k) -> k (Str "Hello")
}

conIntHdlr = Handler{
    ret = Pure,
    hdlr = \(Var _ k) -> k (Number 7)
}

v = un $ handle conIntHdlr confusedOperation

simpleEval inp = un $ handle simpleHdlr (freeAExp $ intoAST aExp (NumS 0) inp)
