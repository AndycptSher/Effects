{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.State where
import Data.Maybe (fromJust)
-- import Control.Monad.State
--     ( MonadState(put, get), runState, State)
-- https://casperbp.net/posts/2023-07-algebraic-effects/
data Free f a where
    Pure :: a -> Free f a
    Op :: Functor f => (f (Free f a)) -> Free f a


instance (Show a) => Show (Free [] a) where
    show :: Free [] a -> String
    show (Pure a) = "Pure " ++ show a
    show (Op fa) = "Op " ++ show fa

instance (Show b, Show a) => Show (Free (State a) b) where
    show (Pure x) = "Pure ("++ show x ++")"
    show (Op x) = "Op("++ show x ++")"

instance Show a => Show (Free End a) where
    show (Pure x) = "Pure("++ show x ++")"
    show (Op x) = case x of
        otherwise -> undefined

instance (Functor f, Functor g, Show a, Show (f (Free (f + g) a)), Show (g (Free (f + g) a))) 
    => Show (Free (f + g) a) where
    show (Pure x) = "Pure("++ show x++")"
    show (Op (L x)) = "Op("++ show x ++")"
    show (Op (R x)) = "Op("++ show x ++")"

instance (Functor f, Functor g, Show (f a), Show (g a)) => Show ((f + g) a) where
    show (L x) = show x
    show (R x) = show x 

i = Op [Op [Pure 1, Op [Pure 2, Pure 3]], Pure 4, Pure 5, Op [Pure 6 ]]

foo :: Show a => Free [] a -> String
foo = flatten show concat

instance Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap f (Pure x) = Pure (f x)
    fmap f (Op f') = Op (fmap (fmap f) f')

flatten :: forall f a b. Functor f => (a -> b) -- pure handler
                    -> (f b -> b) -- Op handler
                    -> Free f a -- to be flattened
                    -> b -- result
flatten gen alg (Pure x) = gen x
flatten gen alg (Op f) = alg (fb)
    where
        fb :: f b
        fb = transform f

        transform :: f (Free f a) -> f b
        transform = fmap reduce

        reduce :: Free f a -> b
        reduce = flatten gen alg

instance Functor f => Applicative (Free f) where
    pure :: a -> Free f a
    pure = Pure

    (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    freef <*> free = flatten (`fmap` free) Op freef

    -- liftA2 :: (a -> b -> c) -> Free f a -> Free f b -> Free f c
    -- liftA2 f fa fb = flatten (`fmap` fb) Op (fmap f fa)


instance Functor f => Monad (Free f) where
    -- return :: a -> Free f a
    -- return a = pure a

    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    f >>= f' = flatten f' Op f


----
-- idea of control flow?

data State s k
    = Put s k
    | Get (s -> k)
    deriving Functor

instance (Show s, Show k) => Show (State s k) where
    show (Put s k) = "Put(" ++ show s ++ "), " ++ show k
    show (Get k) = "Get()..."

-- instance Monad (State s) where 
--   (>>=) :: State s a -> (a -> State s b) -> State s b
--   (Put s k) >>= f = f k
--   (Get f) >>= f' = Get (\s -> case (f' . f) s of 
--     Put r k -> k
--     Get sk -> undefined)

class f < g where
    inj :: f k -> g k

instance f < f where inj = id
instance f < (f + g) where inj = L
instance {-# OVERLAPPING #-} f < h => f < (g + h) where inj = R . inj

-- manual adjusting to get desired "functor" out

infix 5 ->:
type (f ->: g) = forall a. f a -> g a

assocSum :: f + (g + h) ->: (f + g) + h
assocSum (L x) = L (L x)
assocSum (R (L x)) = L (R x)
assocSum (R (R x)) = R x

assocSum' :: (f + g) + h ->: f + (g + h)
assocSum' (R x) = R (R x)
assocSum' (L (L x)) = L x
assocSum' (L (R x)) = R (L x)

commSum :: f + g ->: g + f
commSum (L x) = R x
commSum (R x) = L x

permute :: (Functor f , Functor f')
        => (f ->: f') -> Free f a -> Free f' a
permute f = flatten Pure (Op . f)

-- un $ handle hErr (permute commSum (handle_ simpleStateHandler (permute (assocSum' . commSum) four) (Just 9)))

-- get :: (State s < f, Functor f) => Free f s
-- get :: Free (Err + (State s + End)) Int
-- get = Op (inj (Get Pure))
get :: (Functor f, State s < f) => Free f s
get = Op (temp)
    where

        temp = inj (temp')
        temp' = Get Pure
-- put :: s -> Free (Err + (State s + End)) ()
put s = Op (inj (Put s (Pure ())))
err msg = Op (inj (Err msg))


data Err k = Err String deriving (Functor, Show)

data End k deriving (Functor, Show)

infixr 6 +
data (f + g) a
    = L(f a)
    | R(g a)
    deriving Functor

simpleStateHandler :: Functor g => Handler_ (State s) a (Maybe s) g (a, Maybe s)
simpleStateHandler = Handler_ {
    -- ret_ :: a -> Maybe s -> Free g (Maybe s)
    ret_ = curry Pure,
    hdlr_ = \state m -> case state of 
        Put s' k -> k (Just s')
        Get k -> k (fromJust m) m
}

loggingHandler :: Functor g => Handler_ (State s) a [s] g (a, [s])
loggingHandler = Handler_ {
    ret_ = (pure .) . (,) ,
    hdlr_ = \x ss -> case x of
        Put s' k -> k (s':ss)
        Get k -> k (head ss) ss
}


incerr' :: Free (Err + State Int + End) a
incerr' = do
    (s :: Int) <- get
    put (s+1)
    err "ae"

mixedOp :: Free (Err + State Int + End) a
mixedOp = do
    put (3::Int) :: Free (Err + State Int + End) ()
    err "a"



data Handler f a f' b =
    Handler { ret   :: Functor f'
                    => a -> Free f' b -- Pure case
            , hdlr  :: Functor f'
                    => f (Free f' b) -> Free f' b} -- Op case

-- stateful handler
data Handler_ f a state f' b =
    Handler_ { ret_  :: a -> (state -> Free f' b) -- Pure case
            , hdlr_ :: f (state -> Free f' b) -> (state -> Free f' b)} -- Op case

hErr :: Functor f' => Handler Err a f' (Either String a)
hErr = Handler { ret = pure . Right
                , hdlr = \(Err x) -> pure (Left x) }

handle  :: (Functor f, Functor f')
        => Handler f a f' b
        -> Free (f + f') a -> Free f' b
handle h = flatten
    (ret h)
    (\case
        L x -> hdlr h x
        R x -> Op x)

handle_ :: (Functor f, Functor f')
        => Handler_ f a state f' b
        -> Free (f + f') a -> state -> Free f' b
handle_ h = flatten
    (ret_ h)
    (\case
        L x -> hdlr_ h x
        R x -> \p -> Op (fmap (\m -> m p) x))

un :: (Free End a) -> a
un (Pure a) = a
un (Op f) = undefined --  end has no instances

one = un (handle_ loggingHandler (handle hErr incerr') [0])

two = handle_ loggingHandler (handle hErr incerr') [0]

three = handle hErr incerr'

-- four :: State Int ()
-- four :: Free (State Integer) ()
four :: Free (State Int + End) ()
four = do
    put (3:: Int) -- :: Free (State Integer) ()
    (v:: Int) <- get
    put (2:: Int)
    return v
    put v

-- five = runState four

liftF :: Functor f => f a -> Free f a
liftF func = Op (fmap pure func)

-- six :: Free (State Int) ()
-- six = liftF four


-- data Effects obj continuation

-- data State s k = 
--     Put s k
--     | Get (s -> k)


-- instance Functor (State s) where
--     fmap :: (a -> b) -> State s a -> State s b
--     fmap f (Put s k) = Put s (f k)
--     fmap f (Get g) = Get (f . g)

-- type FState = Free State 