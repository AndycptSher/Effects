{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Beyond_Effects.Effects(
    Free(Pure, Op),
    flatten,
    type (+),
    type (<)(inj),
    -- type (->:),
    permute,
    assocSum,
    assocSum',
    commSum,
    Handler(Handler, ret, hdlr),
    Handler_(Handler_, ret_, hdlr_),
    handle,
    handle_,
    un,
    End
) where
-- import Language.Haskell.TH (Overlap(Overlapping))


data Free f a where
    Pure :: a -> Free f a
    Op :: Functor f => (f (Free f a)) -> Free f a

-- continuation style
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

instance Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap f (Pure x) = Pure (f x)
    fmap f (Op f') = Op (fmap (fmap f) f')

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

-- data class wizardry
infixr 6 +
data (f + g) a
    = L(f a)
    | R(g a)
    deriving Functor

class f < g where
    inj :: f k -> g k

instance f < f where inj = id
instance {-# Overlapping #-}f < (f + g) where inj = L
instance {-# OVERLAPPING #-} f < h 
    => f < (g + h) where inj = R . inj

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


instance Show a => Show (Free End a) where
    show (Pure x) = "Pure("++ show x ++")"
    show (Op x) = case x of

instance (Functor f, Functor g, Show (f a), Show (g a)) => Show ((f + g) a) where
    show (L x) = show x
    show (R x) = show x


instance (Functor f, Functor g, Show a, Show (f (Free (f + g) a)), Show (g (Free (f + g) a)))
    => Show (Free (f + g) a) where
    show (Pure x) = "Pure("++ show x++")"
    show (Op (L x)) = "Op("++ show x ++")"
    show (Op (R x)) = "Op("++ show x ++")"



data Handler f a f' b =
    Handler { ret   :: a -> Free f' b -- Pure case
            , hdlr  :: f (Free f' b) -> Free f' b} -- Op case

-- stateful handler
data Handler_ f a state f' b =
    Handler_ { ret_  :: a -> (state -> Free f' b) -- Pure case
            , hdlr_  :: f (state -> Free f' b) -> (state -> Free f' b)} -- Op case


handle  :: (Functor f, Functor f')
        => Handler f a f' b
        -> Free (f + f') a -> Free f' b
handle h = flatten
    (ret h)
    (\case
        L x -> hdlr h x
        R x -> Op x)

-- stateful handler
handle_ :: (Functor f, Functor f')
        => Handler_ f a state f' b
        -> Free (f + f') a -> state -> Free f' b
handle_ h = flatten
    (ret_ h)
    (\case
        L x -> hdlr_ h x
        R x -> \p -> Op (fmap (\m -> m p) x))


data End k deriving (Functor, Show)

un :: Free End a -> a
un (Pure a) = a
un (Op f) = undefined --  end has no instances

