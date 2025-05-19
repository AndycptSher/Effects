{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Effects.SimpleAssignExamples where
import Effects.Effects
import Prelude hiding (log)
import Data.Map (Map, (!), empty)
import Data.Map.Lazy (insert)

data Log k = Log String k deriving (Show, Functor)

log msg = Op (inj (Log msg (Pure ())))

logHandler :: Handler_ Log a (IO ()) f' (a, IO ())
logHandler = Handler_ {
    ret_= \a io -> Pure (a, io),
    hdlr_= \(Log msg k) io -> k (io >> print msg)
}

data Vars t k
    = Assign String t k
    | Retrieve String (t -> k)
    deriving Functor

assign var val = Op (inj (Assign var val (pure ())))

retrieve var = Op (inj (Retrieve var (pure)))

variableHandler :: Handler_ (Vars t) a (Map String t) f' (a)
variableHandler = Handler_ {
    ret_ = \a m -> Pure (a),
    hdlr_ = \x m -> case x of
    (Assign var val k) -> k (insert var val m)
    (Retrieve var k) -> k (m!var) m
}

assignNGet :: (Functor f, Vars Int < f) => Free f Int
assignNGet = do
    assign "R" (2:: Int)
    (e:: Int) <- retrieve "R"
    return e

v :: Free End Int
v = handle_ variableHandler (assignNGet :: Free (Vars Int + End) Int) empty

asgnNLogNRet :: Free (Log + Vars Int + End) ()
asgnNLogNRet = do
    ret <- assignNGet :: Free (Log + Vars Int + End) Int
    log (show ret)

-- data Assign t k = Assign String t k deriving (Show, Functor)

-- assign var val = Op(inj(Assign var val (Pure ())))

-- assignNLog :: Free (Log + Assign Int + End) ()
-- assignNLog = do
--     assign "V" (1:: Int)
--     log "hey"
--     assign "S" (2::Int)

-- assignHandler :: Handler_ (Assign t) a [(String, t)] f' (a, [(String, t)])
-- assignHandler = Handler_ {
--     ret_ = \a b -> Pure (a, b), 
--     hdlr_ = \(Assign var val k) mapping -> k ((var, val):mapping)
-- }


-- assignF = handle_ logHandler assignNLog (pure ())

-- logF = permute commSum $ handle_ assignHandler (permute (assocSum' . commSum) assignNLog) []
