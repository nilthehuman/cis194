-- Convey.hs

-- A toolkit of handy combinators for Arrows
-- (I haven't quite figured Stack out so I'm just dumping these definitions here
-- instead of referring to another package. TODO: import Convey properly.)

module Control.Arrow.Convey
    (
      recombine1
    , recombine2
    , recombine3
    , recombine4
    , recombineA1
    , recombineA2
    , recombineA3
    , recombineA4
    ) where

import Data.Functor        ( Functor )
import Control.Category    ( Category )
import Control.Applicative ( Applicative, liftA2, liftA3, (<*>) )
import Control.Arrow

-- Auxiliary functions
liftA4 f a b c d = fmap f a <*> b <*> c <*> d
flatten3     (x, (y, z))  =    (x, y, z)
flatten4 (w, (x, (y, z))) = (w, x, y, z)
uncurry3 c    (x, y, z)   = c   x y z
uncurry4 c (w, x, y, z)   = c w x y z

-- recombine1 is a degenerate case included only for the sake of consistency.
recombine1 :: Category cat => cat b c -> cat a b -> cat a c
recombine1 = (<<<)

recombine2 :: Arrow a => (c -> d -> e)           -> a b c -> a b d -> a b e
recombine2 c f g     = (f &&& g)                          >>^ uncurry  c

recombine3 :: Arrow a => (c -> d -> e -> f)      -> a b c -> a b d -> a b e -> a b f
recombine3 c f g h   = (f &&& g &&& h)       >>^ flatten3 >>^ uncurry3 c

recombine4 :: Arrow a => (c -> d -> e -> f -> g) -> a b c -> a b d -> a b e -> a b f -> a b g
recombine4 c f g h i = (f &&& g &&& h &&& i) >>^ flatten4 >>^ uncurry4 c

-- recombineA1 is a degenerate case included only for the sake of consistency.
recombineA1 ::     (Functor f, Arrow a) => (c -> d)                -> a b (f c) -> a b (f d)
recombineA1 c f       = f >>^ fmap c

recombineA2 :: (Applicative f, Arrow a) => (c -> d -> e)           -> a b (f c) -> a b (f d) -> a b (f e)
recombineA2 c f g     = (f &&& g)                          >>^ uncurry  (liftA2 c)

recombineA3 :: (Applicative f, Arrow a) => (c -> d -> e -> g)      -> a b (f c) -> a b (f d) -> a b (f e) -> a b (f g)
recombineA3 c f g h   = (f &&& g &&& h)       >>^ flatten3 >>^ uncurry3 (liftA3 c)

recombineA4 :: (Applicative f, Arrow a) => (c -> d -> e -> g -> h) -> a b (f c) -> a b (f d) -> a b (f e) -> a b (f g) -> a b (f h)
recombineA4 c f g h i = (f &&& g &&& h &&& i) >>^ flatten4 >>^ uncurry4 (liftA4 c)

-- The above combinators aren't worth defining further than 4 fan-out arguments imo
-- because one would lose track of the arguments easily.

recombineMap :: (b -> c -> c) -> c -> [a -> b] -> a -> c
recombineMap c z fs x = foldr c z $ map ($ x) fs

