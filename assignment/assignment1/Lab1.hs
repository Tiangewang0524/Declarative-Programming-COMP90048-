module Lab1 (subst, interleave, unroll) where

subst :: Eq t => t -> t -> [t] -> [t]
subst _ _ [] = []
subst a b (c:cs) =
    if a == c then b:(subst a b cs)
    else c:(subst a b cs)

interleave :: [t] -> [t] -> [t]
interleave as [] = as
interleave [] bs = bs
interleave (a:as) (b:bs) = a:b:(interleave as bs)

unroll :: Int -> [a] -> [a]
unroll a [] = []
unroll 0 bs = []
unroll a (b:bs) = b:(unroll (a-1) (bs++[b]))
