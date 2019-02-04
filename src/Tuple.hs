module Tuple where

    -- | Extract the 'fst' of a triple.
    fst3 :: (a,b,c) -> a
    fst3 (a,b,c) = a

    -- | Extract the 'snd' of a triple.
    snd3 :: (a,b,c) -> b
    snd3 (a,b,c) = b

    -- | Extract the final element of a triple.
    thd3 :: (a,b,c) -> c
    thd3 (a,b,c) = c
