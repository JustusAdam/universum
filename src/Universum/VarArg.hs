{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Provides operator of variable-arguments function composition.

module Universum.VarArg
       ( SuperComposition(..)
       , mkVarArg
       , VarArg
       ) where

-- $setup
-- >>> import Universum.Base ((+))
-- >>> import Universum.Container (null)
-- >>> import Prelude (show)
-- >>> import Data.List (zip5)

import Data.Function ((.))

-- | Class machinery to create var-arg functions
class VarArg a b c d where
  -- | A higher order function that turns a function from @[a]@ to @b@ into a variadic function from @a@'s to @b@.
  --
  -- As an example you can use it to create 'printf' like functions.
  --
  mkVarArg :: (d -> a) -> ([a] -> b) -> c
instance VarArg a b c d => VarArg a b (d -> c) d where
  mkVarArg g f a = mkVarArg g (f . (g a:))
  {-# INLINE mkVarArg #-}
instance VarArg a c c d where
  mkVarArg _ f = f []
  {-# INLINE mkVarArg #-}


-- | This type class allows to implement variadic composition operator.
class SuperComposition a b c | a b -> c where
    -- | Allows to apply function to result of another function with multiple
    -- arguments.
    --
    -- >>> (show ... (+)) 1 2
    -- "3"
    -- >>> show ... 5
    -- "5"
    -- >>> (null ... zip5) [1] [2] [3] [] [5]
    -- True
    --
    -- Inspired by <http://stackoverflow.com/questions/9656797/variadic-compose-function>.
    --
    -- ==== Performance
    -- To check the performance there was done a bunch of benchmarks. Benchmarks were made on
    -- examples given above and also on the functions of many arguments.
    -- The results are showing that the operator ('...') performs as fast as
    -- plain applications of the operator ('Prelude..') on almost all the tests, but ('...')
    -- leads to the performance draw-down if @ghc@ fails to inline it.
    -- Slow behavior was noticed on functions without type specifications.
    -- That's why keep in mind that providing explicit type declarations for functions is very
    -- important when using ('...').
    -- Relying on type inference will lead to the situation when all optimizations
    -- disappear due to very general inferred type. However, functions without type
    -- specification but with applied @INLINE@ pragma are fast again.
    --
    (...) :: a -> b -> c

infixl 8 ...

instance (a ~ c, r ~ b) =>
         SuperComposition (a -> b) c r where
    f ... g = f g
    {-# INLINE (...) #-}

instance (SuperComposition (a -> b) d r1, r ~ (c -> r1)) =>
         SuperComposition (a -> b) (c -> d) r where
    (f ... g) c = f ... g c
    {-# INLINE (...) #-}
