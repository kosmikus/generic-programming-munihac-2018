--
-- An Introduction to Datatype-Generic Programming in Haskell
--
-- Andres Löh
-- MuniHac, 2018-11-17
--

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Demo where

import Data.Kind
import Data.Void


data Foo = MkFoo Int Char

-- (Int, Char)

pairToFoo :: (Int, Char) -> Foo
pairToFoo (x, y) = MkFoo x y

fooToPair :: Foo -> (Int, Char)
fooToPair (MkFoo x y) = (x, y)


type Code = Nat

type Rep a = Run (CodeOf a)

class Generic a where
  type CodeOf a :: Code

  from :: a -> Rep a
  to :: Rep a -> a

instance Generic Bool where
  type CodeOf Bool = Suc (Suc Zero)

  from :: Bool -> Rep Bool
  from False = Nothing
  from True  = Just Nothing

  to :: Rep Bool -> Bool
  to Nothing = False
  to (Just Nothing) = True

class MyEnum a where
  enum :: [a]

newtype FromGeneric a = FromGeneric a

instance (Generic a, GEnum (CodeOf a)) => MyEnum (FromGeneric a) where
  enum = map (FromGeneric . to) (genum @(CodeOf a))

deriving via FromGeneric Bool instance MyEnum Bool

{-
instance MyEnum Bool where
  enum = map to (genum @(CodeOf Bool))
-}

class GEnum (c :: Code) where
  genum :: [Run c]

instance GEnum Zero where
  genum = []

instance GEnum c => GEnum (Suc c) where
  genum = Nothing : map Just (genum @c)

data Nat = Zero | Suc Nat

type family Run (c :: Code) :: Type where
  Run Zero = Void
  Run (Suc n) = Maybe (Run n)

