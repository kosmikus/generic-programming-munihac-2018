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

data Code =
    Zero
  | One
  | Const Type
  | Plus Code Code
  | Times Code Code

type Rep a = Run (CodeOf a)

class Generic a where
  type CodeOf a :: Code

  from :: a -> Rep a
  to :: Rep a -> a

instance Generic Bool where
  type CodeOf Bool = Plus One One

  from :: Bool -> Rep Bool
  from False = Left ()
  from True  = Right ()

  to :: Rep Bool -> Bool
  to (Left ()) = False
  to (Right ()) = True

data List a = Nil | Cons a (List a)
  deriving Show
  deriving (MyEnum, Eq) via FromGeneric (List a)

instance Generic (List a) where
  type CodeOf (List a) = Plus One (Times (Const a) (Const (List a)))

  from :: List a -> Rep (List a)
  from Nil = Left ()
  from (Cons x xs) = Right (x, xs)

  to :: Rep (List a) -> List a
  to (Left ()) = Nil
  to (Right (x, xs)) = Cons x xs

class MyEnum a where
  enum :: [a]

newtype FromGeneric a = FromGeneric a

instance (Generic a, GEnum (CodeOf a)) => MyEnum (FromGeneric a) where
  enum = map (FromGeneric . to) (genum @(CodeOf a))

instance (Generic a, GEq (CodeOf a)) => Eq (FromGeneric a) where
  FromGeneric x1 == FromGeneric x2 = geq @(CodeOf a) (from x1) (from x2)

deriving via FromGeneric Bool instance MyEnum Bool

{-
instance MyEnum Bool where
  enum = map to (genum @(CodeOf Bool))
-}

class GEnum (c :: Code) where
  genum :: [Run c]

instance GEnum Zero where
  genum = []

instance GEnum One where
  genum = [ () ]

instance (GEnum c1, GEnum c2) => GEnum (Plus c1 c2) where
  genum =
    map Left (genum @c1) ++ map Right (genum @c2)

instance (GEnum c1, GEnum c2) => GEnum (Times c1 c2) where
  genum =
    [ (x, y) | x <- genum @c1, y <- genum @c2 ]

instance MyEnum a => GEnum (Const a) where
  genum = enum

type family Run (c :: Code) :: Type where
  Run Zero          = Void
  Run One           = ()
  Run (Const a)     = a
  Run (Plus c1 c2)  = Either (Run c1) (Run c2)
  Run (Times c1 c2) = (Run c1, Run c2)















class GEq (c :: Code) where
  geq :: Run c -> Run c -> Bool

instance GEq Zero where
  geq x _ = absurd x

instance GEq One where
  geq () () = True

instance (GEq c1, GEq c2) => GEq (Plus c1 c2) where
  geq (Left x1) (Left x2) = geq @c1 x1 x2
  geq (Right y1) (Right y2) = geq @c2 y1 y2
  geq _ _ = False

instance (GEq c1, GEq c2) => GEq (Times c1 c2) where
  geq (x1, y1) (x2, y2) =
    geq @c1 x1 x2 && geq @c2 y1 y2

instance Eq a => (GEq (Const a)) where
  geq x y = x == y
