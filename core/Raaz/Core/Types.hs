-- |
--
-- Module      : Raaz.Core.Types
-- Description : The core types of raaz
-- Copyright   : (c) Piyush P Kurur, 2019
-- License     : Apache-2.0 OR BSD-3-Clause
-- Maintainer  : Piyush P Kurur <ppk@iitpkd.ac.in>
-- Stability   : experimental
--

module Raaz.Core.Types
       ( -- * Overview.
         -- $overview$
         module Raaz.Core.Primitive
       , module Raaz.Core.Types.Equality
       , module Raaz.Core.Types.Endian
       , module Raaz.Core.Types.Pointer
       , module Raaz.Core.Types.Tuple
       , module Raaz.Core.Types.Copying
       , module Raaz.Core.Types.Curve25519
       ) where

import Raaz.Core.Types.Equality

import Raaz.Core.Primitive            ( BlockCount                  )

-- Developer note: We want to expose LE and BE without its
-- constructors. This is a ugly hack for it.
import Raaz.Core.Types.Endian  hiding (LE, BE)
import Raaz.Core.Types.Endian         (LE, BE)

import Raaz.Core.Types.Pointer hiding ( AlignedPtr, BYTES)
import Raaz.Core.Types.Pointer        ( AlignedPtr, BYTES)
import Raaz.Core.Types.Tuple   hiding ( map                     )
import Raaz.Core.Types.Copying( Src, Dest, source, destination)
import Raaz.Core.Types.Curve25519( Elem, Scalar)

-- $overview$
--
-- __WARNING:__ If you are just a user of this library, it is unlikely
-- that you will need to import this module. It is only required if
-- you are a developer and want to define a new cryptographic data
-- type.
--
-- A lot of cryptographic code is low level and involves quite a bit
-- of boilerplate and are therefore fertile grounds for bugs. This
-- module describes types specific to raaz that are designed to catch
-- bugs in such low level code. The three principles that we follow
-- in the design are:
--
-- 1. Define distinct types for semantically different objects. For
--    example, distinguish between buffer length/pointer offset in
--    bytes versus other units (see `LengthUnit`) or make endian aware
--    variants of standard word types (see `BE` and `LE`) etc.
--
-- 2. Make sure that the low level functions are sensitive to these
--    types. For example, the function `sizeOf` exposed here returns
--    @`BYTES` `Int`@ instead of just `Int` and functions like
--    `allocaBuffer` are generic enough to work with any length units.
--
-- 3. Provide obvious instances for some basic type and have and
--    idiom/design pattern to build such interfaces for user defined
--    types. For example, we have a very specific way to build timing
--    safe equality functions for arbitrary types. Most of the time,
--    in our case it just amounts to handling product types.
--
-- == Role of Monoids.
--
-- Monoids play an important role in facilitating the top down
-- approach to type safety that we mentioned above. Some types
-- described here have a natural monoid semantics. For example, when
-- dealing with pointer offsets and buffer sizes, we use type safe
-- length units like `BYTES`. These length units are instances of
-- monoids where the underlying operation is addition. On the other
-- hand, when it comes to pointer alignment which is captured by the
-- type `Alignment`, the monoid operation is taking the lowest common
-- multiple.
--
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
