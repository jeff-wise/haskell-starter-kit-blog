
{-| Application Prelude
    
    Common definitions and imports for application.
    Assumes that the default prelude is hidden (-XNoImplicitPrelude).
-}

module Blog.Prelude
  ( -- * Basic types and functions
      -- ** Operators
      (Prelude.$)
    , (Prelude.$!)
    , (Prelude.&&)
    , (Prelude.||)
    , (Control.Category..)
    , (Control.Category.>>>)
    , (Control.Category.<<<)
      -- ** Functions
    , Prelude.id
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Prelude.odd
    , Prelude.even
    , Prelude.uncurry
    , Prelude.curry
    , Data.Tuple.swap
      -- ** Type classes
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Show (..)
    , Prelude.Read
    , Data.String.IsString (..)
      -- ** Ord
    , Data.Ord.Ordering (..)
    , Data.Ord.comparing
      -- ** Numeric type classes
    , Prelude.Num (..)
    , Prelude.Real (..)
    , Prelude.Integral (..)
    , Prelude.Fractional (..)
    , Prelude.Floating (..)
    , Prelude.RealFrac (..)
    , Prelude.RealFloat(..)
      -- ** Basic Data types
    , Prelude.Bool (..)
    , Prelude.Char
    , Prelude.String
    , Data.Text.Text
    , Prelude.IO
      -- ** Numbers
    , Word, Word8, Word32, Word64
    , Prelude.Int
    , Int32, Int64
    , Prelude.Integer
    , Prelude.Rational
    , Prelude.Float
    , Prelude.Double
      -- ** Numeric functions
    , (Prelude.^)
    , (Prelude.^^)
    , Prelude.subtract
    , Prelude.fromIntegral
    , Prelude.realToFrac
    -- * Typeclassopedia
    -- ** Monoid
    , Data.Monoid.Monoid (..)
    , (Data.Monoid.<>)
    -- , (++)
    -- ** Functor
    , Data.Functor.Functor
    , Data.Functor.fmap
    , (Data.Functor.<$>)
      -- ** Applicative
    , Control.Applicative.Applicative
    , Control.Applicative.Alternative
    , (Control.Applicative.<**>)
    , (Control.Applicative.<*>)
    , (Control.Applicative.<|>)
    -- ** Arrow
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.***)
    , (Control.Arrow.&&&)
      -- ** Monad
    , Control.Monad.Monad
    , (Control.Monad.>>=)
    , (Control.Monad.>>)
    , Control.Monad.return
    , Control.Monad.fail
    , Control.Monad.MonadPlus
    , Control.Monad.mzero
    , Control.Monad.mplus
    , Control.Monad.when
    , Control.Monad.unless
    , Control.Monad.forever
    , Control.Monad.join
    , Control.Monad.guard
    , (Control.Monad.=<<)
    , (Control.Monad.>=>)
    , (Control.Monad.<=<)
      -- ** Folds and Traversals
    , Data.Foldable.Foldable
    , Data.Foldable.fold
    , Data.Foldable.foldMap
    , Data.Foldable.foldr
    , Data.Foldable.foldr'
    , Data.Foldable.foldl
    , Data.Foldable.foldl'
    , Data.Foldable.foldr1
    , Data.Foldable.foldl1
    , Data.Foldable.toList
    , Data.Foldable.null
    , Data.Foldable.length
    , Data.Foldable.elem
    , Data.Foldable.maximum
    , Data.Foldable.minimum
    , Data.Foldable.sum
    , Data.Foldable.product
    , Data.Foldable.foldrM
    , Data.Foldable.foldlM
    , Data.Foldable.traverse_
    , Data.Foldable.for_
    , Data.Foldable.sequenceA_
    , Data.Foldable.asum
    , Data.Foldable.mapM_
    , Data.Foldable.forM_
    , Data.Foldable.sequence_
    , Data.Foldable.msum
    , Data.Foldable.concat
    , Data.Foldable.concatMap
    , Data.Foldable.and
    , Data.Foldable.or
    , Data.Foldable.any
    , Data.Foldable.all
    , Data.Foldable.maximumBy
    , Data.Foldable.minimumBy
    , Data.Foldable.notElem
    , Data.Foldable.find
    , Data.Traversable.Traversable
    , Data.Traversable.traverse
    , Data.Traversable.sequenceA
    , Data.Traversable.mapM
    , Data.Traversable.sequence
    , Data.Traversable.for
    , Data.Traversable.forM
    -- * Common Polymorphic Types
    -- ** Maybe
    , Data.Maybe.Maybe (..)
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
    , Data.Maybe.isJust
    , Data.Maybe.isNothing
    , Data.Maybe.listToMaybe
    , Data.Maybe.maybeToList
      -- ** Either
    , Data.Either.Either (..)
    , Data.Either.partitionEithers
    , Data.Either.lefts
    , Data.Either.rights
    -- * I/O
    , Control.Monad.IO.Class.MonadIO
    , Control.Monad.IO.Class.liftIO
    , System.IO.putStrLn
    , System.IO.print
    -- * Lenses
    ) where




-- BASE PACKAGES
--------------------------------------------------------------------------------

import qualified Prelude

import qualified Data.Either
import qualified Data.Maybe
import qualified Data.Ord
import qualified Data.String
import qualified Data.Tuple

import Data.Word (Word8, Word32, Word64, Word)
import Data.Int (Int32, Int64)

import qualified Data.Monoid
import qualified Data.Functor
import qualified Control.Applicative 
import qualified Control.Arrow
import qualified Control.Category
import qualified Control.Monad 
import qualified Data.Foldable
import qualified Data.Traversable (Traversable (..), for, forM)

import qualified Control.Monad.IO.Class

import qualified System.IO


-- NON-BASE PACKAGES
--------------------------------------------------------------------------------

import qualified Data.Text


{-# ANN module "HLint: ignore Use import/export shortcut" #-}
