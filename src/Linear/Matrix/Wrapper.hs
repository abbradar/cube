-- | Wrapper types for matrices to define proper instances.

module Linear.Matrix.Wrapper where

import Data.Data
import Data.Ix
import GHC.Generics
import Foreign.Storable
import Linear

newtype WM22 a = WM22 (M22 a)
               deriving ( Show, Eq, Ord, Generic, Functor, Foldable, Traversable
                        , Data, Read, Ix
                        )
               deriving newtype ( Num, Storable
                                )

newtype WM33 a = WM33 (M33 a)
               deriving ( Show, Eq, Ord, Generic, Functor, Foldable, Traversable
                        , Data, Read, Ix
                        )
               deriving newtype ( Num, Storable
                                )

newtype WM44 a = WM44 (M44 a)
               deriving ( Show, Eq, Ord, Generic, Functor, Foldable, Traversable
                        , Data, Read, Ix
                        )
               deriving newtype ( Num, Storable
                                )
