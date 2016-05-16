module Text.Parser.Comments
       ( LineCommentT(..)
       , BlockCommentT(..)
       ) where

import Data.Proxy
import Control.Applicative
import Control.Monad
import GHC.TypeLits
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token

skipComment :: TokenParsing m => m end -> m ()
skipComment comment = doComment <|> doSpace
  where doComment = comment *> optional (skipComment comment) *> pure ()
        doSpace = someSpace *> optional doComment *> pure ()

newtype LineCommentT (start :: Symbol) m a =
  LineCommentT { runLineCommentT :: m a }
  deriving ( Functor, Applicative, Monad, MonadPlus
           , Alternative, Parsing, CharParsing
           )

instance (KnownSymbol start, TokenParsing m) => TokenParsing (LineCommentT start m) where
  someSpace = LineCommentT $ skipComment comment
    where comment = try (string $ symbolVal (Proxy :: Proxy start))
                    *> manyTill anyChar (void (char '\n') <|> eof)

  nesting = LineCommentT . nesting . runLineCommentT
  semi = LineCommentT semi
  highlight h c = LineCommentT $ highlight h $ runLineCommentT c
  token = LineCommentT . token . runLineCommentT

newtype BlockCommentT (start :: Symbol) (end :: Symbol) m a =
  BlockCommentT { runBlockCommentT :: m a }
  deriving ( Functor, Applicative, Monad, MonadPlus
           , Alternative, Parsing, CharParsing
           )

instance (TokenParsing m, KnownSymbol start, KnownSymbol end) => TokenParsing (BlockCommentT start end m) where
  someSpace = BlockCommentT $ skipComment comment
    where comment = try (string $ symbolVal (Proxy :: Proxy start))
                    *> manyTill anyChar (try $ string $ symbolVal (Proxy :: Proxy end))

  nesting = BlockCommentT . nesting . runBlockCommentT
  semi = BlockCommentT semi
  highlight h c = BlockCommentT $ highlight h $ runBlockCommentT c
  token = BlockCommentT . token . runBlockCommentT
