{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AutoProof.AST
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines an abstract syntax tree class and related functions.
module AutoProof.AST
  ( -- * Abstract syntax tree class and metadata type
    AST (Root, root, children, height, size, metadata),
    ASTMetadata (ASTMetadata, getHeight, getSize),

    -- *  Helper functions for creating AST constructors
    atomicASTConstructor,
    unaryASTConstructor,
    binaryASTConstructor,
    unaryRootedASTConstructor,
    binaryRootedASTConstructor,
    ternaryRootedASTConstructor,
  )
where

-- | Container type for AST properties, intended for constant-time access.
data ASTMetadata = ASTMetadata
  { -- | Get an AST's height (see also 'height')
    getHeight :: Int,
    -- | Get an AST's size (see also 'size')
    getSize :: Int
  }
  deriving (Eq, Ord)

-- | Abstract syntax tree class.
class AST t where
  -- | The type of the values annotating AST nodes.
  type Root t

  -- | The value at the AST's root node.
  root :: t -> Root t

  -- | The AST's child ASTs.
  children :: t -> [t]

  -- | Number of edges on the longest path from the root of the AST to a leaf.
  height :: t -> Int
  height = getHeight . metadata

  -- | Number of nodes in the AST.
  size :: t -> Int
  size = getSize . metadata

  -- | The AST's metadata
  metadata :: t -> ASTMetadata
  metadata t = ASTMetadata {getHeight = height t, getSize = size t}

  {-# MINIMAL root, children, (metadata | height, size) #-}

-- Helper functions for constructing ASTs

-- | Helper function for creating metadata-aware ASTs.
atomicASTConstructor :: (ASTMetadata -> a -> t) -> a -> t
atomicASTConstructor g = g atomicMetadata

-- | Helper function for creating metadata-aware ASTs.
unaryASTConstructor :: AST t => (ASTMetadata -> t -> t) -> t -> t
unaryASTConstructor g t = g (unaryMetadata t) t

-- | Helper function for creating metadata-aware ASTs.
binaryASTConstructor :: AST t => (ASTMetadata -> t -> t -> t) -> t -> t -> t
binaryASTConstructor g t u = g (binaryMetadata t u) t u

-- | Helper function for creating metadata-aware rooted ASTs.
unaryRootedASTConstructor :: AST t => (ASTMetadata -> a -> t -> t) -> a -> t -> t
unaryRootedASTConstructor g a t = g (unaryMetadata t) a t

-- | Helper function for creating metadata-aware rooted ASTs.
binaryRootedASTConstructor :: AST t => (ASTMetadata -> a -> t -> t -> t) -> a -> t -> t -> t
binaryRootedASTConstructor g a t u = g (binaryMetadata t u) a t u

-- | Helper function for creating metadata-aware rooted ASTs.
ternaryRootedASTConstructor :: AST t => (ASTMetadata -> a -> t -> t -> t -> t) -> a -> t -> t -> t -> t
ternaryRootedASTConstructor g a t u v = g (ternaryMetadata t u v) a t u v

-- Internal helper functions for computing metadata for new ASTs

atomicMetadata :: ASTMetadata
atomicMetadata = ASTMetadata {getHeight = 0, getSize = 1}

unaryMetadata :: AST t => t -> ASTMetadata
unaryMetadata t = ASTMetadata {getHeight = 1 + height t, getSize = 1 + size t}

binaryMetadata :: AST t => t -> t -> ASTMetadata
binaryMetadata t u =
  ASTMetadata
    { getHeight = 1 + max (height t) (height u),
      getSize = 1 + size t + size u
    }

ternaryMetadata :: AST t => t -> t -> t -> ASTMetadata
ternaryMetadata t u v =
  ASTMetadata
    { getHeight = 1 + max (height t) (max (height u) (height v)),
      getSize = 1 + size t + size u + size v
    }
