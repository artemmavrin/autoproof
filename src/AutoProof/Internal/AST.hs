{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AutoProof.Internal.AST
-- Copyright   : (c) Artem Mavrin, 2021
-- License     : BSD3
-- Maintainer  : artemvmavrin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Defines an abstract syntax tree class and related functions.
module AutoProof.Internal.AST
  ( -- * Abstract syntax tree class and metadata type
    AST (Root, root, children, height, size, metadata),
    ASTMetadata (ASTMetadata, getHeight, getSize),

    -- * AST functions
    subtrees,
    properSubtrees,

    -- *  Helper functions for creating AST constructors
    atomicASTConstructor,
    unaryASTConstructor,
    binaryASTConstructor,
    unaryRootedASTConstructor,
    binaryRootedASTConstructor,
    ternaryRootedASTConstructor,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Container type for AST properties, intended for constant-time access.
data ASTMetadata = ASTMetadata
  { -- | Get an AST's height (see also 'height')
    getHeight :: !Int,
    -- | Get an AST's size (see also 'size')
    getSize :: !Int
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

  -- | The AST's metadata
  metadata :: t -> ASTMetadata

  -- | Number of edges on the longest path from the root of the AST to a leaf.
  height :: t -> Int
  height = getHeight . metadata

  -- | Number of nodes in the AST.
  size :: t -> Int
  size = getSize . metadata

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

-- | @('subtrees' t)@ is the set of all subtrees of an AST @t@ (including @t@
-- itself).
subtrees :: (AST t, Ord t) => t -> Set t
subtrees t = foldr (Set.union . subtrees) (Set.singleton t) (children t)

-- | @('properSubtrees' t)@ is the the set of all /proper/ subtrees of an AST
-- @t@ (i.e., not including @t@ itself).
properSubtrees :: (AST t, Ord t) => t -> Set t
properSubtrees t = foldr (Set.union . subtrees) Set.empty (children t)
