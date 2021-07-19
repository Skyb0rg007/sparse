
# Sparse

## A Standard ML library for efficient parsing combinators with customizable error handling

This library is a port of the [Megaparsec](https://hackage.haskell.org/package/megaparsec) Haskell library.

## Design

The SparseFn functor will create the base of the library, including all primitive
combinators. All other combinators are based off of these primitives, so you are
free to write your own derived parsers without any loss of efficiency.

The error handling provided by Sparse uses file offsets, which is likely not
enough for dealing with user-facing error messages. The SourcePosFn functor
uses additional state to provide line+column information.

## SparseFn

The main functor, creates the primitive parser combinators

## SparseExtrasFn

Creates the parser combinators you'd want for most tasks

## SparseCharExtrasFn

Creates combinators for working with character data

## StringInput

Structure containing information for string-based parsers.
Uses `Substring` as the input datatype.

## SortedVectorFn

Creates a set-like data structure backed by a `vector`. Use case is for
small sets, so it is a very space-efficient representation.

