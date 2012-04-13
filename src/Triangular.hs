module Triangular (
    Mesh, parseMeshFile
) where

import Control.Monad
import Text.ParserCombinators.Parsec

--           Coordinates of points,   Edges,        Triangles
type Mesh = ([(Float, Float, Float)], [(Int, Int)], [(Int, Int, Int)])


parseInt :: Parser Int
parseInt = do
  sign <- option ' ' (char '-')
  str <- many1 (digit)
  return (read (sign : str) :: Int)

parseFloat :: Parser Float
parseFloat = do
  sign <- option ' ' (char '-')
  str <- many1 (digit <|> char '.')
  return (read (sign : str) :: Float)

parseHeader :: Parser (Int,Int,Int,Int)
parseHeader = do
  dim <- parseInt
  char '\n'
  pointcount <- parseInt
  spaces
  facetcount <- parseInt
  spaces
  parseInt -- Edgecount?!
  return (dim,pointcount,facetcount,0)
  <?> "Header must equal <Dimensions>NEWLINE<No of points> <No of facets> <No of edges>"

parsePoints :: Int -> Int -> Parser [[Float]]
parsePoints dim pointcount = do count pointcount (skipMany space >> parsePoint dim)

parsePoint :: Int -> Parser [Float]
parsePoint dim = do
  spaces
  pts <- count dim (skipMany space >> parseFloat)
  return pts
  <?> "Points must be given as followed <x1> <x2> ... <xn> Where n is <Dimensions>, i.e. the first number in the file"

parseFacets :: Int -> Parser [(Int, Int, Int)]
parseFacets n = do count n (skipMany space >> parseFacet)

parseFacet :: Parser (Int, Int, Int)
parseFacet = do
  digit -- Dimension, should be 3
  spaces
  idxs <- count 3 (skipMany space >> parseInt)
  return (idxs !! 0, idxs !! 1, idxs !! 2)

ensure3d :: [[Float]] -> [(Float, Float, Float)]
ensure3d list = map f list
  where
    f []              = (0,0,0)
    f [x]             = (x,0,0)
    f [x,z]           = (x,0,z)
    f (x : y : z : _) = (x,y,z)

extractEdges :: [(Int, Int, Int)] -> [(Int, Int)]
extractEdges facets = foldr f [] facets
  where
    f (i,j,k) acc = (i,j) : (j,k) : (i,k) : acc

parseMeshFile :: SourceName -> IO (Either ParseError Mesh)
parseMeshFile name = parseFromFile myParser name
  where
   myParser = do {
      (dim,pts,fcts,edgs) <- parseHeader;
      points <- parsePoints dim pts;
      facets <- parseFacets fcts;
      return (ensure3d points,extractEdges facets,facets)
     }
