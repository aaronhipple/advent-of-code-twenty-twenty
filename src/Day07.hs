-- | 

module Day07 (day07, run07a, run07b) where

import Parsing
import Data.List (intercalate, elemIndex)
import qualified Data.Graph as Graph
import Data.Map (Map)
import qualified Data.Map as Map

day07 :: String -> String
day07 input = unlines
  [ "Part 1: " ++ (show $ run07a input)
  , "Part 2: " ++ (show $ run07b input)
  ]
    
run07a :: String -> String
run07a input = show $ do
  specs <- runParse bagSpecs input
  let (graph, _nodeFromVertex, vertexFromKey) = Graph.graphFromEdges $ toEdge <$> specs
  vertex <- vertexFromKey "shiny gold"
  let otherVertices = filter (/= vertex) (Graph.vertices graph)
  pure $ length $ filter (\v -> Graph.path graph v vertex) otherVertices
  
run07b :: String -> String
run07b input = show $ do
  specs <- runParse bagSpecs input
  let m = Map.fromList $ toTuple <$> specs
  (+ (-1)) <$> sumFrom m "shiny gold"
  where
    sumFrom :: Map String [(Int, String)] -> String -> Maybe Int
    sumFrom m key = (+ 1) <$> childSum
      where
        specChildren = Map.lookup key m

        childSum :: Maybe Int
        childSum = (sum . (fmap sum)) <$> ((fmap . traverse) sumForChild specChildren)
        
        sumForChild :: (Int, String) -> Maybe Int
        sumForChild (n, key') = (* n) <$> sumFrom m key'
        
    
toTuple :: BagSpec -> (String, [(Int, String)])
toTuple (BagSpec key connected) = (key, connected)

toEdge :: BagSpec -> (BagSpec, String, [String])
toEdge spec@(BagSpec key connected) = (spec, key, snd <$> connected)

data BagSpec = BagSpec String [(Int, String)] deriving Show

bagSpecs :: Parser [BagSpec]
bagSpecs = many (bagSpec <* (skip (char '\n') <|> eof))

bagSpec :: Parser BagSpec
bagSpec = BagSpec <$> bag <* word (string "contain") <*> containedBags

containedBags :: Parser [(Int, String)]
containedBags = (string "no other bags." *> pure [])
  <|> (many (bagWithCount <* (string ", " <|> string ".")))

bagWithCount :: Parser (Int, String)
bagWithCount = (,) <$> word int <*> bag

bag :: Parser String
bag = collect <$> (endWord <|> ((:) <$> colorWord <*> bagTail))
  where
    collect = intercalate " " . filter (/= "") -- ew. I think there's a better way but eh.
    endWord = word (string "bags" <|> string "bag") *> pure []
    colorWord = word ident
    bagTail = (:[]) <$> bag

word :: Parser a -> Parser a
word p = sp *> p <* sp
  where sp = many (sat (== ' ')) *> pure ()
  
