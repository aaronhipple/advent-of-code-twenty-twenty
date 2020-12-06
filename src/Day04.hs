-- | 

module Day04 (day04, run04a, run04b) where

import Parsing
import Data.Maybe
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

day04 input = unlines
  [ "Part 1: " ++ (show $ run04a input)
  , "Part 2: " ++ (show $ run04b input)
  ]

run04a :: String -> Int
run04a input = length $ filter isValid $ maybe [] id $ runParse passports input 
  
requiredTags = Set.fromList
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  ]

isValid :: PassportData -> Bool
isValid (PassportData pairs) = requiredTags `Set.isSubsetOf` Set.fromList (map tag pairs)

data PassportData = PassportData [DataPair]
  deriving Show

data DataPair = DataPair
  { tag :: String
  , value :: String
  }
  deriving Show
  
passports = many passport
  
passport = PassportData <$> some pdata <* (skip (char '\n') <|> eof)

dataPair :: String -> Parser DataPair
dataPair tag = DataPair tag <$> (string tag *> string ":" *> (many $ sat (invert isSpace)))
  
pdata = (dataPair "byr"
        <|> dataPair "iyr"
        <|> dataPair "eyr"
        <|> dataPair "hgt"
        <|> dataPair "hcl"
        <|> dataPair "ecl"
        <|> dataPair "pid"
        <|> dataPair "cid") <* (skip (char ' ' <|> char '\n') <|> eof)

invert :: (a -> Bool) -> a -> Bool
invert pred a = not $ pred a

run04b :: String -> Int
run04b input = length $ mapMaybe toPassport $ filter isValid $ maybe [] id $ runParse passports input

data Passport = Passport
  { byr :: Int
  , iyr :: Int
  , eyr :: Int
  , hgt :: (Int, LengthUnit)
  , hcl :: String
  , ecl :: EyeColor
  , pid :: String
  , cid :: Maybe String
  }

data LengthUnit = Inch | Centimeter
data EyeColor = Amber | Blue | Brown | Gray | Green | Hazel | Other

toPassport :: PassportData -> Maybe Passport
toPassport (PassportData pairs) = case validated of
  ( Just byr_
    , Just iyr_
    , Just eyr_
    , Just hgt_
    , Just hcl_
    , Just ecl_
    , Just pid_
    , cid_
    ) -> Just $ Passport byr_ iyr_ eyr_ hgt_ hcl_ ecl_ pid_ cid_
  _ -> Nothing
  where
    validated =
      ( Map.lookup "byr" pairMap >>= mkbyr
      , Map.lookup "iyr" pairMap >>= mkiyr
      , Map.lookup "eyr" pairMap >>= mkeyr
      , Map.lookup "hgt" pairMap >>= mkhgt
      , Map.lookup "hcl" pairMap >>= mkhcl
      , Map.lookup "ecl" pairMap >>= mkecl
      , Map.lookup "pid" pairMap >>= mkpid
      , Map.lookup "cid" pairMap >>= mkcid
      )
    pairMap = Map.fromList $ map (\x -> (tag x, value x)) pairs

mkbyr yr = mkyr yr >>= validateInRange (1920, 2002)
mkiyr yr = mkyr yr >>= validateInRange (2010, 2020)
mkeyr yr = mkyr yr >>= validateInRange (2020, 2030)

mkyr :: String -> Maybe Int
mkyr yr
  | all isDigit yr && length yr == 4 = Just (read yr)
  | otherwise = Nothing

validateInRange :: (Int, Int) -> Int -> Maybe Int
validateInRange (low, high) n
  | n >= low && n <= high = Just n
  | otherwise = Nothing
  
mkhgt :: String -> Maybe (Int, LengthUnit)
mkhgt inp = runParse p inp >>= validate
  where
    p = (,) <$> int <*> lengthUnit

    lengthUnit = inch <|> cm
    inch = string "in" *> pure Inch
    cm = string "cm" *> pure Centimeter

    validate ht@(n, Inch) = if n >= 59 && n <= 76 then Just ht else Nothing
    validate ht@(n, Centimeter) = if n >= 150 && n <= 193 then Just ht else Nothing
  
mkhcl :: String -> Maybe String
mkhcl cl@('#':xs)
  | all isHexDigit xs && length xs == 6 = Just cl
  | otherwise = Nothing
mkhcl _ = Nothing
  
mkecl :: String -> Maybe EyeColor
mkecl "amb" = Just Amber
mkecl "blu" = Just Blue
mkecl "brn" = Just Brown
mkecl "gry" = Just Gray
mkecl "grn" = Just Green
mkecl "hzl" = Just Hazel
mkecl "oth" = Just Other
mkecl _ = Nothing
  
mkpid :: String -> Maybe String
mkpid n
  | all isDigit n && length n == 9 = Just n
  | otherwise = Nothing
  
mkcid :: String -> Maybe String
mkcid n = Just n
