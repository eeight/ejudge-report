module Csv( Csv(..)
          , readCsv
          , scanCsvRows
          ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Data.Array
import Control.Monad(liftM)
import Data.Maybe(fromJust)

data Csv = Csv { csv_header :: M.Map B.ByteString Int
               , csv_body :: Array (Int, Int) B.ByteString
               } deriving (Show)

forceString = B.concat . L.toChunks

readCsv :: FilePath -> IO Csv
readCsv filename = do
    lines <- liftM L.lines $ L.readFile filename
    let header = parseHeader $ head lines
    let body = parseBody (M.size header) (tail lines)
    return $ Csv header body
  where
    parseHeader header = let
        fields = B.split ';' $ forceString header
        in
        M.fromList $ zip fields [0..]

    parseBody fieldsNumber body = let
        rows = concatMap (B.split ';') (map forceString body)
        in
        listArray ((0, 0), (length body - 1, fieldsNumber - 1)) rows


scanCsvRows :: [String] -> Csv -> [[B.ByteString]]
scanCsvRows names (Csv header rows) = let
    ids = map (fromJust . (flip M.lookup $ header). B.pack) names
    section row = map (\i -> (rows ! (row, i))) ids
    in
    map section [0..(fst . snd . bounds $ rows) - 1]
