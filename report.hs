#!/usr/bin/runhaskell

import Csv(Csv, readCsv, scanCsvRows)

import Control.Arrow(first)
import Data.Maybe(fromJust)
import Data.List(foldl')
import Text.Printf(printf)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

data RunStatus = OK | WA | TL | ML | RE | PE | SV | CE
    deriving (Show, Eq, Ord)

data Run = Run { run_user :: B.ByteString
               , run_problem :: B.ByteString
               , run_status :: RunStatus
               } deriving (Show)

penalty :: RunStatus -> Int
penalty OK = 0
penalty WA = 1
penalty PE = 1
penalty TL = 2
penalty ML = 2
penalty RE = 2
penalty _ = 0

runsFromCsv :: Csv -> [Run]
runsFromCsv = map go . scanCsvRows [ "User_Name", "Prob", "Stat_Short"] where
    go [name, problem, status] = Run name problem (parseStatus status)
    parseStatus status = let
        m = M.fromList . map (first B.pack) $  [ ("OK", OK)
                                               , ("WA", WA)
                                               , ("TL", TL)
                                               , ("ML", ML)
                                               , ("RT", RE)
                                               , ("PE", PE)
                                               , ("", SV)
                                               , ("CE", CE)
                                               ]
        lookup s = case M.lookup s m of
            Just s' -> s'
            Nothing -> error $ "Unknown status: `" ++ (B.unpack s) ++ "'"
        in
            lookup status

type ResultMap = M.Map (B.ByteString, B.ByteString) [RunStatus]

gatherRuns :: [Run] -> ResultMap
gatherRuns = M.map reverse . foldl' add M.empty where
    add acc (Run user problem status) = let
        key = (user, problem)
        in
        case M.lookup key acc of
            Just ss@(OK:_) -> acc
            Just ss -> M.insert key (status:ss) acc
            Nothing -> M.insert key [status] acc

printResults :: ResultMap -> IO ()
printResults = mapM_ printOne . M.toList where
    printOne ((user, problem), record) = do
        let score = if (last record) == OK
            then Just $ 10 - min 10 (sum . map penalty $ record)
            else Nothing
        B.putStr $ B.intercalate (B.pack ": ") [user, problem]
        putStrLn $ printf "\t%s (= %s)" (show record) (show score)

main = do
    c <- readCsv "/dev/stdin"
    let runs = runsFromCsv c
    printResults $ gatherRuns runs
