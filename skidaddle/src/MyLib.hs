{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module MyLib where

import Choreograph ( toInstr, MixedInstr )
import WAT ( emit )
import ReductionRules
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString.Lazy as BL
import System.Directory ( removeFile, renameFile ) 

go :: IO ()
go = do
    writeComb "S" redRuleS
    rmAndMv

rmAndMv :: IO ()
rmAndMv = do
    removeFile "../SKeleton.wat"
    renameFile "../skGen.wat" "../SKeleton.wat"

-- takes combinator and generated code for that comb
-- takes code from SKeleton.wat and writes to skGen.wat
writeComb :: String -> [MixedInstr] -> IO ()
writeComb c is = do
    fb <- BL.readFile "../SKeleton.wat"
    let ft = TE.decodeUtf8 fb 
    -- doing it this way because of 
    -- https://hackage-content.haskell.org/package/text-2.1.2/docs/Data-Text-Lazy-IO.html#v:readFile
        untilStart = f1 start ft
        rest       = f2 end   ft
        r = [untilStart, T.pack start, T.pack (emit (toInstr is)), "\n", T.pack end, rest]
    TIO.writeFile "../skGen.wat" $ T.concat r
    where
        start = ";; " ++ c ++ " Combinator Start\n"
        end   = ";; " ++ c ++ " Combinator End\n"
        split' x = T.splitOn (T.pack x)

        f1 x (split' x -> [untilStart, _]) = untilStart
        f1 _ _                             = error "Error while splitting on Start"

        f2 x (split' x -> [_, rest])       = rest
        f2 _ _                             = error "Error while splitting on End"