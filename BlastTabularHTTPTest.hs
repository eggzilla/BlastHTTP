-- | BlastHTTP test script
--runghc -package-db --ghc-arg=.cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d/ BlastTabularHTTPTest.hs aeromonas.fa

module Main where
    
import System.Environment (getArgs)
import System.Process 
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment
import Data.List
import System.Directory
import System.Process
import Control.Monad    
import Data.Either.Unwrap
import Bio.BlastHTTP
import Biobase.Fasta.Streaming
import Biobase.Fasta.Types
import qualified Data.ByteString.Lazy.Char8 as L8
    
main = do
  args <- getArgs
  let input_file = (head args)
  putStrLn "Test:"
  inputFasta <- parseFastaFile input_file
  let blastQuery = BlastHTTPQuery (Just "ncbi") (Just "blastn") (Just "nt") inputFasta Nothing (Just (7200000000 ::Int))
  blastOutput <- blastTabularHTTP blastQuery
  if isRight blastOutput
    then print (fromRight blastOutput)
    else  print (fromLeft blastOutput)
