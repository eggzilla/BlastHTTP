{-# LANGUAGE OverloadedStrings #-}

-- | Blast REST service 
--module Bio.BlastHTTP (
--                       blastHTTP
--                      ) where

module Main where
    
import Network.HTTP.Conduit 
import Data.Conduit
import System.Environment (getArgs)    
import Data.Conduit.Binary (sinkFile)
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)    

-- |
--blastHTTP = do
main :: IO ()
main = do
  let program = "blastn"
  let database = "refseq_genomic"
  let query = "AATATTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGGGGGGGG"
  let rid = "89DY46RJ015"
  --http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=blastn&DATABASE=refseq_genomic&QUERY=AATATTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGGGGGGGG
  runResourceT $ do
         manager <- liftIO $ newManager def
         --send query and retrieve RID to track status of computation
         --http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=$program&DATABASE=$database&QUERY=" . $encoded_query;
         req0 <- liftIO $ parseUrl ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ "&QUERY=" ++ query)
                    
         --wait for computation to finish or fail
         --"http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Get&FORMAT_OBJECT=SearchInfo&RID=$rid"

         --get Result in blastxml format 
         --http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=8752WHW0015&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get
         req <- liftIO $ parseUrl ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=" ++ rid ++ "&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get")
         let req2 = req {
                      method = "POST",
                  --    content_type = "application/x-www-form-urlencoded",
                      redirectCount = 0
--                      checkStatus = \_ _ -> Nothing
                    }
         res0 <- http req0 manager
         --rid_response <- responseBody res0
         responseBody res0 $$+- sinkFile "test.xml"        
         --print rid_response
     --    res2 <- http req2 manager
     --    responseBody res2 $$+- sinkFile "test.xml"
                                                   

                                                            
                                                            
