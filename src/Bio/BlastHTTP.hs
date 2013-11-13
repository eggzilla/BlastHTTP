{-# LANGUAGE OverloadedStrings #-}

-- | Parse RNAz output
--   For more information on RNAz consult: <http://www.tbi.univie.ac.at/~wash/RN 
--module Bio.BlastHTTP (
--                       blastHTTP
--                      ) where

module Main where
    
import Network.HTTP.Conduit
--import Network.HTTP.Conduit.Types  
import Data.Conduit
import System.Environment (getArgs)    
import Data.Conduit.Binary (sinkFile)
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)    

-- | 
--blastHTTP = do
main :: IO ()
main = do
  let rid = "8752WHW0015"
  runResourceT $ do
         manager <- liftIO $ newManager def
         req <- liftIO $ parseUrl ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=" ++ rid ++ "&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get")
         let req2 = req {
                      method = "POST",
                  --    content_type = "application/x-www-form-urlencoded",
                      redirectCount = 0
--                      checkStatus = \_ _ -> Nothing
                    }
         res2 <- http req2 manager
         responseBody res2 $$+- sinkFile "test.xml"
                                                   
-- Result URL in blastxml format 
-- http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=8752WHW0015&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get
                                                            
                                                            
