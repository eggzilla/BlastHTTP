{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

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
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class (liftIO)    
import Control.Monad
import Text.XML.HXT.Core
import Network
import qualified Data.Conduit.List as CL
import Text.Regex.Posix

-- | Parse XML results in XML format
parseXML :: String -> IOStateArrow s b XmlTree              
parseXML = readDocument [ withValidate no
                        , withRemoveWS yes  -- throw away formating WS
                        ] 
parseHTML html = readString [withParseHTML yes, withWarnings no] html          
-- | gets all subtrees with the specified tag name
atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

-- | gets all subtrees with the specified id attribute
atName :: ArrowXml a => String -> a XmlTree XmlTree
atName elementId = deep (isElem >>> hasAttrValue "name" (== elementId))

-- | gets all subtrees with the specified id attribute
atId :: ArrowXml a =>  String -> a XmlTree XmlTree
atId elementId = deep (isElem >>> hasAttrValue "id" (== elementId))

-- | gets the RID
getRID :: ArrowXml a => a XmlTree String  
getRID = atName "RID" >>> 
  proc memeResult -> do
  rid_value <- getAttrValue "value" -< memeResult
  returnA -< rid_value

-- send query and retrieve RID to track status of computation
-- http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=$program&DATABASE=$database&QUERY=" . $encoded_query;
sendQuery program database query = do
  requestXml <- withSocketsDo
    $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ "&QUERY=" ++ query)
  let requestXMLString = (L8.unpack requestXml)
  rid <- liftM head (runX $ parseHTML requestXMLString //> atId "rid" >>> getAttrValue "value")
  return rid

-- retrieve session status
-- "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Get&FORMAT_OBJECT=SearchInfo&RID=$rid"
retrieveSessionStatus rid = do
  statusXml <- withSocketsDo
    $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Get&FORMAT_OBJECT=SearchInfo&RID=" ++ rid)
  let statusXMLString = (L8.unpack statusXml)
  return statusXMLString

-- retrieve result in blastxml format 
-- http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=$rid&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get
retrieveResult rid = do
  statusXml <- withSocketsDo
    $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=" ++ rid ++ "&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get" ++ rid)
  let resultXMLString = (L8.unpack statusXml)
  return resultXMLstring

-- Check if job is completed, if yes retrieve results, otherwise check again or return with an error message in case of failure
checkStatus statusXMLString rid = 
  | statusXMLString =~ "Status=READY" :: Bool = retrieveResult
  | statusXMLString =~ "Status=WAITING" :: Bool = checkStatus (retrieveSessionStatus rid)
  | statusXMLString =~ "Status=FAILED" :: Bool = "Search $rid failed; please report to blast-help\@ncbi.nlm.nih.gov.\n"
  | statusXMLString =~ "Status=UNKNOWN" :: Bool = "Error - Search $rid expired"

-- |
--blastHTTP = do
main :: IO ()
main = do
  let program = "blastn"
  let database = "refseq_genomic"
  let query = "AATATTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGGGGGGGG"

  -- send query and retrive session id                 
  rid <- sendQuery program database query
  -- retrieve session status
  status <- (retrieveSessionStatus rid)
  --check if job is finished and retrieve results 
  result <- checkStatus status rid
  print result
