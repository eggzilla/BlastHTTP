{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

-- | Searches a provided sequence with the NCBI Blast REST service and returns a blast result in xml format as String.
--
-- The function blastHTTP takes the BlastHTTPQuery datatype as argument, which contains following elements: 
--
-- 1. program:  Selects the blast-program to be used for the query. Example values are blastn, blastp, blastx,.. If Nothing is used as argument the function will default to blastn. Type: Maybe String
--
-- 2. database: Selects the database to be queried against. Example values are refseq_genomic, nr, est,.. Please consider that the database must be chosen in accordance with the blastprogram. Default value: refseq_genomic. Type: Maybe String
--
-- 3. querySequence: nucleotides or protein sequence, depending on the blast program used. If no sequence is provided an exception as String will be produced. Type: Maybe SeqData
--
-- 4. entrezQuery: This argument is optional and will filter the result if provided. Type: Maybe String
--
-- and returns Either a BlastResult (Right) on success or an exception as String (Left)
--
-- If you plan to submit more than 20 searches in one session, please look up the Usage Guidelines in the webservice information <http://www.ncbi.nlm.nih.gov/BLAST/developer.shtml>.
module Bio.BlastHTTP ( BlastHTTPQuery (..),
                       blastHTTP) where

import Network.HTTP.Conduit 
import Data.Conduit    
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class (liftIO)    
import qualified Control.Monad as CM
import Bio.BlastXML 
import Text.XML.HXT.Core
import Network
import qualified Data.Conduit.List as CL
import Data.List
import Control.Monad.Error as CM
import Control.Concurrent
import Data.Maybe
import Data.Either
import Bio.Core.Sequence

data BlastHTTPQuery = BlastHTTPQuery 
  { program :: Maybe String
  , database :: Maybe String
  , querySequence :: Maybe SeqData
  , entrezQuery :: Maybe String }
  deriving (Show, Eq)

-- | Parse HTML results into Xml Tree datastructure
parseHTML :: String -> IOStateArrow s0 b0 XmlTree
parseHTML html = readString [withParseHTML yes, withWarnings no] html         
-- | Gets all subtrees with the specified id attribute
atName :: ArrowXml a => String -> a XmlTree XmlTree
atName elementId = deep (isElem >>> hasAttrValue "name" (== elementId))

-- | Gets all subtrees with the specified id attribute
atId :: ArrowXml a =>  String -> a XmlTree XmlTree
atId elementId = deep (isElem >>> hasAttrValue "id" (== elementId))
      
-- | Send query and parse RID from retrieved HTML 
startSession :: String -> String -> String -> Maybe String -> IO String
startSession program database querySequence entrezQuery = do
  requestXml <- withSocketsDo
      $ sendEntrezQuery program database querySequence entrezQuery
  let requestXMLString = (L8.unpack requestXml)
  rid <- CM.liftM head (runX $ parseHTML requestXMLString //> atId "rid" >>> getAttrValue "value")
  return rid

-- | Send query with or without Entrez query and return response HTML
sendEntrezQuery :: String -> String -> String -> Maybe String -> IO L8.ByteString
sendEntrezQuery program database querySequence entrezQuery 
  | isJust entrezQuery = simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ "&QUERY=" ++ querySequence ++ "&ENTREZ_QUERY=" ++ (fromJust entrezQuery))
  | otherwise = simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ "&QUERY=" ++ querySequence)
         
-- | Retrieve session status with RID
retrieveSessionStatus :: String -> IO String 
retrieveSessionStatus rid = do
  statusXml <- withSocketsDo
    $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Get&FORMAT_OBJECT=SearchInfo&RID=" ++ rid)
  let statusXMLString = (L8.unpack statusXml)
  return statusXMLString

-- | Retrieve result in blastxml format with RID 
retrieveResult :: String -> IO (Either String BlastResult)
retrieveResult rid = do
  statusXml <- withSocketsDo
    $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=" ++ rid ++ "&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get")
  resultXML <- (readXMLString statusXml)
  return (Right resultXML)

-- | Check if job results are ready and then retrieves results
checkSessionStatus :: String -> Int -> IO (Either String BlastResult)
checkSessionStatus rid counter = do
    let counter2 = counter + 1
    let counter2string = show counter2
    threadDelay 60000000
    status <- retrieveSessionStatus rid
    results <- waitOrRetrieve status rid counter2
    return results

waitOrRetrieve :: String -> String -> Int -> IO (Either String BlastResult)
waitOrRetrieve status rid counter
  | (isInfixOf "Status=READY" status) = retrieveResult rid
  | (isInfixOf "Status=FAILURE" status) = do
      let exceptionMessage = "Search $rid failed; please report to blast-help at ncbi.nlm.nih.gov.\n"
      return (Left exceptionMessage)
  | (isInfixOf "Status=UNKNOWN" status) = do
      let exceptionMessage = "Search $rid expired.\n"
      return (Left exceptionMessage)
  | otherwise = checkSessionStatus rid counter


-- | Sends Query and retrieves result on reaching READY status, will return exeption message if no query sequence has been provided 
performQuery :: String -> String -> Maybe SeqData -> Maybe String -> Int -> IO (Either String BlastResult)                               
performQuery program database querySequenceMaybe entrezQueryMaybe counter
  | isJust querySequenceMaybe = do 
     rid <- startSession program database (L8.unpack (unSD (fromJust querySequenceMaybe))) entrezQueryMaybe
     result <- checkSessionStatus rid counter
     return result
  | otherwise = do 
     let exceptionMessage = "Error - no query sequence provided"
     return (Left exceptionMessage)

-- | Retrieve Blast results in BlastXML format from the NCBI REST Blast interface
-- The querySequence has to be provided, all other parameters are optional. It is possible to provide an ENTREZ query string
--blastHTTP :: Maybe String -> Maybe String -> Maybe SeqData -> Maybe String -> IO (Either String BlastResult)
blastHTTP :: BlastHTTPQuery -> IO (Either String BlastResult)
blastHTTP (BlastHTTPQuery program database querySequence entrezQuery) = do
  let counter = 1
  let defaultProgram = "blastn"
  let defaultDatabase = "refseq_genomic"                  
  let selectedProgram = fromMaybe defaultProgram program
  let selectedDatabase = fromMaybe defaultDatabase database  
  result <- performQuery selectedProgram selectedDatabase querySequence entrezQuery counter
  return result

      
