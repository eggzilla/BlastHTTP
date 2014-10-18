{-# LANGUAGE OverloadedStrings #-}

-- | Searches a provided sequence with the NCBI Blast REST service and returns a blast result in xml format as BlastResult.
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
import Control.Monad.Except as CM
import Control.Concurrent
import Data.Maybe
import Data.Either
import Bio.Core.Sequence

data BlastHTTPQuery = BlastHTTPQuery 
  { provider :: Maybe String
  , program :: Maybe String
  , database :: Maybe String
  , querySequence :: Maybe SeqData
  , optionalArguments :: Maybe String 
  }
  deriving (Show, Eq)

-- | Parse HTML results into Xml Tree datastructure
parseHTML :: String -> IOStateArrow s0 b0 XmlTree
parseHTML = readString [withParseHTML yes, withWarnings no] 

-- | Gets all subtrees with the specified id attribute
atName :: ArrowXml a => String -> a XmlTree XmlTree
atName elementId = deep (isElem >>> hasAttrValue "name" (== elementId))

-- | Gets all subtrees with the specified id attribute
atId :: ArrowXml a =>  String -> a XmlTree XmlTree
atId elementId = deep (isElem >>> hasAttrValue "id" (== elementId))
      
-- | Send query and parse RID from retrieved HTML 
startSession :: String -> String -> String -> String -> Maybe String -> IO String
startSession provider program database querySequence optionalArguments 
  | provider == "ebi" = startSessionEBI program database querySequence optionalArguments
  | otherwise = startSessionNCBI program database querySequence optionalArguments

startSessionEBI :: String -> String -> String -> Maybe String -> IO String
startSessionEBI  program database querySequence optionalArguments = do
  requestXml <- withSocketsDo
      $ sendQueryEBI program database querySequence optionalArguments
  let requestID = L8.unpack requestXml
  print "EBI - extracted request ID"
  return requestID

startSessionNCBI :: String -> String -> String -> Maybe String -> IO String
startSessionNCBI program database querySequence optionalArguments = do
  requestXml <- withSocketsDo
      $ sendQueryNCBI program database querySequence optionalArguments
  let requestXMLString = L8.unpack requestXml
  CM.liftM head (runX $ parseHTML requestXMLString //> atId "rid" >>> getAttrValue "value")

-- | Send query with or without optional arguments and return response HTML
sendQueryEBI :: String -> String -> String -> Maybe String -> IO L8.ByteString
sendQueryEBI program database querySequence optionalArguments = do
  putStrLn "Making HTTP request"
  res <- do
    --initReq <- parseUrl "http://postcatcher.in/catchers/541811052cb53502000001a7"
    initReq <- parseUrl "http://www.ebi.ac.uk/Tools/services/rest/ncbiblast/run"
    let req = (flip urlEncodedBody) initReq $
             [ ("email", "florian.eggenhofer@univie.ac.at")
             , ("program", "blastn")
             , ("database", "em_rel_mam")
             , ("stype", "dna")
             , ("sequence",  "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
             ]
    withManager $ httpLbs req
        { method = "POST" }
  putStrLn "EBI Response"
  print res
  putStrLn "EBI Response Body"
  print (responseBody res)
  return (responseBody res) 
   
  --print res
  --runResourceT $ do
  --  manager <- liftIO $ newManager conduitManagerSettings
  --  req <- liftIO $ parseUrl "http://www.ebi.ac.uk/Tools/services/rest/ncbiblast/run/"
  --  let req2 = req
  --              { method = "POST"
  --              , redirectCount = 0
  --              , checkStatus = \_ _ _ -> Nothing
  --              }
  --  res <- http req2 manager
  --  responseBody res2 $$+- sinkFile "post-foo.txt"

-- | Send query with or without optional arguments and return response HTML
sendQueryNCBI :: String -> String -> String -> Maybe String -> IO L8.ByteString
sendQueryNCBI program database querySequence optionalArguments
  | isJust optionalArguments = simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ fromJust optionalArguments ++ "&QUERY=" ++ querySequence)
  | otherwise = simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ "&QUERY=" ++ querySequence)
         
-- | Retrieve session status with RID
retrieveSessionStatus :: String -> String -> IO String 
retrieveSessionStatus provider rid = do
  if provider == "ebi"
     then do
       statusXml <- withSocketsDo $ simpleHttp ("http://www.ebi.ac.uk/Tools/services/rest/ncbiblast/status/" ++ rid)
       let statusXMLString = L8.unpack statusXml
       putStrLn "EBI statusXMLString"
       print statusXMLString
       return statusXMLString
     else do
       statusXml <- withSocketsDo $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Get&FORMAT_OBJECT=SearchInfo&RID=" ++ rid)
       let statusXMLString = L8.unpack statusXml
       return statusXMLString
  
-- | Retrieve result in blastxml format with RID 
retrieveResult :: String -> String -> IO (Either String BlastResult)
retrieveResult provider rid = do
  if provider == "ebi"
     then do
       statusXml <- withSocketsDo $ simpleHttp ("http://www.ebi.ac.uk/Tools/services/rest/ncbiblast/result/" ++ rid ++ "/xml")
       resultXML <- parseXML statusXml
       return (Right resultXML)
     else do
       statusXml <- withSocketsDo $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=" ++ rid ++ "&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get")
       resultXML <- parseXML statusXml
       return (Right resultXML)
 
-- | Check if job results are ready and then retrieves results
checkSessionStatus :: String -> String -> IO (Either String BlastResult)
checkSessionStatus provider rid = do
    threadDelay 60000000
    status <- retrieveSessionStatus provider rid
    waitOrRetrieve provider status rid 

waitOrRetrieve :: String -> String -> String -> IO (Either String BlastResult)
waitOrRetrieve provider status rid 
  | provider == "ebi" = waitOrRetrieveEBI status rid 
  | otherwise = waitOrRetrieveNCBI status rid 

waitOrRetrieveEBI :: String -> String -> IO (Either String BlastResult)
waitOrRetrieveEBI status rid 
  | "FINISHED" `isInfixOf` status = retrieveResult "ebi" rid
  | "FAILURE" `isInfixOf` status = do
      let exceptionMessage = "BLASTHTTP: The EBI blast job failed."
      return (Left exceptionMessage)
  | "ERROR" `isInfixOf` status = do
      let exceptionMessage = "BLASTHTTP: An error occurred attempting to get the EBI blast job status."
      return (Left exceptionMessage)
  | "NOT_FOUND" `isInfixOf` status = do
      let exceptionMessage = "BLASTHTTP: The EBI blast job cannot be found."
      return (Left exceptionMessage)
-- RUNNING
  | otherwise = checkSessionStatus "ebi" rid

waitOrRetrieveNCBI :: String -> String -> IO (Either String BlastResult)
waitOrRetrieveNCBI status rid 
  | "Status=READY" `isInfixOf` status = retrieveResult "ncbi" rid
  | "Status=FAILURE" `isInfixOf` status = do
      let exceptionMessage = "Search $rid failed; please report to blast-help at ncbi.nlm.nih.gov.\n"
      return (Left exceptionMessage)
  | "Status=UNKNOWN" `isInfixOf` status = do
      let exceptionMessage = "Search $rid expired.\n"
      return (Left exceptionMessage)
  | otherwise = checkSessionStatus "ncbi" rid

-- | Sends Query and retrieves result on reaching READY status, will return exeption message if no query sequence has been provided 
performQuery :: String -> String -> String -> Maybe SeqData -> Maybe String -> IO (Either String BlastResult)                               
performQuery provider program database querySequenceMaybe optionalArgumentMaybe
  | isJust querySequenceMaybe = do 
     rid <- startSession provider program database (L8.unpack (unSD (fromJust querySequenceMaybe))) optionalArgumentMaybe
     checkSessionStatus provider rid
  | otherwise = do 
     let exceptionMessage = "Error - no query sequence provided"
     return (Left exceptionMessage)

-- | Retrieve Blast results in BlastXML format from the NCBI REST Blast interface
-- The querySequence has to be provided, all other parameters are optional and can be set to Nothing
-- optionalArguments is attached to the query as is .e.g: "&ALIGNMENTS=250"
blastHTTP :: BlastHTTPQuery -> IO (Either String BlastResult)
blastHTTP (BlastHTTPQuery provider program database querySequence optionalArguments) = do
  let defaultProvider = "ncbi"
  let defaultProgram = "blastn"
  let defaultDatabase = "refseq_genomic"   
  let selectedProvider = fromMaybe defaultProvider provider
  let selectedProgram = fromMaybe defaultProgram program
  let selectedDatabase = fromMaybe defaultDatabase database  
  performQuery selectedProvider selectedProgram selectedDatabase querySequence optionalArguments


      
