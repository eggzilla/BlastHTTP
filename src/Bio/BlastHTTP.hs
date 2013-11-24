{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

-- | Blast REST service 
module Bio.BlastHTTP (
                       blastHTTP
                     ) where

import Network.HTTP.Conduit 
import Data.Conduit    
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class (liftIO)    
import qualified Control.Monad as CM
import Text.XML.HXT.Core
import Network
import qualified Data.Conduit.List as CL
import Data.List
import Control.Monad.Error as CM
import Control.Concurrent
import Data.Maybe
import Data.Either

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
startSession program database querySequence entrezQuery = do
  requestXml <- withSocketsDo
--    $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ "&QUERY=" ++ querySequence ++ "&ENTREZ_QUERY=" ++ entrezQuery)
      $ sendEntrezQuery program database querySequence entrezQuery
  let requestXMLString = (L8.unpack requestXml)
  rid <- CM.liftM head (runX $ parseHTML requestXMLString //> atId "rid" >>> getAttrValue "value")
  return rid

--sendEntrezQuery :: String -> String -> String -> Maybe String
sendEntrezQuery program database querySequence entrezQuery 
  | isJust entrezQuery = simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ "&QUERY=" ++ querySequence ++ "&ENTREZ_QUERY=" ++ (fromJust entrezQuery))
  | otherwise = simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program ++ "&DATABASE=" ++ database ++ "&QUERY=" ++ querySequence)
         
-- retrieve session status
retrieveSessionStatus :: String -> IO String 
retrieveSessionStatus rid = do
  statusXml <- withSocketsDo
    $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Get&FORMAT_OBJECT=SearchInfo&RID=" ++ rid)
  let statusXMLString = (L8.unpack statusXml)
  return statusXMLString

-- retrieve result in blastxml format 
retrieveResult :: String -> IO String 
retrieveResult rid = do
  statusXml <- withSocketsDo
    $ simpleHttp ("http://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=" ++ rid ++ "&FORMAT_TYPE=XML&FORMAT_OBJECT=Alignment&CMD=Get")
  let resultXMLString = (L8.unpack statusXml)
  --print "Retrieved result"
  return resultXMLString

-- Check if job is completed, if yes retrieve results, otherwise check again or return with an e rror message in case of failure
checkSessionStatus :: String -> Int -> IO String
checkSessionStatus rid counter = do
    let counter2 = counter + 1
    let counter2string = show counter2
    threadDelay 60000000
    --print ("Check session status" ++ counter2string)
    status <- retrieveSessionStatus rid
    let readyString = "Status=READY"
    let failureString = "Status=FAILURE"
    let expiredString = "Status=UNKNOWN"
    --CM.when (isInfixOf failureString status)(throwError "Search $rid failed; please report to blast-help at ncbi.nlm.nih.gov.\n")
    --CM.when (isInfixOf expiredString status)(throwError "Search $rid expired.\n")
    results <- waitOrRetrieve (isInfixOf readyString status) rid counter2
    return results

waitOrRetrieve :: Bool -> String -> Int -> IO String
waitOrRetrieve ready rid counter
  | ready  = retrieveResult rid
  | otherwise = checkSessionStatus rid counter
                               
checkQuerySequencePresence program database querySequenceMaybe entrezQueryMaybe counter
  | isJust querySequenceMaybe = performQuery program database (fromJust querySequenceMaybe) entrezQueryMaybe counter
  | otherwise = do 
     let exceptionMessage = "Error - no query sequence provided"
     return (Left exceptionMessage)

performQuery program database querySequenceMaybe entrezQueryMaybe counter = do
  rid <- startSession program database querySequenceMaybe entrezQueryMaybe
  result <- checkSessionStatus rid counter
  return (Right result)

blastHTTP :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO (Either String String)
blastHTTP programMaybe databaseMaybe querySequenceMaybe entrezQueryMaybe = do
  let counter = 1
  let defaultProgram = "blastn"
  let defaultDatabase = "refseq_genomic"                  
  let program = fromMaybe defaultProgram programMaybe
  let database = fromMaybe defaultDatabase databaseMaybe  
  result <- checkQuerySequencePresence program database querySequenceMaybe entrezQueryMaybe counter
  -- send query and retrieve session id                 
  --rid <- startSession program database querySequenceMaybe entrezQueryMaybe
  --check if job is finished and retrieve results 
  --result <- checkSessionStatus rid counter
  return result

      
