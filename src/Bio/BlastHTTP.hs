{-# LANGUAGE OverloadedStrings #-}

-- | Searches a provided sequence with the NCBI Blast REST service and returns a blast result in xml format as BlastReport.
--
-- The function blastHTTP takes the BlastHTTPQuery datatype as argument, which contains following elements:
--
-- 1. program:  Selects the blast-program to be used for the query. Example values are blastn, blastp, blastx,.. If Nothing is used as argument the function will default to blastn. Type: Maybe String
--
-- 2. database: Selects the database to be queried against. Example values are refseq_genomic, nr, est,.. Please consider that the database must be chosen in accordance with the blastprogram. Default value: refseq_genomic. Type: Maybe String
--
-- 3. querySequences: nucleotides or protein sequences, depending on the blast program used. If no sequence is provided an exception as String will be produced. Type: [Fasta]
--
-- 4. optionalArguments: This argument is optional and will filter the result if provided. Type: Maybe String
--
-- 5. optionalWalltime: Optional walltime in mircroseconds. If specified, will terminate the query after reaching the timelimit and return Left. Type: Maybe Int
--
-- and returns Either a BlastReport (Right) on success or an exception as String (Left)
--
-- If you plan to submit more than 20 searches in one session, please look up the Usage Guidelines in the webservice information <http://www.ncbi.nlm.nih.gov/BLAST/developer.shtml>.
module Bio.BlastHTTP ( BlastHTTPQuery (..),
                       blastHTTP,
                       blastTabularHTTP,
                     ) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B
import qualified Control.Monad as CM
import Text.XML.HXT.Core hiding (trace)
import Network.Socket
import Data.List
import Control.Concurrent
import Data.Maybe
import Network.HTTP.Base
import Biobase.BLAST.Import
import Biobase.BLAST.Types
import Biobase.Fasta.Strict
--import Biobase.Fasta.Export()
import qualified Data.Either.Unwrap as E
import Data.Int
--import Debug.Trace
--import qualified Data.Aeson as DA
import qualified Codec.Archive.Zip as CZ

data BlastHTTPQuery = BlastHTTPQuery
  { provider :: Maybe String
  , program :: Maybe String
  , database :: Maybe String
  , querySequences :: [Fasta () ()]
  , optionalArguments :: Maybe String
  , optionalWalltime :: Maybe Int
  }
  deriving (Show, Eq)

-- | Parse HTML results into Xml Tree datastructure
parseHTML :: String -> IOStateArrow s0 b0 XmlTree
parseHTML = readString [withParseHTML yes, withWarnings no]

-- | Gets all subtrees with the specified id attribute
atId :: ArrowXml a =>  String -> a XmlTree XmlTree
atId elementId = deep (isElem >>> hasAttrValue "id" (== elementId))

-- | Send query and parse RID from retrieved HTML
startSession :: String -> String -> String -> String -> Maybe String -> IO String
startSession provider' program' database' querySequences' optionalArguments'
  | provider' == "ebi" = startSessionEBI program' database' querySequences' optionalArguments'
  | otherwise = startSessionNCBI program' database' querySequences' optionalArguments'

startSessionEBI :: String -> String -> String -> Maybe String -> IO String
startSessionEBI  program' database' querySequences' optionalArguments' = do
  requestXml <- withSocketsDo
      $ sendQueryEBI program' database' querySequences' optionalArguments'
  let requestID = L8.unpack requestXml
  return requestID

startSessionNCBI :: String -> String -> String -> Maybe String -> IO String
startSessionNCBI program' database' querySequences' optionalArguments' = do
  requestXml <- withSocketsDo
      $ sendQueryNCBI program' database' querySequences' optionalArguments'
  let requestXMLString = L8.unpack requestXml
  CM.liftM head (runX $ parseHTML requestXMLString //> atId "rid" >>> getAttrValue "value")

-- | Send query with or without optional arguments and return response HTML
sendQueryEBI :: String -> String -> String -> Maybe String -> IO L8.ByteString
sendQueryEBI program' database' querySequences' _ = do
  putStrLn "Making HTTP request"
  res <- do
    --initReq <- parseUrl "http://postcatcher.in/catchers/541811052cb53502000001a7"
    initReq <- parseUrlThrow "http://www.ebi.ac.uk/Tools/services/rest/ncbiblast/run"
    let req = (flip urlEncodedBody) initReq $
             [ ("email", "florian.eggenhofer@univie.ac.at")
             , ("program", (B.pack program'))
             , ("database", (B.pack database'))
             , ("stype", "dna")
             , ("sequence", (B.pack querySequences'))
             ]
    newManager tlsManagerSettings >>= httpLbs req
        { method = "POST" }
  putStrLn "EBI Response"
  print res
  putStrLn "EBI Response Body"
  print (responseBody res)
  return (responseBody res)

-- | Send query with or without optional arguments and return response HTML
sendQueryNCBI :: String -> String -> String -> Maybe String -> IO L8.ByteString
sendQueryNCBI program' database' querySequences' optionalArguments'
  | isJust optionalArguments' = simpleHttp ("https://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program' ++ "&DATABASE=" ++ database' ++ fromJust optionalArguments' ++ "&QUERY=" ++ querySequences')
  | otherwise = simpleHttp ("https://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Put&PROGRAM=" ++ program' ++ "&DATABASE=" ++ database' ++ "&QUERY=" ++ querySequences')

-- | Retrieve session status with RID
retrieveSessionStatus :: String -> String -> IO String
retrieveSessionStatus provider' rid = do
  if provider' == "ebi"
     then do
       statusXml <- withSocketsDo $ simpleHttp ("http://www.ebi.ac.uk/Tools/services/rest/ncbiblast/status/" ++ rid)
       let statusXMLString = L8.unpack statusXml
       putStrLn "EBI statusXMLString"
       return statusXMLString
     else do
       statusXml <- withSocketsDo $ simpleHttp ("https://www.ncbi.nlm.nih.gov/blast/Blast.cgi?CMD=Get&FORMAT_OBJECT=SearchInfo&RID=" ++ rid)
       let statusXMLString = L8.unpack statusXml
       return statusXMLString

-- | Retrieve result in blast tabular format with RID
retrieveTabularResult :: String -> String -> IO (Either String [BlastTabularResult])
retrieveTabularResult provider' rid = do
  if provider' == "ebi"
     then do
       resultResponse <- withSocketsDo $ simpleHttp ("http://www.ebi.ac.uk/Tools/services/rest/ncbiblast/result/" ++ rid ++ "/Tabular")
       let resultHeaderLessResponse = L8.drop (0  :: Int64) resultResponse
       let resultTabular = parseTabularHTTPBlasts resultHeaderLessResponse
       return (Right resultTabular)
     else do
       resultResponse <- withSocketsDo $ simpleHttp ("https://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=" ++ rid ++ "&FORMAT_TYPE=Tabular&FORMAT_OBJECT=Alignment&CMD=Get")
       let resultHeaderLessResponse = L8.drop (60  :: Int64) resultResponse
       let resultTabular = parseTabularHTTPBlasts resultHeaderLessResponse
       return (Right resultTabular)

-- | Retrieve result in blast tabular format with RID
retrieveJSONResult :: String -> String -> IO (Either String BlastJSON2)
retrieveJSONResult provider' rid = do
  resultResponse <- withSocketsDo $ simpleHttp ("https://www.ncbi.nlm.nih.gov/blast/Blast.cgi?RESULTS_FILE=on&RID=" ++ rid ++ "&FORMAT_TYPE=JSON2&FORMAT_OBJECT=Alignment&CMD=Get")
  let archive = CZ.toArchive resultResponse
  let files = CZ.filesInArchive archive
  let bs = CZ.fromEntry (fromJust (CZ.findEntryByPath (files !! 1) archive))
  let eitherjson = parseJSONBlast bs
  return eitherjson

-- | Check if job results are ready and then retrieves results
--   If a walltime in microseconds was set query retrieval will termiate after it is consumed and return a Left result
checkSessionStatus :: String -> String -> Maybe Int -> Int -> IO (Either String String)
checkSessionStatus provider' rid walltime consumedTime = do
    threadDelay 120000000
    status <- retrieveSessionStatus provider' rid
    if (isNothing walltime)
       then do
         waitOrRetrieve provider' status rid walltime consumedTime
       else do
         if (consumedTime < (fromJust walltime))
           then do
             waitOrRetrieve provider' status rid walltime (consumedTime + 120000000)
           else do
             let exceptionMessage = "BLASTHTTP: Query did not return result within walltime"
             return (Left exceptionMessage)

waitOrRetrieve :: String -> String -> String -> Maybe Int -> Int -> IO (Either String String)
waitOrRetrieve provider' status rid walltime consumedTime
  | provider' == "ebi" = waitOrRetrieveEBI status rid walltime consumedTime
  | otherwise = waitOrRetrieveNCBI status rid walltime consumedTime

waitOrRetrieveEBI :: String -> String -> Maybe Int -> Int -> IO (Either String String)
waitOrRetrieveEBI status rid walltime consumedTime
  | "FINISHED" `isInfixOf` status = return (Right rid)
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
  | otherwise = checkSessionStatus "ebi" rid walltime consumedTime

waitOrRetrieveNCBI :: String -> String -> Maybe Int -> Int -> IO (Either String String)
waitOrRetrieveNCBI status rid walltime consumedTime
  | "Status=READY" `isInfixOf` status = return (Right rid)
  | "Status=FAILURE" `isInfixOf` status = do
      let exceptionMessage = "Search $rid failed; please report to blast-help at ncbi.nlm.nih.gov.\n"
      return (Left exceptionMessage)
  | "Status=UNKNOWN" `isInfixOf` status = do
      let exceptionMessage = "Search $rid expired.\n"
      return (Left exceptionMessage)
  | "Status=WAITING" `isInfixOf` status = do
      checkSessionStatus "ncbi" rid walltime consumedTime
  --Unexpected status, return Left
  | otherwise = do
      let exceptionMessage = "Status has unexpected value " ++ status ++ " - aborting blast search\n"
      return (Left exceptionMessage)

-- | Retrieve Blast results in Blast tabular format from the NCBI REST Blast interface
-- The querySequence has to be provided, all other parameters are optional and can be set to Nothing
-- optionalArguments is attached to the query as is .e.g: "&ALIGNMENTS=250"
blastTabularHTTP :: BlastHTTPQuery -> IO (Either String [BlastTabularResult])
blastTabularHTTP (BlastHTTPQuery provider' program' database' querySequences' optionalArguments' walltime') = do
  let defaultProvider = "ncbi"
  let defaultProgram = "blastn"
  let defaultDatabase = "refseq_genomic"
  let defaultWalltime = Nothing
  let selectedProvider = fromMaybe defaultProvider provider'
  let selectedProgram = fromMaybe defaultProgram program'
  let selectedDatabase = fromMaybe defaultDatabase database'
  let selectedWalltime = maybe defaultWalltime Just walltime'
  --walltime of 1h in microseconds
  --let walltime = Just (7200000000 ::Int)
  performTabularQuery selectedProvider selectedProgram selectedDatabase querySequences' optionalArguments' selectedWalltime

-- | Retrieve Blast results in Blast JSON2 format from the NCBI REST Blast interface
-- The querySequence has to be provided, all other parameters are optional and can be set to Nothing
-- optionalArguments is attached to the query as is .e.g: "&ALIGNMENTS=250"
blastHTTP :: BlastHTTPQuery -> IO (Either String BlastJSON2)
blastHTTP (BlastHTTPQuery provider' program' database' querySequences' optionalArguments' walltime') = do
  let defaultProvider = "ncbi"
  let defaultProgram = "blastn"
  let defaultDatabase = "refseq_genomic"
  let defaultWalltime = Nothing
  let selectedProvider = fromMaybe defaultProvider provider'
  let selectedProgram = fromMaybe defaultProgram program'
  let selectedDatabase = fromMaybe defaultDatabase database'
  let selectedWalltime = maybe defaultWalltime Just walltime'
  --walltime of 1h in microseconds
  --let walltime = Just (7200000000 ::Int)
  performJSONQuery selectedProvider selectedProgram selectedDatabase querySequences' optionalArguments' selectedWalltime

-- | Sends Query and retrieves result on reaching READY status, will return exeption message if no query sequence has been provided
performTabularQuery :: String -> String -> String -> [Fasta () ()] -> Maybe String -> Maybe Int -> IO (Either String [BlastTabularResult])
performTabularQuery provider' program' database' querySequences' optionalArgumentMaybe walltime
  | null querySequences' = do
      let exceptionMessage = "Error - no query sequence provided"
      return (Left exceptionMessage)
  | otherwise = do
     -- TODO do not use @concatMap show@.
     let sequenceString = urlEncode $ concatMap (convertString . fastaToByteString 999999999) querySequences'
     -- (concatMap show querySequences')
     rid <- startSession provider' program' database' sequenceString (Just (maybe "&FORMAT_TYPE=TABULAR" ("&FORMAT_TYPE=TABULAR" ++) optionalArgumentMaybe))
     sessionStatus <- checkSessionStatus provider' rid walltime (0 :: Int)
     if E.isRight sessionStatus
        then retrieveTabularResult provider' rid
        else return (Left (E.fromLeft sessionStatus))


-- | Sends Query and retrieves result on reaching READY status, will return exeption message if no query sequence has been provided
performJSONQuery :: String -> String -> String -> [Fasta () ()] -> Maybe String -> Maybe Int -> IO (Either String BlastJSON2)
performJSONQuery provider' program' database' querySequences' optionalArgumentMaybe walltime
  | null querySequences' = do
      let exceptionMessage = "Error - no query sequence provided"
      return (Left exceptionMessage)
  | otherwise = do
     -- TODO see comment above!
     -- let sequenceString = urlEncode (concatMap show querySequences')
     let sequenceString = urlEncode $ concatMap (convertString . fastaToByteString 999999999) querySequences'
     rid <- startSession provider' program' database' sequenceString (Just (maybe "" ("" ++) optionalArgumentMaybe))
     sessionStatus <- checkSessionStatus provider' rid walltime (0 :: Int)
     --result <- retrieveJSONResult provider' rid
     --return (Right result)
     if E.isRight sessionStatus
        then retrieveJSONResult provider' rid
        else return (Left (E.fromLeft sessionStatus))
