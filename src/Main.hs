{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import qualified Codec.Binary.UTF8.String   as UTF8
import           Control.Lens.Combinators
import           Control.Monad.State.Strict
import           Data.Aeson
import qualified Data.ByteString.Lazy       as BS
import           Data.Fixed
import           Data.Time.Clock
import           GHC.Generics
import           System.Console.Haskeline
import           System.Directory           (doesFileExist)
import           System.Exit
import qualified System.IO.Strict           as Strict

data Session = MkSession
  { _breathHolds :: [Pico]
  , _date        :: UTCTime
  } deriving (Show, Generic)

makeLenses ''Session

data RespirareState = MkRespirareState
  { _records         :: [Session]
  , _currentSession  :: Maybe Session
  , _breathHoldStart :: Maybe UTCTime
  } deriving (Show, Generic)

makeLenses ''RespirareState

instance ToJSON Session
instance FromJSON Session

instance ToJSON RespirareState
instance FromJSON RespirareState

getRespirareState :: IO RespirareState
getRespirareState = do
  fileExists <- doesFileExist "session_history.json"
  case fileExists of
    True  -> pure ()
    False -> writeFile "session_history.json" ""
  respirareStateString :: String <- Strict.readFile "session_history.json"
  let mRespirareState :: Maybe RespirareState = decode . BS.pack . UTF8.encode $ respirareStateString
  case mRespirareState of
    Nothing -> do
      putStrLn "Unable to read state from session_history.json file.\nStarting with no session history."
      pure emptyRespirareState
    Just rs -> pure rs

saveStateToFile :: RespirareState -> IO ()
saveStateToFile respirareState = do
  let respirareStateString :: String = UTF8.decode . BS.unpack . encode $ respirareState
  writeFile "session_history.json" respirareStateString

emptyRespirareState = MkRespirareState
  { _records         = []
  , _currentSession  = Nothing
  , _breathHoldStart = Nothing
  }

updateRespirareStateSessionAdd :: UTCTime                              -- ^Session start
                               -> Pico                                 -- ^Time in seconds of breath hold to be added to records
                               -> InputT (StateT RespirareState IO) ()
updateRespirareStateSessionAdd sessionStart t = do
  lift . modify $ \(MkRespirareState records current timeStart) ->
    -- TODO: do this with lenses
    case current of
      Nothing -> MkRespirareState
                   records
                   (Just $ MkSession [t] sessionStart)
                   (Just sessionStart)
      Just (MkSession bhs d) -> MkRespirareState
                                  records
                                  (Just $ MkSession (bhs <> [t]) d)
                                  timeStart

innerLoop :: InputT (StateT RespirareState IO) a
innerLoop = do
  outputStrLn "Take 35 fast deep breaths."
  stopSignal <- waitForAnyKey "Press any key to start breath hold."
  case stopSignal of
    False -> error "TODO: stopSignal error path."
    True -> do
      time <- liftIO getCurrentTime
      c <- getInputChar "Press space to continue to next breath hold cycle, or any other key to finish session."
      case c of
        Just ' ' -> do
          recordCycle time
          innerLoop
        _        -> do
          recordCycle time
          outputStrLn "Session finished"
          loop
 where
  recordCycle time = do
    time' <- liftIO getCurrentTime
    let breathHoldTime = diffUTCTime time' time
        breathHoldTimeSeconds :: Pico = nominalDiffTimeToSeconds breathHoldTime
    outputStrLn $ "Breath hold of: " <> show breathHoldTimeSeconds <> " seconds."
    updateRespirareStateSessionAdd time breathHoldTimeSeconds

loop :: InputT (StateT RespirareState IO) a
loop = do
  minput :: Maybe String <- getInputLine instructions
  case minput of
    Nothing  -> loop
    Just str ->
      case str of
        "0" -> innerLoop
        "1" -> do
          respirareState :: RespirareState <- lift get
          -- TODO: improve this
          outputStrLn . show $ _currentSession respirareState
          loop
        "2" -> error "TODO: do discard current session path"
        "3" -> do
          respirareState :: RespirareState <- lift get
          let respirareStateToBeSaved :: RespirareState -> RespirareState
              respirareStateToBeSaved rs@(MkRespirareState r cs bhs) =
                case cs of
                  Nothing -> rs
                  Just x -> MkRespirareState
                    (r <> [x])
                    Nothing
                    Nothing
          liftIO . saveStateToFile . respirareStateToBeSaved $ respirareState
          lift $ modify respirareStateToBeSaved
          outputStrLn "Respirare\'s state has been saved."
          loop
        "4" -> do
          respirareState :: RespirareState <- lift get
          -- TODO: improve this
          outputStrLn . show $ respirareState
          loop
        "5" -> error "TODO: do Edit session records path"
        "6" -> do
          outputStrLn "Have a nice rest of your day <3"
          liftIO exitSuccess
        _   -> do
          outputStrLn $ exitErrorMessage str
          liftIO exitSuccess

instructions = (unlines $
  [ "Usage:"
  , "0 -> Start a breath hold session"
  , "1 -> View current session"
  , "TODO: 2 -> Discard current session"
  , "3 -> Save current session"
  , "4 -> View all recorded sessions"
  , "TODO: 5 -> Edit session records"
  , "6 -> Exit"
  ])
  <> "% "
exitErrorMessage str = unlines $
  [ "Exiting: Behaviour not defined in respirare\'s usage."
  , "Maybe re-run respirare and follow usage instructions."
  , "Input was: " <> str
  ]


main :: IO (a, RespirareState)
main = do
  respirareStateFromFile <- getRespirareState
  runStateT (runInputT defaultSettings loop) respirareStateFromFile

-- settings :: Settings (StateT Int IO)
-- settings = setComplete comp defaultSettings where
--   comp _ = do
--     n <- get
--     put (n+1)
--     return ("", [Completion (show n) "" True])
