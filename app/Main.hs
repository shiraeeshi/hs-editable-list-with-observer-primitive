module Main where

import Control.Monad (when, forM_)
import Control.Exception (try)
import System.IO (stdin, stdout, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import ViewUtils (clearScreen, showInRectangle, clearRectangle, showInGrid, drawGrid, highlightCell, printFromBottom)

data RowData = Row { smth :: String } deriving Eq

initialRows = [
  Row "something a"
  , Row "something b"
  , Row "something c"
  , Row "something d"
  , Row "something e"
  ]

data AppStateData = AppState
  { rows :: [RowData]
  , activeCellY :: Maybe Int
  , debugMessages :: [String]
  , listeners :: AppStateListenersData
  }

data AppStateListenersData = AppStateListeners
  { rowsListeners :: [[RowData] -> IO ()]
  , activeCellYListeners :: [Maybe Int -> IO ()]
  , debugMessagesListeners :: [[String] -> IO ()]
  }

addRowsListener :: ([RowData] -> IO ()) -> AppStateListenersData -> AppStateListenersData
addRowsListener listener (AppStateListeners rowsListeners _activeCellYListeners _debugMessagesListeners) =
  AppStateListeners (listener:rowsListeners) _activeCellYListeners _debugMessagesListeners

addActiveCellYListener :: (Maybe Int -> IO ()) -> AppStateListenersData -> AppStateListenersData
addActiveCellYListener listener (AppStateListeners _rowsListeners activeCellYListeners _debugMessagesListeners) =
  AppStateListeners _rowsListeners (listener:activeCellYListeners) _debugMessagesListeners

addDebugMessagesListener :: ([String] -> IO ()) -> AppStateListenersData -> AppStateListenersData
addDebugMessagesListener listener (AppStateListeners _rowsListeners _activeCellYListeners debugMessagesListeners) =
  AppStateListeners _rowsListeners _activeCellYListeners (listener:debugMessagesListeners)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  let appState = AppState initialRows Nothing [] initListeners
  clearScreen
  forM_ (rowsListeners (listeners appState)) $ \listener -> listener (rows appState)
  loop appState
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows
    debugLinesCount = 20

    initListeners :: AppStateListenersData
    -- initListeners =
    --     addRowsListener mainRowsListener
    --     (addActiveCellYListener activeCellYListener
    --     (addDebugMessagesListener debugMessagesListener
    --     (empty)))
    initListeners =
        addRowsListener mainRowsListener
        $ addActiveCellYListener activeCellYListener
        $ addDebugMessagesListener debugMessagesListener
        $ empty
      where
        empty = AppStateListeners [] [] []

    mainRowsListener :: [RowData] -> IO ()
    mainRowsListener rows = do
      showInGrid
        xUpperLeft
        yUpperLeft
        columnCount
        columnWidth
        Nothing
        (map (\row -> [smth row]) rows)

    activeCellYListener :: Maybe Int -> IO ()
    activeCellYListener activeCellY = do
      let activeCellCoords = fmap (\y -> (0, y)) activeCellY
      drawGrid xUpperLeft yUpperLeft columnWidth columnCount rowCount
      case activeCellCoords of
        Nothing -> return ()
        Just coordsPair -> highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount coordsPair

    debugMessagesListener :: [String] -> IO ()
    debugMessagesListener debugMessages = do
      printFromBottom
        xUpperLeft
        (yUpperLeft+12+debugLinesCount)
        debugMessages

    loop :: AppStateData -> IO ()
    loop appState@(AppState rows activeCellY debugMessages listenersHolder) = do
      key <- getKey
      when (key /= "\ESC") $ do
        case key of
          "\ESC[A" -> do -- up
            let newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ max 0 (y-1)
                    Nothing -> Just 0
                debugRow = "up, " ++ show(newActiveCellY)
                newDebugRows = take debugLinesCount (debugRow:debugMessages)
            forM_ (activeCellYListeners listenersHolder) $ \listener -> listener newActiveCellY
            forM_ (debugMessagesListeners listenersHolder) $ \listener -> listener newDebugRows
            loop (AppState rows newActiveCellY newDebugRows listenersHolder)
          "\ESC[B" -> do -- down
            let newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ min (rowCount-1) (y+1)
                    Nothing -> Just 0
                debugRow = "down, " ++ show(newActiveCellY)
                newDebugRows = take debugLinesCount (debugRow:debugMessages)
            forM_ (activeCellYListeners listenersHolder) $ \listener -> listener newActiveCellY
            forM_ (debugMessagesListeners listenersHolder) $ \listener -> listener newDebugRows
            loop (AppState rows newActiveCellY newDebugRows listenersHolder)
          "\n" -> do -- enter
            let eitherValue =
                  case activeCellY of
                    Nothing -> Left "there's no selected cell"
                    Just cellIndex -> do
                      if cellIndex < 0 || cellIndex >= (length rows)
                        then Left $ "index out of bounds: " ++ (show cellIndex)
                        else Right $ smth $ rows !! cellIndex
            let 

              showEditField value = do
                let
                  txt = "edit cell value:"
                  lentxt = length txt
                  yPos = 0
                  xPos = (columnCount * (columnWidth + 1)) + 3
                  replaceNth lst idx val = if idx < 1 then val:(tail lst) else (head lst) : (replaceNth (tail lst) (idx - 1) val)
                showInRectangle xPos yPos lentxt [txt, value]
                key <- getKey
                case key of
                  "\n" -> do
                    case activeCellY of
                      Nothing -> return ()
                      Just cellIndex -> do
                        clearRectangle xPos yPos lentxt 2
                        let
                          newRows = replaceNth rows cellIndex (Row value)
                          msg = "updated rows"
                          newDebugRows = take debugLinesCount (msg:debugMessages)
                        forM_ (rowsListeners listenersHolder) $ \listener -> listener newRows
                        forM_ (debugMessagesListeners listenersHolder) $ \listener -> listener newDebugRows
                        loop (AppState newRows Nothing newDebugRows listenersHolder)
                  "\DEL" -> showEditField (if (length value) == 0 then value else init value)
                  c -> showEditField (value ++ c)
            case eitherValue of
              Left e -> do
                let msg = "error: " ++ (show e)
                let newDebugRows = take debugLinesCount (msg:debugMessages)
                forM_ (debugMessagesListeners listenersHolder) $ \listener -> listener newDebugRows
                loop (AppState rows activeCellY newDebugRows listenersHolder)
              Right v -> do
                showEditField v
          "q" -> return ()
          _ -> return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
