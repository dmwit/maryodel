import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Maybe
import Data.String
import System.Process
import qualified Data.Map as M

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Graphics.Vty
import Graphics.Vty.Attributes

import Dr.Mario.Protocol.Client as Dr.M

main :: IO ()
main = do
	eventChan <- newBChan 100
	(conn, ph) <- initializeConnection def
		{ icLaunch = def
			{ lfRom = "/home/dmwit/.roms/nes/dr_mario.zip"
			, lfServer = "/home/dmwit/programming/maryodel/fceux/dr_mario_server.lua"
			}
		, icConnect = def
			{ cthDeltaCallback = writeBChan eventChan . Right
			, cthDiagnosticCallback = writeBChan eventChan . Left
			}
		}
	gs <- currentGameState conn
	s <- customMain (mkVty def) (Just eventChan) (app conn ph) def { gameState = gs }
	unless (serverHasQuit s) (killServer def { ksHandle = ph })

app :: Connection -> ProcessHandle -> App ProgramState (Either Diagnostic GameDelta) ()
app conn ph = App
	{ appDraw = pure . joinBorders . renderProgramState
	, appChooseCursor = neverShowCursor
	, appHandleEvent = \s e -> case e of
		AppEvent (Right delta) -> case delta of
			Quit -> do
				-- the server's probably dead already, but just in case it
				-- closed its communication pipe without actually quitting,
				-- kick it for being rude to us like that
				liftIO . forkIO $ killServer def { ksHandle = ph }
				continue s { serverHasQuit = True }
			Frame n       -> continue s { lastFrame = Just n }
			PillChanged{} -> do
				gs <- liftIO $ currentGameState conn
				continue $ s
					{ gameState = gs
					, recentPills = delta : take 2 (recentPills s)
					}
			_ -> do
				gs <- liftIO $ currentGameState conn
				continue $ s
					{ gameState = gs
					, recentInterestingMessages = delta : take 4 (recentInterestingMessages s)
					}
		AppEvent (Left err) -> continue s { recentDiagnostics = err : take 6 (recentDiagnostics s) }
		VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
		_ -> continue s
	, appStartEvent = return
	, appAttrMap = \_ -> attrMap def
		[ ( badAttr, withForeColor def red)
		, (goodAttr, withForeColor def green)
		, (colorAttr Red   , withForeColor def red)
		, (colorAttr Yellow, withForeColor def yellow)
		, (colorAttr Blue  , withForeColor def cyan)
		]
	}

renderProgramState :: ProgramState -> Widget n
renderProgramState s = vBox
	[ renderGameState (gameState s)
	, renderServerState (serverHasQuit s)
	, renderFrame (lastFrame s)
	, label "Errors"
	, renderDiagnostics (recentDiagnostics s)
	, label "Deltas"
	, renderDeltas (recentInterestingMessages s)
	, label "Spam"
	, renderDeltas (recentPills s)
	]

renderGameState :: GameState -> Widget n
renderGameState (Setup m) = hBox (renderPlayerStates m)
renderGameState (InProgress _ youS m) = hBox (renderPlayerState you youS : renderPlayerStates m)

renderPlayerStates :: Map PlayerIdentifier PlayerState -> [Widget n]
renderPlayerStates m = [renderPlayerState n s | (n, s) <- M.toAscList m]

renderPlayerState :: PlayerIdentifier -> PlayerState -> Widget n
renderPlayerState n s = vBox . map hCenter $
	[ renderIdentifier n <+> renderDeath (dead s)
	, renderPillContent (pillLookahead s)
	, renderBoard (board s) (mode s)
	, str $ show (dropRate s) ++ " frames per row"
	, str $ case mode s of
		Cleanup -> " " -- reserve some space so the display doesn't jump vertically on mode switches
		Control n _ -> "next forced drop: " ++ show n
	]

bottomLeftCell, otherCell :: PillContent -> Cell
bottomLeftCell pc = Occupied (bottomLeftColor pc) $ case orientation pc of
	Dr.M.Horizontal -> West
	Dr.M.Vertical   -> South

otherCell pc = Occupied (otherColor pc) $ case orientation pc of
	Dr.M.Horizontal -> East
	Dr.M.Vertical   -> North

renderPillContent :: PillContent -> Widget n
renderPillContent pc = case orientation pc of
	Dr.M.Horizontal -> renderCell (bottomLeftCell pc) <+> renderCell (     otherCell pc)
	Dr.M.Vertical   -> renderCell (     otherCell pc) <=> renderCell (bottomLeftCell pc)

renderBoard :: Board -> ModeState -> Widget n
renderBoard b m = border . vBox $
	[ hBox
		[ renderPosition (Position x y)
		| x <- [0..width b-1]
		]
	| y <- [height b-1, height b-2..0]
	]
	where
	(oPos, blPos) = case m of
		Cleanup -> (Nothing, Nothing)
		Control _ p -> (Just (otherPosition p), Just (bottomLeftPosition p))

	renderPosition = renderCell . case m of
		Cleanup -> fromMaybe Empty . get b
		Control _ pill -> \pos -> case () of
			_ | bottomLeftPosition pill == pos -> bottomLeftCell (content pill)
			  |      otherPosition pill == pos ->      otherCell (content pill)
			  | otherwise -> fromMaybe Empty (get b pos)

renderCell :: Cell -> Widget n
renderCell Empty = str "  "
renderCell (Occupied c s) = withAttr (colorAttr c) . str $ case s of
	Virus        -> "☻ "
	Disconnected -> "o "
	North        -> "^ "
	South        -> "v "
	East         -> "> "
	West         -> "< "

renderServerState :: Bool -> Widget n
renderServerState quit = str "Server is " <+> case quit of
	True  -> withAttr  badAttr $ str "finished"
	False -> withAttr goodAttr $ str "connected"

renderFrame :: Maybe Word32 -> Widget n
renderFrame n = str $ "Last game frame was " ++ maybe "not yet announced" show n

renderDiagnostics :: [Diagnostic] -> Widget n
renderDiagnostics = withAttr badAttr . vBox . map (str . show)

renderDeltas :: [GameDelta] -> Widget n
renderDeltas = vBox . map (str . show)

renderIdentifier :: PlayerIdentifier -> Widget n
renderIdentifier = str . tail . init . show . getIdentifier

renderDeath :: Bool -> Widget n
renderDeath False = withAttr goodAttr (str " (play)")
renderDeath True  = withAttr  badAttr (str " (lost)")

label :: String -> Widget n
label s = vLimit 1 $ hBox
	[ hLimit 8 hBorder
	, vBorder
	, str (" " ++ s ++ " ")
	, vBorder
	, hBorder
	]

badAttr, goodAttr :: AttrName
badAttr  = fromString "bad"
goodAttr = fromString "good"

colorAttr :: Dr.M.Color -> AttrName
colorAttr = fromString . show

data ProgramState = ProgramState
	{ gameState :: GameState
	, recentDiagnostics :: [Diagnostic]
	, lastFrame :: Maybe Word32
	, recentPills :: [GameDelta]
	, recentInterestingMessages :: [GameDelta]
	, serverHasQuit :: Bool
	} deriving (Eq, Ord, Read, Show)

instance Default ProgramState where
	def = ProgramState
		{ gameState = def
		, recentDiagnostics = def
		, lastFrame = def
		, recentPills = def
		, recentInterestingMessages = def
		, serverHasQuit = False
		}

instance Default Config where def = mempty
instance Default Attr where def = defAttr
instance Default AttrMap where def = attrMap def def
