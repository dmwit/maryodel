import Brick
import Brick.BChan
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Vty
import System.Process

import Dr.Mario.Protocol.Client

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
	s <- customMain (mkVty def) (Just eventChan) (app conn ph) def
	unless (serverHasQuit s) (killServer def { ksHandle = ph })

app :: Connection -> ProcessHandle -> App ProgramState (Either Diagnostic GameDelta) ()
app conn ph = App
	{ appDraw = \_ -> [str "TODO"]
	, appChooseCursor = neverShowCursor
	, appHandleEvent = \s e -> case e of
		AppEvent (Right delta) -> case delta of
			Quit -> do
				-- the server's probably dead already, but just in case it
				-- closed its communication pipe without actually quitting,
				-- kick it for being rude to us like that
				liftIO . forkIO $ killServer def { ksHandle = ph }
				continue s { serverHasQuit = True }
			Frame{}       -> continue (boring delta s)
			PillChanged{} -> do
				gs <- liftIO $ currentGameState conn
				continue (boring delta s { gameState = Just gs })
			_ -> do
				gs <- liftIO $ currentGameState conn
				continue (interesting delta s { gameState = Just gs })
		AppEvent (Left err) -> continue s { recentDiagnostics = err : take 4 (recentDiagnostics s) }
		VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
		_ -> continue s
	, appStartEvent = return
	, appAttrMap = def
	}
	where
	boring      delta s = s { recentBoringMessages      = delta : take 2 (recentBoringMessages      s) }
	interesting delta s = s { recentInterestingMessages = delta : take 2 (recentInterestingMessages s) }

data ProgramState = ProgramState
	{ gameState :: Maybe GameState
	, recentDiagnostics :: [Diagnostic]
	, recentBoringMessages :: [GameDelta]
	, recentInterestingMessages :: [GameDelta]
	, serverHasQuit :: Bool
	} deriving (Eq, Ord, Read, Show)

instance Default ProgramState where
	def = ProgramState
		{ gameState = def
		, recentDiagnostics = def
		, recentBoringMessages = def
		, recentInterestingMessages = def
		, serverHasQuit = False
		}

instance Default Config where def = mempty
instance Default Attr where def = mempty
instance Default AttrMap where def = attrMap def def
