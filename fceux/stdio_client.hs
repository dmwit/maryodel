import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Data.Monoid
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

knownVersions = BS.pack <$> ["0", "0-statefix"]

guessPipes = do
	fps <- listDirectory "/tmp"
	case filter ("tmp." `isPrefixOf`) fps of
		[] -> threadDelay 1000000 >> guessPipes
		[dir] -> awaitPipes ("/tmp" </> dir)
		_ -> die "Multiple candidates for server connection pipe. Dunno what to do."

awaitPipes dir = do
	fps <- listDirectory dir
	case fps of
		[] -> loop
		["s2c"] -> loop
		["c2s"] -> loop
		["s2c", "c2s"] -> return dir
		["c2s", "s2c"] -> return dir
		_ -> die "Uh-oh. Just one possible server connection directory, but it doesn't have the right pipes."
	where
	loop = threadDelay 1000000 >> awaitPipes dir

setVersion hs2c hc2s = findVersion where
	requestVersion = BS.pack "request-version"

	findVersion = do
		message <- BS.hGetLine hs2c
		when (message == requestVersion)
		     (die "Didn't see any versions we know. Forget it.")
		let version = BS.drop (length "propose-version ") message
		if version `elem` knownVersions
		then do
			BS.hPutStrLn hc2s (BS.pack "version " <> version)
			hFlush hc2s
			clearVersions
		else findVersion

	clearVersions = do
		message <- BS.hGetLine hs2c
		when (message /= requestVersion) clearVersions

openWriteEnd dir 0 = die $ "Couldn't open " <> dir </> "c2s for writing. Is the server listening on that pipe?"
openWriteEnd dir n = catch
	(openFile (dir </> "c2s") WriteMode)
	(\e -> if isDoesNotExistError e
	       then threadDelay 1000 >> openWriteEnd dir (n-1)
	       else throwIO e
	)

main = do
	dir <- guessPipes
	hs2c <- openFile (dir </> "s2c") ReadMode
	-- Opening pipes for writing is a bit finicky. Gotta wait until there's
	-- somebody reading it. We'll give them one second to show up. It ain't the
	-- only race condition in this client, that's for sure.
	hc2s <- openWriteEnd dir 10

	hSetBuffering stdin  NoBuffering
	hSetBuffering stdout NoBuffering
	setVersion hs2c hc2s
	forkIO (LBS.hGetContents hs2c >>= LBS.putStr)
	LBS.getContents >>= LBS.hPutStr hc2s
