module Dr.Mario.Protocol.Raw
	( Identifier, identifier, unsafeIdentifier, getIdentifier
	, playerIdentifier, you
	, protocolVersion
	, Button(..)
	, ButtonAction(..)
	, ButtonPress(..)
	) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Dr.Mario.Model

-- | An identifier is any sequence of 40 bytes with no ASCII spaces or
-- newlines.
newtype Identifier = Identifier { _getIdentifier :: ByteString }
	deriving (Eq, Ord, Read, Show)

-- | An 'Identifier' where the bytes for @"you"@ are considered special.
type PlayerIdentifier = Identifier

-- | Returns nothing if the 'ByteString' is too long or has invalid bytes.
identifier :: ByteString -> Maybe Identifier
identifier bs = do
	guard (BS.length bs <= 40)
	guard (10 `BS.notElem` bs)
	guard (32 `BS.notElem` bs)
	return (Identifier bs)

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p v = v <$ guard (p v)

-- | In addition to the checks made by 'identifier', this ensures that its
-- argument is not the special identifier 'you' used by servers to indicate the
-- current client.
playerIdentifier :: ByteString -> Maybe PlayerIdentifier
playerIdentifier = identifier >=> ensure (you/=)

-- | Does not check the length or validity of the contained bytes.
unsafeIdentifier :: ByteString -> Identifier
unsafeIdentifier = Identifier

getIdentifier :: Identifier -> ByteString
getIdentifier = _getIdentifier

-- | Intended for internal use only. Doesn't check that characters are in the
-- single-byte range, doesn't check the length, doesn't check the validity of
-- the bytes produced.
unsafeStringIdentifier :: String -> Identifier
unsafeStringIdentifier = Identifier . BS.pack . map (fromIntegral . fromEnum)

-- | The version of the protocol currently supported by this library.
protocolVersion :: Identifier
protocolVersion = unsafeStringIdentifier "0-statefix-oldfarfix"

-- | The special identifier used by servers to indicate the player associated
-- with the current client.
you :: PlayerIdentifier
you = unsafeStringIdentifier "you"

data Button = L | R | D | A | B deriving (Bounded, Enum, Eq, Ord, Read, Show)
data ButtonAction = Toggle | Close | Open deriving (Bounded, Enum, Eq, Ord, Read, Show)
newtype ButtonPress = ButtonPress { getAtomicButtonPresses :: Map Button ButtonAction } deriving (Eq, Ord, Read, Show)

data ModeState
	= CleanupState
	| ControlState !Word32 !Pill
	deriving (Eq, Ord, Read, Show)

data StateRequestTime
	= AtFrame !Word32
	| Immediately
	| NextCleanupMode
	| NextControlMode
	deriving (Eq, Ord, Read, Show)

-- | Messages that the server can send.
data ServerMessage
	= AcceptControl !Identifier
	| AcceptQueue !Identifier
	| FarControl !Identifier
	| FarState !Word32
	| Frame !Word32
	| Garbage !PlayerIdentifier !(Map Int Color)
	| Loser !PlayerIdentifier
	| ModeCleanup !PlayerIdentifier
	| ModeControl !PlayerIdentifier !PillContent
	| OldControl !Identifier
	| OldState !Word32
	| Pill !PlayerIdentifier !Pill
	| Players !Word32
	| ProposeVersion !Identifier
	| RequestVersion
	| Speed !PlayerIdentifier !Word32
	| State !PlayerIdentifier !Word32 !Pill !Board !ModeState
	| Winner !PlayerIdentifier
	deriving (Eq, Ord, Read, Show)

-- | Messages the client can send.
data ClientMessage
	= Control !Identifier !Word32 [ButtonPress]
	| Debug [(Position, Cell)]
	| Queue !Identifier [ButtonPress]
	| RequestState !StateRequestTime
	| Version !Identifier
