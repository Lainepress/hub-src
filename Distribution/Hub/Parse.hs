module Distribution.Hub.Parse
    ( parse
    , Hub(..)
    , HubName
    ) where

import qualified Data.ByteString          as B
import qualified Text.XML.Expat.Annotated as X


type HubName = String

data Hub = HUB {
    handleHUB :: HubName,
    locatnHUB :: FilePath,
    hc_binHUB :: FilePath,
    cb_binHUB :: FilePath,
    hp_binHUB :: Maybe FilePath,
    glb_dbHUB :: FilePath,
    usr_dbHUB :: Maybe FilePath
    }                                                           deriving (Show)

parse :: HubName -> FilePath -> IO Hub
parse hn hf =
     do cts <- B.readFile hf
     	case parse' cts of
     	  YUP t | YUP h <- check hn hf t -> return h
     	  _                              -> ioError $ userError "Hub parse error"

type Loc    = X.XMLParseLocation

type Err    = X.XMLParseError

type Tag    = String

type Node   = X.LNode Tag String

data Poss a = NOPE Err | YUP a
																deriving (Show)
instance Monad Poss where
	(>>=) ps f = undefined ps f -- poss (\_->ei) f ps
	return     = YUP

--poss :: (Err->b) -> (a->b) -> Poss a -> b
--poss n _ (NOPE e) = n e
--poss _ y (YUP  x) = y x

ei2ps :: Either Err a -> Poss a
ei2ps = either NOPE YUP

err :: Loc -> String -> Err
err = flip X.XMLParseError

loc0 :: Loc
loc0 = X.XMLParseLocation 1 0 0 0

parse' :: B.ByteString -> Poss Node
parse' = ei2ps . X.parse' X.defaultParseOptions


check :: HubName -> FilePath -> Node -> Poss Hub
check hn hf (X.Element "hub" [] ns lc) = final $ foldl chk (YUP $ start hn hf lc) ns 
check _  _  _                          = NOPE $ err loc0 "expected simple <hub>...</hub>"

data PSt = ST {
    handlST :: HubName,
    hpathST :: FilePath,	
	locwfST :: Loc,
	hcbinST :: Maybe FilePath,
	cbbinST :: Maybe FilePath,
	hpbinST :: Maybe FilePath,
	glbdbST :: Maybe FilePath,
	usrdbST :: Maybe FilePath
	}															deriving (Show)

start :: HubName -> FilePath -> Loc -> PSt
start hn fp lc = ST hn fp lc Nothing Nothing Nothing Nothing Nothing

final :: Poss PSt -> Poss Hub
final (NOPE er) = NOPE er
final (YUP  st) = 
	 do hc <- get_hc
	    cb <- get_cb
	    gl <- get_gl
	    return $ HUB hn hf hc cb mb_hp gl mb_ur
	  where
		get_hc = maybe (NOPE hc_err) YUP                    mb_hc
		get_cb = maybe (NOPE cb_err) YUP $ maybe mb_hp Just mb_cb
		get_gl = maybe (NOPE gl_err) YUP                    mb_hp

		hc_err = err lc "Hub doesn't specify a GHC bin directory"
		cb_err = err lc "Hub doesn't specify an HP or Cabal bin directory"
		gl_err = err lc "Hub doesn't specify a global package directory"

		ST hn hf lc mb_hc mb_cb mb_hp mb_gl mb_ur = st

chk :: Poss PSt -> Node -> Poss PSt
chk (NOPE er) _  = NOPE er
chk (YUP  st) nd = foldr (trial st nd) (NOPE $ unrecognised st)
		[ chk_wspce
		, chk_hcbin
		, chk_cibin
		, chk_hpbin
		, chk_glbdb
		]

trial :: PSt -> Node -> (PSt -> Node -> Maybe(Poss PSt)) -> Poss PSt -> Poss PSt
trial st nd f ps = maybe ps id $ f st nd

unrecognised :: PSt -> Err
unrecognised = undefined

chk_wspce, chk_hcbin, chk_cibin, chk_hpbin, chk_glbdb :: PSt -> Node -> Maybe(Poss PSt)
chk_wspce = undefined
chk_hcbin = undefined
chk_cibin = undefined
chk_hpbin = undefined
chk_glbdb = undefined





 
 

{-
		case nd of
		  Element 
		chk_top_txt
-}

test :: IO ()
test = 
     do cts <- B.readFile "test.xml"
     	print $ parse' cts

     	
     	
     	