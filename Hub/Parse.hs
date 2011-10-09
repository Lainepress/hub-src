module Hub.Parse
    ( parse
    , dump
    , PSt(..) -- kill warnings
    ) where

import           Char
import           Text.Printf
import qualified Data.ByteString          as B
import qualified Text.XML.Expat.Annotated as X
import           Hub.Hub


hub_bin :: FilePath
hub_bin = "/usr/hs/bin"


parse :: HubName -> FilePath -> IO Hub
parse hn hf =
     do cts <- B.readFile hf
     	case parse' cts of
     	  YUP  tr -> case check hub_bin hn hf tr of
     	               NOPE er -> fail_err hn hf er
     	               YUP  hb -> return hb
     	  NOPE er -> fail_err hn hf er

dump :: Hub -> IO ()
dump hub = B.writeFile path xml_bs
      where
        xml_bs   = B.pack $ map (toEnum.fromEnum) xml
        
        xml      = unlines $
            [        "<hub>" 
            , printf "  <hcbin>%s</hcbin>" hcbin
            , printf "  <cibin>%s</cibin>" cibin
            ] ++
            [ printf "  <hpbin>%s</hpbin>" hpbin | Just hpbin<-[mb_hpbin]
            ] ++
            [ printf "  <glbdb>%s</glbdb>" glbdb
            ] ++
            [ printf "  <usrdb>%s</usrdb>" usrdb | Just usrdb<-[mb_usrdb]
            ] ++
            [        "</hub>"
            ]
                
        path     = path__HUB hub
        hcbin    = hc_binHUB hub
        cibin    = ci_binHUB hub
        mb_hpbin = hp_binHUB hub
        glbdb    = glb_dbHUB hub
        mb_usrdb = usr_dbHUB hub
        


fail_err :: HubName -> FilePath -> Err -> IO a
fail_err _ hf er = ioError $ userError rs
      where
        rs = printf "%s:%d:%d %s" hf ln (cn+1) es
        
        ln = X.xmlLineNumber   lc
        cn = X.xmlColumnNumber lc
        
        X.XMLParseError es lc = er

type Loc    = X.XMLParseLocation

type Err    = X.XMLParseError

type Tag    = String

type Node   = X.LNode Tag String

data Poss a = NOPE Err | YUP a
																deriving (Show)
instance Monad Poss where
	(>>=) ps f = poss NOPE f ps
	return     = YUP

poss :: (Err->b) -> (a->b) -> Poss a -> b
poss n _ (NOPE e) = n e
poss _ y (YUP  x) = y x

ei2ps :: Either Err a -> Poss a
ei2ps = either NOPE YUP

tx_err :: Loc -> String -> Err
tx_err _ = err loc0

err :: Loc -> String -> Err
err = flip X.XMLParseError

loc0 :: Loc
loc0 = X.XMLParseLocation 1 0 0 0

parse' :: B.ByteString -> Poss Node
parse' = ei2ps . X.parse' X.defaultParseOptions

check :: FilePath -> HubName -> FilePath -> Node -> Poss Hub
check c0 hn hf (X.Element "hub" [] ns lc) = 
                                final $ foldl chk (YUP $ start c0 hn hf lc) ns 
          where
            chk (NOPE er) _  = NOPE er
            chk (YUP  st) nd = foldr (trial st nd) (NOPE $ unrecognised st nd)
                    [ chk_wspce
                    , chk_hcbin
                    , chk_cibin
                    , chk_hpbin
                    , chk_glbdb
                    , chk_usrdb
                    ]
check _ _  _  _ = NOPE $ err loc0 "expected simple <hub>...</hub>"

data PSt = ST {
    handlST :: HubName,
    hpathST :: FilePath,	
	locwfST :: Loc,
	hcbinST :: Maybe FilePath,
	cibn0ST :: FilePath,
	cibinST :: Maybe FilePath,
	hpbinST :: Maybe FilePath,
	glbdbST :: Maybe FilePath,
	usrdbST :: Maybe FilePath
	}															deriving (Show)

start :: FilePath -> HubName -> FilePath -> Loc -> PSt
start c0 hn fp lc = ST hn fp lc Nothing c0 Nothing Nothing Nothing Nothing

final :: Poss PSt -> Poss Hub
final (NOPE er) = NOPE er
final (YUP  st) = 
	 do hc <- get_hc
	    cb <- get_cb
	    gl <- get_gl
	    return $ HUB hn hf hc cb mb_hp gl mb_ur
	  where
		get_hc = maybe (NOPE  hc_err) YUP mb_hc
		get_cb = maybe (YUP $ c0    ) YUP mb_cb
		get_gl = maybe (NOPE  gl_err) YUP mb_gl

		hc_err = err lc "Hub doesn't specify a GHC bin directory"
		gl_err = err lc "Hub doesn't specify a global package directory"

		ST hn hf lc mb_hc c0 mb_cb mb_hp mb_gl mb_ur = st

trial :: PSt -> Node -> (PSt -> Node -> Maybe(Poss PSt)) -> Poss PSt -> Poss PSt
trial st nd f ps = maybe ps id $ f st nd

unrecognised :: PSt -> Node -> Err
unrecognised _  (X.Element tg _ _ lc) = err lc $ printf "<%s> not recognised" tg
unrecognised st (X.Text    tx       ) = err lc $ printf "unexpected text: %s" tx
                                                        where
                                                          lc = locwfST st

chk_wspce, chk_hcbin, chk_cibin, chk_hpbin,
                         chk_glbdb, chk_usrdb :: PSt -> Node -> Maybe(Poss PSt)

chk_wspce st nd =
        case nd of
          X.Element _ _ _ _            -> Nothing
          X.Text txt | all isSpace txt -> Just $ YUP    st
                     | otherwise       -> Just $ NOPE $ tx_err lc txt_er
      where
        lc     = locwfST st 
        txt_er = "unexpected top-level text"

chk_hcbin st0 nd = simple_node st0 nd "hcbin" chk
              where
                chk st lc arg =
                        case hcbinST st of
                          Nothing -> YUP (st{hcbinST=Just arg})
                          Just _  -> NOPE $ err lc "<hcbin> respecified"

chk_cibin st0 nd = simple_node st0 nd "cibin" chk
              where
                chk st lc arg =
                        case cibinST st of
                          Nothing -> YUP (st{cibinST=Just arg})
                          Just _  -> NOPE $ err lc "<cibin> re-specified"

chk_hpbin st0 nd = simple_node st0 nd "hpbin" chk
              where
                chk st lc arg =
                        case hpbinST st of
                          Nothing -> YUP (st{hpbinST=Just arg})
                          Just _  -> NOPE $ err lc "<hpbin> respecified"

chk_glbdb st0 nd = simple_node st0 nd "glbdb" chk
              where
                chk st lc arg =
                        case glbdbST st of
                          Nothing -> YUP (st{glbdbST=Just arg})
                          Just _  -> NOPE $ err lc "<glbdb> respecified"

chk_usrdb st0 nd = simple_node st0 nd "usrdb" chk
              where
                chk st lc arg =
                        case usrdbST st of
                          Nothing -> YUP (st{usrdbST=Just arg})
                          Just _  -> NOPE $ err lc "<usrdb> respecified"

simple_node :: PSt -> Node -> Tag -> (PSt->Loc->String->Poss PSt)
                                                        -> Maybe (Poss PSt)
simple_node st (X.Element tg' as ks lc) tg cont
    | tg==tg'   = Just $
                     do chk_as
                        txt <- chk_ks
                        cont (st {locwfST=lc}) lc txt
    | otherwise = Nothing
                      where
                        chk_as = case as of
                                   []  -> return  ()
                                   _:_ -> NOPE $ err lc ats_er
    
                        chk_ks = case [ () | X.Element _ _ _ _<-ks ] of
                                   []  -> chk_nl
                                   _:_ -> NOPE $ err lc txt_er
                                   
                        chk_nl = case all_tx of
                                   _:_ -> chk_ls
                                   []  -> NOPE $ err lc emp_er
    
                        chk_ls = case all (/='\n') all_tx of
                                   True  -> return all_tx
                                   False -> NOPE $ err lc lns_er
    
                        all_tx = trim $ concat $ [ txt | X.Text txt<-ks ]
                        ats_er = printf "<%s> takes no attributes"        tg
                        txt_er = printf "<%s> takes simple text"          tg
                        emp_er = printf "<%s> shouldn't be empty"         tg
                        lns_er = printf "<%s> should be on a single line" tg
simple_node _ (X.Text _) _ _
                = Nothing


trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

{-
test :: IO ()
test = 
     do h <- parse "test" "test.xml"
        print h
-}
