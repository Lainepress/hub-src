module Hub.Parse
    ( parse
    , dump
    , PSt(..) -- kill warnings
    ) where

import           Data.Char
import           Text.Printf
import qualified Data.ByteString          as B
import qualified Text.XML.Expat.Annotated as X
import           Hub.Poss
import           Hub.Oops
import           Hub.Hub
import           Hub.FilePaths



parse :: HubSource -> FilePath -> HubName -> FilePath -> HubKind -> IO Hub
parse hs dy hn hf hk =
     do cts <- B.readFile hf
        case parse' cts of
          YUP  tr -> case check hs dy hn hf hk tr of
                       NOPE er -> fail_err hn hf er
                       YUP  hb -> return hb
          NOPE er -> fail_err hn hf er

dump :: Hub -> IO ()
dump hub = B.writeFile path xml_bs
      where
        xml_bs   = B.pack $ map (toEnum.fromEnum) xml
        
        xml      = unlines $
            [        "<hub>"
            , printf "  <comnt>%s</comnt>" $ string2xml comnt
            , printf "  <hcbin>%s</hcbin>" $ string2xml hcbin
            , printf "  <tlbin>%s</tlbin>" $ string2xml tlbin
            ] ++
            [ printf "  <glbdb>%s</glbdb>" $ string2xml glbdb
            ] ++
            [ printf "  <usrgh>%s</usrdb>" $ string2xml usrgh | Just usrgh<-[mb_usrgh]
            ] ++
            [ printf "  <usrdb>%s</usrdb>" $ string2xml usrdb | Just usrdb<-[mb_usrdb]
            ] ++
            [        "</hub>"
            ]
                
        path     = path__HUB hub
        comnt    = commntHUB hub
        hcbin    = hc_binHUB hub
        tlbin    = tl_binHUB hub
        glbdb    = glb_dbHUB hub
        mb_usrgh = usr_ghHUB hub
        mb_usrdb = usr_dbHUB hub
        


fail_err :: HubName -> FilePath -> Err -> IO a
fail_err _ hf er = oops SysO rs
      where
        rs = printf "%s:%d:%d %s" hf ln (cn+1) es
        
        ln = X.xmlLineNumber   lc
        cn = X.xmlColumnNumber lc
        
        X.XMLParseError es lc = er

type Loc    = X.XMLParseLocation

type Err    = X.XMLParseError

type Tag    = String

type Node   = X.LNode Tag String

tx_err :: Loc -> String -> Err
tx_err _ = err loc0

err :: Loc -> String -> Err
err = flip X.XMLParseError

loc0 :: Loc
loc0 = X.XMLParseLocation 1 0 0 0

parse' :: B.ByteString -> Poss Err Node
parse' = ei2ps . X.parse' X.defaultParseOptions

check :: HubSource -> FilePath -> HubName -> FilePath -> HubKind -> Node -> Poss Err Hub
check hs dy hn hf hk (X.Element "hub" [] ns lc) = 
                            final hs dy hk $ foldl chk (YUP $ start hn hf lc) ns 
          where
            chk (NOPE er) _  = NOPE er
            chk (YUP  st) nd = foldr (trial st nd) (NOPE $ unrecognised st nd)
                    [ chk_wspce
                    , chk_comnt
                    , chk_hcbin
                    , chk_tlbin
                    , chk_glbdb
                    , chk_usrdb
                    , chk_usrgh
                    -- depracated (no warnings yet)
                    , chk_hpbin
                    , chk_cibin
                    ]
check _  _  _  _  _  _ = NOPE $ err loc0 "expected simple <hub>...</hub>"

data PSt = ST {
    handlST :: HubName,
    hpathST :: FilePath,    
    locwfST :: Loc,
    comntST :: Maybe String,
    hcbinST :: Maybe FilePath,
    tlbinST :: Maybe FilePath,
    glbdbST :: Maybe FilePath,
    usrghST :: Maybe FilePath,
    usrdbST :: Maybe FilePath
    }                                                            deriving (Show)

start :: HubName -> FilePath -> Loc -> PSt
start hn fp lc = ST hn fp lc Nothing Nothing Nothing Nothing Nothing Nothing

final :: HubSource -> FilePath -> HubKind -> Poss Err PSt -> Poss Err Hub
final _  _  _  (NOPE er) = NOPE er
final hs dy hk (YUP  st) = 
     do co    <- get_co
        hc    <- get_hc
        tl    <- get_tl
        gl    <- get_gl
        mb_gh <- case mb_ur of
                   Nothing -> return Nothing
                   Just _  -> case mb_gh0 of
                                Nothing -> Just `fmap` calc_gh gl
                                Just gh -> return $ Just gh 
        return $ HUB hs hn hk hf co hc tl gl mb_dy mb_gh mb_ur
      where
        get_co = maybe (YUP   ""      ) YUP mb_co 
        get_hc = maybe (NOPE  hc_err  ) YUP mb_hc
        get_tl = maybe (YUP $ toolsBin) YUP mb_tl
        get_gl = maybe (NOPE  gl_err  ) YUP mb_gl

        hc_err = err lc "Hub doesn't specify a GHC bin directory"
        gl_err = err lc "Hub doesn't specify a global package directory"
        
        mb_dy  = fmap (const dy) mb_ur
        
        ST hn hf lc mb_co mb_hc mb_tl mb_gl mb_gh0 mb_ur = st

        calc_gh gl =
            case match (mk_re globalHubREs) gl of
              Just gh | isHubName gh == Just GlbHK -> return gh 
              _ -> NOPE $ err loc0 "Could not derive global hub from filepath of global package databse"

trial :: PSt -> Node -> (PSt -> Node -> Maybe(Poss Err PSt)) -> Poss Err PSt -> Poss Err PSt
trial st nd f ps = maybe ps id $ f st nd

unrecognised :: PSt -> Node -> Err
unrecognised _  (X.Element tg _ _ lc) = err lc $ printf "<%s> not recognised" tg
unrecognised st (X.Text    tx       ) = err lc $ printf "unexpected text: %s" tx
                                                        where
                                                          lc = locwfST st

chk_comnt, chk_wspce, chk_hcbin, chk_tlbin,
        chk_glbdb, chk_usrgh, chk_usrdb,
        chk_hpbin, chk_cibin :: PSt -> Node -> Maybe(Poss Err PSt)

chk_wspce st nd =
        case nd of
          X.Element _ _ _ _            -> Nothing
          X.Text txt | all isSpace txt -> Just $ YUP    st
                     | otherwise       -> Just $ NOPE $ tx_err lc txt_er
      where
        lc     = locwfST st 
        txt_er = "unexpected top-level text"

chk_comnt st0 nd = simple_node True  st0 nd "comnt" chk
              where
                chk st lc arg =
                        case comntST st of
                          Nothing -> YUP (st{comntST=Just arg})
                          Just _  -> NOPE $ err lc "<comment> respecified"

chk_hcbin st0 nd = simple_node False st0 nd "hcbin" chk
              where
                chk st lc arg =
                        case hcbinST st of
                          Nothing -> YUP (st{hcbinST=Just arg})
                          Just _  -> NOPE $ err lc "<hcbin> respecified"

chk_tlbin st0 nd = simple_node False st0 nd "tlbin" chk
              where
                chk st lc arg =
                        case tlbinST st of
                          Nothing -> YUP (st{tlbinST=Just arg})
                          Just _  -> NOPE $ err lc "<cibin> re-specified"

chk_glbdb st0 nd = simple_node False st0 nd "glbdb" chk
              where
                chk st lc arg =
                        case glbdbST st of
                          Nothing -> YUP (st{glbdbST=Just arg})
                          Just _  -> NOPE $ err lc "<glbdb> respecified"

chk_usrgh st0 nd = simple_node False st0 nd "usrgh" chk
              where
                chk st lc arg =
                        case usrghST st of
                          Nothing -> YUP (st{usrghST=Just arg})
                          Just _  -> NOPE $ err lc "<usrgh> respecified"

chk_usrdb st0 nd = simple_node False st0 nd "usrdb" chk
              where
                chk st lc arg =
                        case usrdbST st of
                          Nothing -> YUP (st{usrdbST=Just arg})
                          Just _  -> NOPE $ err lc "<usrdb> respecified"


-- deprecated (pre-0.3) constructions

chk_hpbin st0 nd = simple_node False st0 nd "hpbin" $ \st _ _ -> YUP st

chk_cibin st0 nd = simple_node False st0 nd "cibin" $ \st _ _ -> YUP st


simple_node :: Bool -> PSt -> Node -> Tag -> (PSt->Loc->String->Poss Err PSt)
                                                        -> Maybe (Poss Err PSt)
simple_node ev st (X.Element tg' as ks lc) tg cont
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
                                   []  | not ev -> NOPE $ err lc emp_er
                                   _            -> chk_ls
    
                        chk_ls = case all (/='\n') all_tx of
                                   True  -> return all_tx
                                   False -> NOPE $ err lc lns_er
    
                        all_tx = trim $ concat $ [ txt | X.Text txt<-ks ]
                        ats_er = printf "<%s> takes no attributes"        tg
                        txt_er = printf "<%s> takes simple text"          tg
                        emp_er = printf "<%s> shouldn't be empty"         tg
                        lns_er = printf "<%s> should be on a single line" tg
simple_node _  _ (X.Text _) _ _
                = Nothing

string2xml :: String -> String
string2xml = concatMap fixChar
    where
      fixChar '<' = "&lt;"
      fixChar '>' = "&gt;"
      fixChar '&' = "&amp;"
      fixChar '"' = "&quot;"
      fixChar c | ord c < 0x80 = [c]
      fixChar c = "&#" ++ show (ord c) ++ ";"
