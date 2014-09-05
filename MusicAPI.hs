-- ghc -O2 -threaded MusicAPI.hs -package bytestring-0.9.2.1 -package network-2.3.0.13

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module MusicAPI where

import Text.XML.HXT.Core
import Network.HTTP
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.Trans.Maybe
import Network.URI
import Text.Regex.Posix
import Control.Monad.Error
data Music =
	Music { 
		title :: String,
		songId :: String,
		author :: String,
		albumTitle :: String,
		lrclink :: String	
 	}
  deriving (Show, Eq)


atResponse = getChildren >>> hasName "search_common_response" >>> getChildren

atSongList = atResponse >>> hasName "song_list" >>> getChildren

getErrorCode = atResponse >>> deep (isElem >>> hasName "error_code")

getPages = atResponse >>> hasName "pages" >>> getChildren >>> hasName "total" >>> getChildren >>> getText

getElement str = getChildren >>> hasName str >>> getChildren >>> getText

atTag str = deep (isElem >>> hasName str)

getMusicList = atSongList >>> deep (isElem >>> hasName "song") >>>
    proc x -> do
      title'   <- getElement "title"       -< x 
      sid      <- getElement "song_id"     -< x
      author'  <- getElement "author"      -< x
      album    <- getElement "album_title" `orElse` arr (const "") -< x
      lrclink' <- getElement "lrclink" `orElse` arr (const "")     -< x 
      returnA  -< Music {title = title', 
                         songId = sid,
                         author = author',
                         albumTitle = album,
                         lrclink = lrclink'}

parseXML = readString [ withValidate no
                      , withRemoveWS yes
                      ] 

fetchMusicList :: String -> IO (Maybe ([Music],Int))
fetchMusicList url = do
  rsp <- simpleHTTP $ getRequest url
  doc <- getResponseBody rsp
  if null doc 
    then return Nothing
    else do
      let xml = parseXML doc
      err' <- runX (xml >>> getErrorCode)
      if not . null $ err' 
        then return Nothing
        else do
          result <- runX (xml >>> getMusicList) 
          total <- runX (xml >>> getPages) 
          return . Just $ (result, read . head $ total)

makeSearchURL :: Int -> String -> String
makeSearchURL page keywords = 
  "http://tingapi.ting.baidu.com/v2/restserver/ting?method=baidu.ting.search.common&query="
  ++
  urlEncode keywords
  ++
  "&page_size=10&page_no=" ++ show page ++ "&format=xml"

searchMusic :: Int -> String -> MaybeT IO ([Music],Int)
searchMusic page keywords = MaybeT . fetchMusicList $ makeSearchURL page keywords

correctShow = TL.unpack . E.decodeUtf8 . B.pack

parseHTML = readString [ withValidate no
                       , withParseHTML yes
                       , withWarnings no
                       ]

fetchDownloadURL = atTag "html" >>> atTag "body" >>> atTag "div" >>>
                   hasAttrValue "class" (=="download-info clearfix") >>>
                   getChildren >>>
                   hasAttrValue "class" (=="download-wrapper") >>> 
                   getChildren >>>
                   hasAttrValue "class" (=="operation clearfix ") >>>
                   getChildren >>> 
                   hasAttr "data-btndata" >>>
                   hasAttrValue "id" (=="128") >>>
                   getAttrValue "href"

-- needs login we should use cookie
makeDownloadURL :: [String] -> String -> IO String
makeDownloadURL session songID = do
  let url = "http://music.baidu.com/song/" ++ songID
         ++ "/download?__o%2Fsong%2F" ++ songID
  let header = mkHeader HdrCookie $ "BAIDUID=" ++ session !! 1 ++ ":"
          ++"FG=1; Hm_lvt_d0ad46e4afeacf34cd12de4c9b553aa6=1407811580;" 
          ++"u_lo=0; u_id=; u_t=; bdshare_firstime=1405256709630;"
          ++"PRY=1; MCITY=-321%3A360%3A; tracesrc=-1%7C%7Cwww.baidu.com;" 
          ++"BDUSS=" ++ head session ++ ";"  
          ++"BDRCVFR[OyOV3fcLK2t]=9LXhgZOj4kYfjbkPHcsnHmYgv99Udqs;" 
          ++"BDRCVFR[QXr7TCQhxHb]=ddONZc2bo5mfAF9pywdpAqVuNqsus;" 
          ++"H_PS_PSSID=1448_8235_8488_8056_8504_8593_7825_8579_7476_7799_8482_8319_8107_8548_8436"
  let Just uri = parseURI url
  rsp <- simpleHTTP $ Request uri GET [header] []
  doc <- getResponseBody rsp
  --putStrLn . TL.unpack . E.decodeUtf8 . B.pack $ doc
  if null doc 
    then return []
    else do
      let html = parseHTML doc
      liftM head $ runX (html >>> fetchDownloadURL) 

-- simulate baidu login

data LoginError = Unreachable | LoginWrong | OtherError String
instance Error LoginError where
  noMsg = OtherError "An Error"
  strMsg = OtherError 

getCookie :: String -> String -> ErrorT LoginError IO [String]
getCookie name pass = do 
  -- get BAIDUID
  Right rsp <- liftIO $ simpleHTTP $ getRequest "http://www.baidu.com"
  let maybeCookie = lookupHeader HdrSetCookie $ rspHeaders rsp
  case maybeCookie of 
    Nothing -> throwError Unreachable
    Just cookie -> do 
      -- get token
      let apiurl = "http://passport.baidu.com/v2/api/?getapi&class=login&tpl=mn&tangram=true"
      let Just uri = parseURI apiurl
      resp <- liftIO $ simpleHTTP $ Request uri GET [mkHeader HdrCookie cookie,mkHeader HdrUserAgent "Mozilla/5.0 (X11; Linux x86_64; rv:31.0) Gecko/20100101 Firefox/31.0"] ""
      doc <- liftIO $ getResponseBody resp
      let pattern = "bdPass.api.params.login_token='(.)*'"
      let token = take 32 $ drop 31 $ doc =~ pattern :: String
      -- log in
      let baiduMainLoginUrl = "http://passport.baidu.com/v2/api/?login"
      let staticPage = "http://www.baidu.com/cache/user/html/jump.html"
      
      let content = "charset=" ++ urlEncode "utf-8" ++ "&token="
                    ++ urlEncode token ++ "&isPhone="
                    ++ urlEncode "false" ++ "&index="
                    ++ urlEncode "0" ++ "&staticpage="
                    ++ urlEncode staticPage ++ "&loginType="
                    ++ urlEncode "1" ++ "&tpl="
                    ++ urlEncode "mn" ++ "&callback="
                    ++ urlEncode "parent.bdPass.api.login._postCallback"
                    ++ "&verifycode=" 
                    ++ "&username=" ++ urlEncode name
                    ++ "&password=" ++ urlEncode pass
                    ++ "&mem_pass=" ++ urlEncode "on"

      let postHeader = [mkHeader HdrCookie cookie, 
                        mkHeader HdrContentType "application/x-www-form-urlencoded",
                        mkHeader HdrContentLength $ show $ length content]
      let Just loginuri = parseURI baiduMainLoginUrl
      resp <- liftIO $ simpleHTTP $ Request loginuri POST postHeader content
      case resp of
        Left _ -> throwError LoginWrong
        Right r -> do
          let cookies = map (\(Header name str) -> if name == HdrSetCookie then str else "") $ rspHeaders r
          let bduss = takeWhile (/=';') $ drop 6 $
                        concatMap (=~ "BDUSS=(.)*; ") cookies :: String
          let bduid = take 32 $ drop 8 $ 
                        cookie =~ "BAIDUID=([0-9]|[A-Z]|[a-z])*" :: String
     
          if null cookies || null bduss || null bduid
            then throwError LoginWrong
            else return [bduss,bduid]
    
