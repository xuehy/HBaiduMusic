module MainGUI where
import Data.IORef
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk
import Control.Concurrent 
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit
import Network.URI (parseURI)
import System.Process
import System.Posix.IO
import System.IO
import System.Directory
import MusicAPI
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Text.Regex
import Control.Monad.Error
data GUI = GUI {
	mainWin :: Window,
	playButton :: Button,
	pg :: ProgressBar,
	time :: Label,
	titleshow :: [Label],
	authorshow :: [Label],
	albumshow :: [Label],
	inputEntry :: Entry,
	searchButton :: Button,
	mpage :: Label,
	mprev :: Button,
	mnext :: Button,
	mselect :: [Button],
	titlePlay :: Label
}

data LOGIN = LOGIN {
	win :: Window,
	info :: Label,
	button :: Button,
	username :: Entry,
	passwd :: Entry
}

loadLogin :: FilePath -> IO LOGIN
loadLogin gladepath = do
	builder <- builderNew
	builderAddFromFile builder gladepath
	mw <- builderGetObject builder castToWindow "loginWin"
	minfo <- builderGetObject builder castToLabel "loginInfo"
	mb <- builderGetObject builder castToButton "loginButton"
	mname <- builderGetObject builder castToEntry "usernameEntry"
	mpass <- builderGetObject builder castToEntry "passwordEntry"
	return $ LOGIN mw minfo mb mname mpass

-- environment of player
data ENV = ENV {
	login :: IORef Bool,
	passport :: IORef [String],
	status :: IORef Bool,       -- playing or pause 
	totalTime :: IORef Double,  -- current song's total length (in seconds)
	curTime :: IORef Double,    -- current time position : range [0,1]
	step :: IORef Double,       -- progress bar increase step
	timePos :: IORef Int,       -- current time position : in seconds
	totalSongs :: IORef Int,    -- total number of songs in the search result
	pages :: IORef Int,         -- total pages of the result
	currentPage :: IORef Int,   -- current page
	songList :: IORef [Music],  -- song list of current page
	currentSong :: IORef Int    -- id of current song playing in the list
}

initializeENV :: IO ENV
initializeENV = do
	lg <- newIORef False
	pass <- newIORef []
	st <- newIORef True
	tt <- newIORef 0.0
	ct <- newIORef 0.0
	se <- newIORef 0.0
	tp <- newIORef 0
	total <- newIORef 0
	page <- newIORef 0
	curPage <- newIORef 0
	song <- newIORef []
	csong <- newIORef 0
	return $ ENV lg pass st tt ct se tp total page curPage song csong

searchAndShow :: GUI -> IORef ENV -> Int -> String -> IO ()
searchAndShow gui env page keywords = do
	s <- runMaybeT $ searchMusic page keywords
	case s of 
		Nothing -> return ()
		Just (list,total) -> do
			environment <- readIORef env 
			modifyIORef (currentPage environment) (const page)
			when (page == 1) 
				$ do
					modifyIORef (totalSongs environment) (const total)
					let pageCnt = ceiling $ fromIntegral total / 10.0
					modifyIORef (pages environment) (const pageCnt)
			-- current list of result
			modifyIORef (songList environment) (const list)
			labelSetText (mpage gui) $ show page ++ " / " ++ (show . ceiling $ fromIntegral total / 10.0)
			modifyIORef (currentPage environment) (const page)
			zipWithM_ labelSetText (titleshow gui) (map (correctShow . 
						(\x -> subRegex (mkRegex "<em>") x "") . 
						(\x -> subRegex (mkRegex "</em>") x "") . title) list)
			zipWithM_ labelSetText (authorshow gui) (map (correctShow . (\x -> subRegex (mkRegex "<em>") x "") . 
						(\x -> subRegex (mkRegex "</em>") x "") . author) list)
			zipWithM_ labelSetText (albumshow gui) (map (correctShow . (\x -> subRegex (mkRegex "<em>") x "") . 
						(\x -> subRegex (mkRegex "</em>") x "") . albumTitle) list)


loginFunc loginWin env = do
	name <- entryGetText (username loginWin)
	pass <- entryGetText (passwd loginWin)
	eitherSession <- runErrorT $ getCookie name pass
	case eitherSession of
		Left Unreachable -> labelSetText (info loginWin) "Network unreachable"
		Left LoginWrong -> labelSetText (info loginWin) "Wrong Username or password"
		Left _ -> labelSetText (info loginWin) "Internal Error"
		Right s -> do
			environment <- readIORef env
			modifyIORef (login environment) (const True)
			modifyIORef (passport environment) (const s)
			mainQuit

waitForLogIn env = do
	environment	<- readIORef env 
	lg <- readIORef (login environment)
	unless lg $ waitForLogIn env

main :: FilePath -> IO()
main gladepath = do
	_ <- initGUI
	_ <- timeoutAddFull (yield >> return True)
				   priorityDefaultIdle 50	
	env <- initializeENV >>= newIORef 
	login <- loadLogin gladepath
	button login `on` buttonActivated $ liftIO $ loginFunc login env
	widgetShowAll $ win login
	mainGUI
	
	waitForLogIn env
	widgetHideAll $ win login
	gui <- loadGlade gladepath
	_ <- connectGui gui env
	widgetShowAll $ mainWin gui
	mainGUI

loadGlade :: FilePath -> IO GUI
loadGlade gladepath = do
	builder <- builderNew
	builderAddFromFile builder gladepath
	mw <- builderGetObject builder castToWindow "window1"
	mplay <- builderGetObject builder castToButton "button1"
	mpg <- builderGetObject builder castToProgressBar "progressbar1"
	mtime <- builderGetObject builder castToLabel "label1"
	mtitle <- mapM (\x -> builderGetObject builder castToLabel $ 
		           "label" ++ show x)
	               [3..12]
	mauthor <- mapM (\x -> builderGetObject builder castToLabel $ 
		           "label" ++ show x)
	               [13..22]
	malbum <- mapM (\x -> builderGetObject builder castToLabel $ 
		           "label" ++ show x)
	               [23..32]
	mentry <- builderGetObject builder castToEntry "searchFrame"
	msearch <- builderGetObject builder castToButton "button3"
	mpage <- builderGetObject builder castToLabel "label33"
	prev <- builderGetObject builder castToButton "prev"
	next <- builderGetObject builder castToButton "next"
	play <- mapM (\x -> builderGetObject builder castToButton $ 
		           "button" ++ show x)
	               [4..13]
	title <- builderGetObject builder castToLabel "label2"
	return $ GUI mw mplay mpg mtime mtitle mauthor malbum mentry msearch mpage prev next play title

clickPlay :: GUI -> IORef ENV -> Int -> IO ()
clickPlay gui env songID = do
	environment <- readIORef env
	session <- readIORef (passport environment)
	list <- readIORef $ songList environment
	unless (songID >= length list) $ do
		url <- makeDownloadURL session $ songId (list !! songID)

		content <- simpleHttp $ "http://music.baidu.com" ++ url
		unless (B.null content) $ do
			forkIO (void (system "echo \"quit\" > /tmp/music\n")) >> threadDelay 1000000
			B.writeFile "/tmp/tempmusic.mp3" content
			doesFileExist "/tmp/music" >>= 
				\x -> when x $ void (system "unlink /tmp/music") 
			system "mkfifo /tmp/music"

			threadDelay 2000000

			(_,Just hout,_,_) <- createProcess (proc "mplayer" ["-slave","-quiet","-input","file=/tmp/music","/tmp/tempmusic.mp3"]) 
												{std_out = CreatePipe}
			threadDelay 100000
			print "OK"
			environment <- readIORef env 
			system "echo \"get_time_length\" > /tmp/music" 
			y <- forM [1..17] (\x -> hGetLine hout)
			x <- hGetLine hout
		 	modifyIORef (totalTime environment) (const . read $ drop 11 x)
			total <- readIORef $ totalTime environment
			modifyIORef (step environment) (const (1.0/total))
			time <- newIORef 0.0
			z <- forkIO $ void $ updateProgress gui env
			z' <- forkIO $ void $ updateTime gui env
			labelSetText (titlePlay gui) (correctShow . 
							(\x -> subRegex (mkRegex "<em>") x "") . 
							(\x -> subRegex (mkRegex "</em>") x "") . title $ list !! songID  )
			return ()

-- when the search button is clicked
clickSearch :: GUI -> IORef ENV -> IO ()
clickSearch gui env = do
	keywords <- entryGetText (inputEntry gui)
	_ <- timeoutAddFull (yield >> return True)
				   priorityDefaultIdle 100
	unless (null keywords) $ searchAndShow gui env 1 keywords

prevPage :: GUI -> IORef ENV -> IO ()
prevPage gui env = do
	environment <- readIORef env
	curPage <- readIORef (currentPage environment)
	_ <- timeoutAddFull (yield >> return True)
				   priorityDefaultIdle 100
	unless (curPage == 1) $ do
    	 keywords <- entryGetText (inputEntry gui)
    	 modifyIORef (currentPage environment) (\x->x-1)
    	 unless (null keywords) $ searchAndShow gui env (curPage-1) keywords

nextPage :: GUI -> IORef ENV -> IO ()
nextPage gui env = do 
	environment <- readIORef env
	totalPage <- readIORef (pages environment)
	curPage <- readIORef (currentPage environment)
	_ <- timeoutAddFull (yield >> return True)
				   priorityDefaultIdle 100
	when (curPage < totalPage) $ do
    	 keywords <- entryGetText (inputEntry gui)
    	 modifyIORef (currentPage environment) (+1)
    	 unless (null keywords) $ searchAndShow gui env (curPage+1) keywords


connectGui gui env = do
	_ <- onDestroy (mainWin gui) $ forkIO (void (system "echo \"quit\" > /tmp/music\n")) >> mainQuit
	onClicked (playButton gui) (guiPlay gui env)
	searchButton gui `on` buttonActivated $ liftIO $ clickSearch gui env
	mprev gui `on` buttonActivated $ liftIO $ prevPage gui env 
	mnext gui `on` buttonActivated $ liftIO $ nextPage gui env 
	zipWithM_ (\b i -> b `on` buttonActivated $ liftIO $ clickPlay gui env i)
	          (mselect gui) [0..9]
	mapM_ (`labelSetText` "") $ titleshow gui 
	mapM_ (`labelSetText` "") $ authorshow gui 
	mapM_ (`labelSetText` "") $ albumshow gui 


-- update the progress bar 
updateProgress gui env = do
	environment <- readIORef env 
	s <- readIORef $ status environment
	step <- readIORef $ step environment
	if s then 
		readIORef (curTime environment) >>= 
	    \t -> when (t < 1.0) $ progressBarSetFraction (pg gui) t >>
		threadDelay 1000000 >>
		modifyIORef (curTime environment) (+step) >>
		updateProgress gui env
	else threadDelay 1000000 >> updateProgress gui env

-- update the time label
updateTime gui env = do
	environment <- readIORef env 
	s <- readIORef $ status environment
	total <- readIORef $ totalTime environment
	tp <- readIORef $ timePos environment
	let curTimeShow = showTimeI tp
	let totalShow = showTime total
	if s then 
		labelSetText (time gui) (curTimeShow ++ "/" ++ totalShow) >> 
		threadDelay 1000000 >>
		modifyIORef (timePos environment) (+1) >>
		updateTime gui env
	else threadDelay 1000000 >> updateTime gui env

showTimeI :: Int -> String 
showTimeI y = show minutes ++ ":" ++ show seconds
	where
		minutes = y `div` 60
		seconds = y `mod` 60
		
showTime :: Double -> String 
showTime t = show minutes ++ ":" ++ show seconds
	where
		y = floor t
		minutes = y `div` 60
		seconds = y `mod` 60

guiPlay :: GUI -> IORef ENV -> IO ()
guiPlay gui env = do
	environment <- readIORef env
	modifyIORef (status environment) not 
	system "echo \"pause\" > /tmp/music"
	return ()


