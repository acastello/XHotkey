module XHotkey.Window 
-- typesym for the function used in win_evmap callbacks
    ( EventCB
-- X resources used for windows
    , WinRes, res_bordersize, res_bordercolor, res_bgcolor, res_fgcolor, res_font
-- concurrently-accesible window structures and routines
    , WinEnv, win_dpy, win_id, win_attr, win_gc, win_strbounds, win_putstr
    , win_evmap
-- ReaderT typesym around WinEnv
    , XWin
-- closeable MonadIO evaluation channel, and corresponding XWin typesym
    , MChan, newMChan, closeMChan, evalMChan, runMChan
    , WChan, newWChan, closeWChan, evalWChan, runWChan
-- miriad of functions
    , putStrTL, setEventCB, win, msgbox, parWin, parMsgbox, writeMsg
    ) where


import XHotkey.Types
import XHotkey.Core

import Graphics.X11
import Graphics.X11.Xft
import Graphics.X11.Xrender
import Graphics.X11.Xlib.Extras

import Control.Concurrent
import Control.Monad.Reader

import Data.Bits
import Data.Char (isSpace)
import Data.IORef
import Data.List (inits)

import Foreign
import Foreign.C

import qualified Data.Map as M

type EventCB = Event -> XWin ()

data WinEnv = WinEnv
    { win_dpy       :: Display
    , win_id        :: Window
    , win_attr      :: Ptr SetWindowAttributes
    , win_gc        :: GC
    , win_strbounds :: String -> XWin (Dimension, Dimension, Dimension)
    , win_putstr    :: Position -> Position -> String -> XWin ()
    , win_evmap     :: IORef (M.Map EventType EventCB)
    }

data WinRes = WinRes
    { res_bordersize    :: CInt
    , res_bordercolor   :: Pixel
    , res_bgcolor       :: Pixel
    , res_fgcolor       :: Pixel
    , res_font          :: String
    }

type XWin = ReaderT WinEnv IO

putStrTL :: Position -> Position -> String -> XWin ()
putStrTL x y str = do
    wenv <- ask
    (asc, _, _) <- (win_strbounds wenv) str
    (win_putstr wenv) x (y- (fromIntegral asc)) str

data TextJustify = LeftJ | RightJ | Centered | Filled

strBounds :: String -> XWin (Dimension, Dimension, Dimension)
strBounds "" = ask >>= ($ "") . win_strbounds
strBounds str = do
    WinEnv { win_strbounds = strb } <- ask
    let valid = lines str
        n = fromIntegral $ length valid
    width <- maximum . (fmap (\(_,_,t) -> t)) <$> traverse strb valid
    (asc, des, _) <- strb ""
    return (asc, n*des + (n-1)*asc, width)

putStrJ :: TextJustify -> Position -> Position -> String -> XWin ()
putStrJ LeftJ x y str = do
    WinEnv { win_putstr = pstr, win_strbounds = strb } <- ask
    (asc, des, _) <- strb ""
    foldMapN (\s' n -> 
        pstr x (y+n*(fromIntegral $ asc+des)) s') (lines str)
putStrJ RightJ x y str = do
    WinEnv { win_putstr = pstr, win_strbounds = strb } <- ask
    (asc, des, _) <- strb ""
    let lns = lines str
        hei = fromIntegral $ asc + des
    (_,_, mw) <- strBounds str
    foldMapN (\s' n -> do
        (_,_,w) <- strb s'
        pstr (x+(fromIntegral $ mw-w)) (y+n*hei) s') lns
putStrJ Centered x y str = do
    WinEnv { win_putstr = pstr, win_strbounds = strb } <- ask
    (asc, des, _) <- strb ""
    let lns = lines str
        hei = fromIntegral $ asc + des
    (_,_, mw) <- strBounds str
    foldMapN (\s' n -> do
        (_,_,w) <- strb s'
        pstr (x+((fromIntegral $ mw-w)) `quot` 2) (y+n*hei) s') lns
putStrJ Filled x y str = do
    WinEnv { win_putstr = pstr, win_strbounds = strb } <- ask
    (asc, des, _) <- strb ""
    let lns = lines str
        hei = fromIntegral $ asc + des
    (_,_, mw) <- strBounds str
    flip foldMapN lns $ \s' n -> do
        let wds = words' s'
        szs <- return . ((\(_,_,t) -> t) <$>) =<< traverse strb wds
        let spcs = (spacing (mw-(sum szs)) (fromIntegral $ length szs -1))
            poss = fromIntegral <$> sum <$> (inits $ zipWith (+) szs spcs)
        sequence_ $ zipWith (\pos s'' -> do
            pstr (x+pos) (y+n*hei) s''
            ) poss wds

setEventCB :: EventType -> EventCB -> XWin ()
setEventCB typ fun = do
    WinEnv { win_evmap = evmap } <- ask
    io $ modifyIORef' evmap (M.insert typ fun)

hideWin :: EventCB
hideWin _ = do
    WinEnv { win_dpy = dpy, win_id = wid } <- ask
    io $ unmapWindow dpy wid
    io $ flush dpy 

type WChan = MChan XWin 

newWChan :: MonadIO m => m (WChan a)
newWChan = newMChan

closeWChan :: MonadIO m => WChan a -> m ()
closeWChan = closeMChan

evalWChan :: MonadIO m => WChan a -> XWin a -> m a
evalWChan = evalMChan

runWChan :: MonadIO m => WChan a -> (XWin a -> m a) -> m Bool
runWChan = runMChan

win :: WinRes -> XWin a -> X a
win (WinRes bordersz bordercol bgcolor fgcolor fontn) act = (io initThreads >>) $ copyX $ do
    XEnv { xdisplay = dpy, xroot = root } <- ask
    let screenn = defaultScreen dpy
        visual = defaultVisual dpy screenn
        colormap = defaultColormap dpy screenn
    io $ 
        withXftColorValue dpy visual colormap xrc $ \xftcol ->
        allocaSetWindowAttributes $ \attr -> do

    set_background_pixel attr bgcolor
    set_override_redirect attr True
    set_border_pixel attr bordercol

    window <- createWindow dpy root 10 10 10 10 bordersz copyFromParent inputOutput 
        (defaultVisual dpy (defaultScreen dpy)) 
        (cWBorderPixel .|. cWBackPixel .|. cWOverrideRedirect) attr
    gc <- createGC dpy window
    setForeground dpy gc fgcolor

    selectInput dpy window (exposureMask .|. structureNotifyMask .|. buttonPressMask)

    xftdraw <- xftDrawCreate dpy window visual colormap
    xftfont <- xftFontOpen dpy (screenOfDisplay dpy screenn) fontn
    let df x y str = io $ xftDrawString xftdraw xftcol xftfont x y str
        strb str = io $ do
            asc <- fromIntegral <$> xftfont_ascent xftfont
            desc <- fromIntegral <$> xftfont_descent xftfont
            width <- fromIntegral <$> xglyphinfo_width <$> 
                xftTextExtents dpy xftfont (str)
            return (asc, desc, width)
    -- xftTextExtents dpy xftfont "_" >>= \gi -> print (xglyphinfo_width gi, 
        -- xglyphinfo_height gi, xglyphinfo_x gi, xglyphinfo_y gi, xglyphinfo_xOff gi,
        -- xglyphinfo_yOff gi)

    ev_f <- newIORef mempty
    let env = WinEnv dpy window attr gc strb df ev_f 
    mv <- newEmptyMVar :: IO (MVar ())
    mapWindow dpy window
    flush dpy
    proc <- io $ forkIO $ allocaXEvent $ \ev -> flip runReaderT env $ do
        dowhile $ do
            io $ nextEvent dpy ev
            ev' <- io $ getEvent ev
            f <- io $ readIORef ev_f
            maybe (return ()) ($ ev') $ M.lookup (ev_event_type ev') f 
            return (ev_event_type ev' /= destroyNotify)
        io $ putMVar mv ()

    ret <- runReaderT act env 
    io $ destroyWindow dpy window
    flush dpy
    io $ takeMVar mv
    return ret
    where 
        xrc = XRenderColor (fromIntegral $ fgcolor `shiftR` 16 .&. 0xff * 0x101) 
            (fromIntegral $ fgcolor `shiftR` 8 .&. 0xff * 0x101) 
            (fromIntegral $ fgcolor .&. 0xff * 0x101) 0xffff

parWin :: WinRes -> X (WChan a)
parWin res = do
    x <- newWChan
    forkX_ $ win res $ dowhile $ runWChan x id
    return x

msgbox :: String -> X ()
msgbox str = win (WinRes 2 0xbabdb6 0x222222 0xbabdb6 "Inconsolata: bold: pixelsize=15px") $ do
    WinEnv { win_dpy = dpy, win_id = w', win_gc = gc, win_attr = attr, win_strbounds = strb, win_putstr = drf } <- ask
    (asc,des,wdt) <- strb str
    (_,_, hpad) <- strb "_"
    let vpad = des
    when (wdt+2*hpad > 1) $ io $ do
        resizeWindow dpy w' (wdt+2*hpad) (2*vpad+asc+des)
    drf (fromIntegral hpad) (fromIntegral $ asc + vpad) str
    io $ flush dpy
    io $ getLine
    return ()

parMsgbox :: WinRes -> X (WChan ())
parMsgbox res = do
    xc <- parWin res
    evalWChan xc $ do
        WinEnv { win_dpy = dpy, win_id = wid } <- ask
        setEventCB buttonPress hideWin
        io $ unmapWindow dpy wid
        io $ flush dpy
    return xc

writeMsg :: MonadIO m => WChan () -> String -> m ()
writeMsg xc "" = do
    evalWChan xc $ do
        WinEnv { win_dpy = dpy, win_id = wid } <- ask
        io $ do
            unmapWindow dpy wid
            flush dpy
    return () 
writeMsg xc str = do
    evalWChan xc $ do
        WinEnv { win_dpy = dpy, win_putstr = putstr, win_strbounds = strext, win_id = wid } <- ask
        (asc, des, width) <- strBounds str
        io $ do
            clearWindow dpy wid
            mapWindow dpy wid
            resizeWindow dpy wid (width+2) (asc + des + 2)
        putStrJ Filled 1 (1 + fromIntegral asc) str
        io $ flush dpy

-- various utility functions
foldMapN :: (Foldable t, Monad m, Num n) => (a -> n -> m ()) -> t a -> m ()
foldMapN f xs = foldl step (return 0) xs >> return ()
    where
        step m a = do
            n <- m
            f a n
            return (n+1)

dowhile :: Monad m => m Bool -> m ()
dowhile act = do
    rep <- act
    if rep then
        dowhile act
    else
        return ()

printsz :: String -> XWin ()
printsz str = do
    WinEnv { win_strbounds = strb } <- ask
    io . print =<< strb str

spacing :: Integral a => a -> a -> [a]
spacing n m = if n > m then
        let q = n `quot` m
            r = n-q
        in q:spacing r (m-1)
    else if n == 0 then
        []
    else
        [n]
    
words' :: String -> [String]
words' "" = []
words' str = 
    let (pre,pos) = break isSpace str
        res = uncurry frst $ words' <$> drop' pos
    in if null pre then
        res
    else
        pre:res
    where
        drop' "" = ("","")
        drop' (c:str) =
            if isSpace c then
                let (sp, rem) = drop' str
                in (c:sp, rem)
            else
                ("", c:str)
        frst s' (s:xs) = (s'++s):xs
        frst "" xs = xs
        frst s [] = [s]
