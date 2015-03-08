import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Layout.Tabbed
import qualified XMonad.Util.EZConfig as EZ
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.IO (hPutStrLn)
import System.Environment (getEnvironment, getEnv)
import DesktopLayouts
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ ewmh defaultConfig
        { modMask            = mod4Mask
	, logHook            = dynamicLogWithPP (prettyPrinter dbus)
        , layoutHook         = 
          desktopLayoutModifiers $
          (Tall 1 0.03 0.5 ||| Full ||| simpleTabbed)
        , manageHook         =
                myManageHook
            <+> manageDocks
            <+> manageHook defaultConfig
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , terminal           = "gnome-terminal"
        , keys               = myKeys <+> keys defaultConfig
        , borderWidth        = 1
        , normalBorderColor  = "gray"
        , focusedBorderColor = "crimson"
        , focusFollowsMouse  = True
        , workspaces = map show [1..9]
        , startupHook        =
            gnomeRegister2 >> startupHook defaultConfig
        }

myManageHook = composeAll . concat $
    [ [isFullscreen --> myDoFullFloat]
    , [className =? c --> doIgnore      | c <- myIgnores]
    , [className =? c --> doCenterFloat | c <- myFloats]
    , [appName   =? n --> doCenterFloat | n <- myNames]
    , [currentWs =? n --> insertPosition Below Newer | n <- ["1", "2"]]
    ] where
        -- classnames
        myMovie  = ["mplayer2", "Vlc"]
        myFloats = myMovie ++
            [ "Xmessage"
            , "XFontSel"
            , "Do"
            , "Downloads"
            , "Nm-connection-editor"
            , "Launchbox"
            ]

        -- resources
        myIgnores = ["desktop", "desktop_window", "notify-osd", "stalonetray"]

        -- names
        myNames = ["bashrun", "Google Chrome Options", "Chromium Options"]

        -- a trick for fullscreen but stil allow focusing of other WSs
        myDoFullFloat :: ManageHook
        myDoFullFloat = doF W.focusDown <+> doFullFloat

myKeys = flip EZ.mkKeymap [
      ("M-r", spawn "gnome-do")
    , ("S-M-n", spawn "nautilus --no-desktop --browser")
    , ("S-M-q", spawn "gnome-session-quit")
    , ("M-u", prevScreen )
    , ("S-M-u", shiftPrevScreen )
    , ("M-i", nextScreen )
    , ("S-M-i", shiftNextScreen )
    ]

gnomeRegister2 :: MonadIO m => m ()
gnomeRegister2 = io $ do
    x <- lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=literal"
            ,"--dest=org.gnome.SessionManager"
            ,"/org/gnome/SessionManager"
            ,"org.gnome.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:" ++ sessionId]

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "#DD4814" . pangoSanitize
    , ppVisible  = pangoSanitize
    , ppHidden   = pangoColor "#333333" . pangoSanitize
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
