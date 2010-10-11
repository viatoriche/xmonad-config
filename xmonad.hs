--

-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

-- import XMonad
import XMonad hiding ( (|||) )
import System.Exit
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageDocks
{-import XMonad.Hooks.EwmhDesktops-}
import XMonad.Util.EZConfig
{-import XMonad.Util.Run-}
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Actions.NoBorders
import Graphics.X11.Xlib.Extras
import XMonad.Actions.CycleWS
import Foreign.C.Types (CLong)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.Spiral
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.FloatKeys
{-import System.IO-}
{-import qualified System.IO.UTF8-}
import XMonad.Config.Gnome

import Codec.Binary.UTF8.String (encodeString)
import Control.Monad (liftM2)
import Data.Char ( isSpace, ord )
import Data.List (intersperse, isPrefixOf, sortBy)
import Data.Maybe ( isJust, catMaybes )
import Data.Ord ( comparing )
import qualified Data.Map as M
import qualified XMonad.StackSet as S

import Foreign.C (CChar)

import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedWindows
import XMonad.Util.Run

import XMonad.Layout.LayoutModifier



-- Взять значение свойства окна
getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- Эта функция проверяет, выставлено ли свойство окна name в значение value
checkAtom name value = ask >>= \w -> liftX $ do
          a <- getAtom name
          val <- getAtom value
          mbr <- getProp a w
          case mbr of
            Just [r] -> return $ elem (fromIntegral r) [val]
            _ -> return False
-- Эта функция проверяет, является ли окно диалогом
checkDialog = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
checkMenu = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"
checkFullState = checkAtom "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN"



-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "terminal"

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#babdb6"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- Media Keys

xK_XF86AudioPrev = 0x1008ff16
xK_XF86AudioNext = 0x1008ff17
xK_XF86AudioPlay = 0x1008ff14
xK_XF86AudioStop = 0x1008ff15
xK_XF86Eject = 0x1008ff2c
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioLowerVolume = 0x1008ff11
xK_XF86AudioMute = 0x1008ff12

xK_XF86PowerOff = 0x1008ff2a
xK_XF86Sleep = 0x1008ff2f
xK_XF86WakeUp = 0x1008ff2b
xK_Help = 0xff6a
xK_XF86Favorites = 0x1008ff30
xK_XF86HomePage = 0x1008ff18
xK_XF86Search = 0x1008ff1b


myEZKeys = [ 

             ("M-u",    spawn "browser")
            ,("M-S-u",  spawn "browser `xsel -o`")
            ,("M-y M-y",  spawn "myscrot")
            ,("M-y M-f",  spawn "myscrot -u")
            ,("M-y M-u",  spawn "myscrot -u")
            ,("M-y M-s",  spawn "myscrot -s")
            ,("M-z M-z", kill)
             -- Run Programs Mod+R

            ,("M-r M-b", spawn "browser")
            ,("M-r M-g", spawn "gimp") 
            ,("M-r M-c", spawn "oocalc") 
            ,("M-r M-e", spawn "evince") 
            ,("M-r M-t", spawn "tkabber") 
            ,("M-r M-f", spawn "fbreader") 
            ,("M-r M-m", spawn "comix") 
            ,("M-r M-l", spawn "calc") 
            ,("M-r M-s", spawn "skype") 
            ,("M-r M-p", spawn "pidgin") 
            ,("M-r M-r", spawn "mygmrun") 
            ,("M-r M-w", spawn "oowriter") 
            ,("M-r M-v", spawn "VirtualBox")
            ,("M-r M-a", spawn "gksu mega")
            ,("M-r M-h", spawn "hotkey.py")
            ,("M-r M-q", spawn "gucharmap")
            ,("M-r M-x", spawn "terminal -e screen -Rx viator")

            -- 888888888888888 MOCP HOTKEYS 888888888888888 --
            -- Mod+S

            , ("M-s M-j",     spawn "mocp --next")
            , ("M-s M-k",     spawn "mocp --previous")
            , ("M-s M-p",     spawn "mocp --toggle-pause")
            , ("M-s M-s",     spawn "mocp --stop" )
            , ("M-s M-l",     spawn "mocp --seek +10")
            , ("M-s M-h",     spawn "mocp --seek -10")
            , ("M-s M-1",     spawn "industrial")
            , ("M-s M-2",     spawn "newage")
            , ("M-s M-3",     spawn "sky")
            , ("M-s M-4",     spawn "indradio")
            , ("M-s M-5",     spawn "psy")

            , ("M-a M-q",     spawn "vplus")
            , ("M-S-a M-S-q",   spawn "vminus")
            , ("M-a M-w",     spawn "vplus2")
            , ("M-S-a M-S-w",   spawn "vminus2")
            , ("M-a M-e",     spawn "vplus3")
            , ("M-S-a M-S-e",   spawn "vminus3")
            , ("M-a M-m",     spawn "mastermute")
            , ("M-a M-f",     spawn "frontmute")
            , ("M-a M-a",    spawn "terminal -e alsamixer")

            -- Go To Layouts Keys --

            -- Horizontal orientation M-d ; Vertical Orientation M-f
            , ("M-d M-s", sendMessage $ JumpToLayout "h_std") -- standard
            , ("M-f M-s", sendMessage $ JumpToLayout "v_std")
            , ("M-d M-t", sendMessage $ JumpToLayout "tabs") -- tabs
            , ("M-d M-f", sendMessage $ JumpToLayout "full") -- full
            , ("M-d M-1", sendMessage $ JumpToLayout "h_tabs/2")    
            , ("M-f M-1", sendMessage $ JumpToLayout "v_tabs/2")    
            , ("M-d M-2", sendMessage $ JumpToLayout "tabs/h_std")
            , ("M-f M-2", sendMessage $ JumpToLayout "tabs/v_std")    
            , ("M-d M-3", sendMessage $ JumpToLayout "std/2")
            , ("M-f M-r", sendMessage $ JumpToLayout "v_spir") -- spiral
            , ("M-d M-r", sendMessage $ JumpToLayout "h_spir")
            , ("M-d M-g", sendMessage $ JumpToLayout "gimp")
            , ("M-d M-i", sendMessage $ JumpToLayout "IM")
            , ("M-d M-a", sendMessage $ JumpToLayout "all")
            , ("M-f M-f", sendMessage $ JumpToLayout "full")
            , ("M-d M-d", sendMessage $ JumpToLayout "all")
            , ("M-S-f M-S-f", sendMessage $ JumpToLayout "all")
            , ("M-S-d M-S-d", sendMessage $ JumpToLayout "full")
           ]


myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    , ((modMask,               xK_p     ), spawn "mygmrun")

--    , ((0,                     0xff67   ), spawn "hotkey.py" )

    , ((0,               0xff67   ), spawn "mygmrun" )

    , ((modMask .|. shiftMask, xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

--    , ((modMask,               xK_i     ), spawn "hotkey.py" )

    , ((modMask,                xK_slash), spawn "acpitool | xmessage -timeout 3 -center -default okay -file - &" )

    , ((modMask,             xK_i), spawn "xmessage -center -timeout 3 `moc_title.sh`")

    , ((modMask,             xK_o), spawn "tint2")

    , ((modMask .|. shiftMask,    xK_o), spawn "killall tint2")

    , ((0,                    xK_XF86HomePage), spawn "gksu mega")


    -- *************** My HotKeys ***************

    
        -- Keys Control MultiTiles

    , ((modMask .|. mod1Mask,    xK_l ), sendMessage $ Go R)
    , ((modMask .|. mod1Mask,    xK_h ), sendMessage $ Go L)
    , ((modMask .|. mod1Mask,    xK_k ), sendMessage $ Go U)
    , ((modMask .|. mod1Mask,    xK_j ), sendMessage $ Go D)
    {-, ((modMask .|. controlMask, xK_Right), sendMessage $ Swap R)-}
    {-, ((modMask .|. controlMask, xK_Left ), sendMessage $ Swap L)-}
    {-, ((modMask .|. controlMask, xK_Up   ), sendMessage $ Swap U)-}
    {-, ((modMask .|. controlMask, xK_Down ), sendMessage $ Swap D)-}
    , ((modMask .|. mod1Mask .|. shiftMask,   xK_l ), sendMessage $ Move R)
    , ((modMask .|. mod1Mask .|. shiftMask,   xK_h ), sendMessage $ Move L)
    , ((modMask .|. mod1Mask .|. shiftMask,   xK_k ), sendMessage $ Move U)
    , ((modMask .|. mod1Mask .|. shiftMask,   xK_j ), sendMessage $ Move D)


    , ((modMask,   xK_Page_Down ), withFocused (keysResizeWindow (25,15) (0,0)))

    , ((modMask,   xK_Page_Up ), withFocused (keysResizeWindow (-15,-15) (0,0)))


--    , ((modMask,   xK_Page_Up ),  )

    , ((modMask,  xK_v),  sendMessage ToggleStruts)

    , ((0,                     xK_Print ), spawn "myscrot")
    , ((modMask,               xK_Print ), spawn "myscrot -u")
    , ((modMask .|. shiftMask,             xK_Print ), spawn "myscrot -s")

    {-, ((modMask,   xK_o), spawn "xfce4-panel")-}
    {-, ((modMask .|. shiftMask,   xK_o), spawn "killall xfce4-panel")-}


    , ((modMask,               xK_b     ), withFocused toggleBorder)



    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- conf- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    -- TODO, update this binding with avoidStruts , ((modMask              , xK_b     ),

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_Escape     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_Escape     ), restart "xmonad" True)

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modMask, button4), (\w -> prevWS ))

    , ((modMask, button5), (\w -> nextWS ))
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--onebig = windowNavigation (tile ***|* coltile)
--           where
--             -- компоновка для левой части
--             -- master-окно занимает 3/4 по высоте
--             tile = Mirror $ Tall 1 (1/100) (3/4)
--             -- компоновка для правой части
--             -- располагает все окна в один столбец
--             coltile = Tall 0 (1/100) (1/2)

--  ["1br","2xm","3fm","4cd","5mm","6tr","7vr","8gm","9gv"]
--
myTabConfig = defaultTheme { {-activeColor            = "#555753"-}
                            activeColor            = "#2e3436"
                            {-,inactiveColor          = "#40413f"-}
                            ,inactiveColor          = "#3A3E3F"
                            ,inactiveBorderColor    = "#2e3436"
                            ,activeBorderColor      = "#babdb6"
                            {-,activeTextColor        = "#babdb6"-}
                            ,activeTextColor        = "#babdb6"
                            ,inactiveTextColor      = "#969894"
                            ,urgentColor            = "#555753"
                            ,urgentBorderColor      = "#969894"
                            ,urgentTextColor        = "#babdb6"
                            ,fontName               = "xft:Terminus-12"
                            ,decoHeight             = 18
                           }

myTabs = tabbed shrinkText myTabConfig

myLayout = smartBorders $ named "all" all ||| named "v_std" lvta ||| named "h_std" lhta ||| named "tabs" myTabs ||| named "full" myfull ||| named "v_tabs/2" ltvtz ||| named "h_tabs/2" lthtz ||| named "tabs/h_std" lthtx |||  named "v_spir" myspiral ||| named "h_spir" hmyspiral ||| named "std/2" lthtd ||| named "gimp" lgimp ||| named "IM" im ||| named "tabs/v_std" ltvtx
  where
    lvta = Tall 1 (3/100) (1/2)
    all  = Tall 0 (3/100) (1/2)
    lhta = Mirror $ Tall 1 (3/100) (1/2)
    myfull = Full
    myspiral = spiral (6/7)
    hmyspiral = Mirror $ spiral (6/7)
    ltvtz = windowNavigation (main *||* other)
               where
                 main =  myTabs
                 other = myTabs
    lthtz = windowNavigation (main *//* other)
               where
                 main = myTabs
                 other = myTabs
    ltvtx = windowNavigation (main *||* other)
               where
                 main = myTabs
                 other = Tall 0 (3/100) (1/2)
    lthtx = windowNavigation (main *//* other)
               where
                 main = myTabs
                 other = Mirror $ Tall 0 (3/100) (1/2)
    lthtd = windowNavigation (main *//* other)
               where
                 main = Tall 1 (3/100) (1/2)
                 other =  Tall 1 (3/100) (1/2)
    lgimp = windowNavigation (main ****||* other)
               where
                 main =  myTabs
                 other = myTabs
    im =      windowNavigation (t_w **||* main *||* rst)
               where
                 main  = Tall 0 (3/100) (1/2)
                 t_w = myTabs
                 rst = Tall 0 (3/100) (1/2)


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
--  myWorkspaces    =  ["1br","2xm","3fm","4cd","5mm","6tr","7vr","8gm","9gv"]


myManageHook = composeAll . concat $
    [ [className =? r      --> doIgnore | r <- myIgnores]
    , [title =? tig         --> doIgnore | tig <- myTitleIgnore]
    , [className =? bw     --> moveTo "1" | bw  <- browsers]
    , [className =? xm     --> moveTo "2" | xm  <- xmpp]
    , [className =? fil    --> moveTo "3" | fil <- files] 
    , [className =? tru    --> moveTo "4" | tru <- trueList]
    , [className =? med    --> moveTo "5" | med <- media]
    , [className =? tor    --> moveTo "6" | tor <- torrents]
    , [className =? vrt    --> moveTo "7" | vrt <- virtuals]
    , [className =? gim    --> moveTo "8" | gim <- gimp]
    , [className =? gov    --> moveTo "9" | gov <- govnoList]

    ]
    where      
      myIgnores = ["trayer", "fbpanel", "stalonetray"]
      myTitleIgnore = ["%^&%^"]
      xmpp = ["Tkabber","Dialog","Xchat","Skype", "Chat", "Pidgin"] --2
      browsers = ["Shiretoko", "Firefox", "Opera", "Midori", "Chrome"] --1
      files = ["Gnome-commander", "dolphin", "Dolphin", "Krusader"] --3
      trueList = ["&^**^&"] --4
      media = ["Audacious", "MPlayer"] --5
      torrents = ["Transmission", "Ktorrent"] --6
      virtuals = ["VirtualBox"] --7
      gimp = ["Gimp","Gimp-2.6"] --8
      govnoList =["&*()(^"] --9
      moveTo = doF . W.shift

myFloatsHook = composeAll . concat $
    [ [className =? c      --> doFloat | c <- myFloats]
    , [title =? t          --> doFloat | t <- myOtherFloats]
    ]
    where
      myFloats = ["feh", "MPlayer", "Nitrogen", "Sonata", "Wine", "Gnome-alsamixer", "net-sourceforge-jnlp-runtime-Boot", "lxpanel", "xmessage", "Xmessage"]
      myOtherFloats = ["Python Menu","Downloads", "Firefox Preferences", "Save As...", "Send file", "Open", "File Transfers", "Chromium Options", "HotKey", "Xmessage"]

-- Сделать меню плавающими
manageMenus = checkMenu --> doFloat
-- Сделать диалоги плавающими
manageDialogs = checkDialog --> doFloat
manageFullState = checkFullState --> doFloat

--myToggleHook = composeAll
--    [ className =? "MPlayer"        --> toggleBorder
--   ]



-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
--myLogHook = return ()
--

{-myLogHook = dynamicLogWithPP xmobarPP-}
{-myStatusBar = "/home/viator/.xmonad/status.log"-}

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
--

viaPP :: PP
viaPP = defaultPP { ppCurrent = xmobarColor "#e9b96e" "" . wrap "[" "]"
                     , ppTitle   = xmobarColor "green"  "" . shorten 40
                     , ppVisible = wrap "(" ")"
                     , ppUrgent  = xmobarColor "#d42807" "#e9b96e"
                     , ppLayout  = xmobarColor "#d42807" ""
                  }



myStartupHook = return ()

dynamicLogVia :: X ()
dynamicLogVia = dynamicLogWithPPVia viaPP

dynamicLogWithPPVia :: PP -> X ()
dynamicLogWithPPVia pp = dynamicLogStringVia pp >>= io . ppOutput pp

dynamicLogStringVia :: PP -> X String
dynamicLogStringVia pp = do

    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp

    let ld = description . S.layout . S.workspace . S.current $ winset

    let ws = pprWindowSet sort' urgents pp winset

    extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

    return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        ]
                        ++ catMaybes extras

sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = concat . intersperse sep . filter (not . null)
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
xmonad $ gnomeConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        {-numlockMask        = myNumlockMask,-}
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts myLayout,
        manageHook         = myManageHook <+> manageDocks <+> myFloatsHook <+> manageMenus <+> manageDialogs <+> manageFullState,
        {-logHook            = dynamicLogWithPP $ xmobarPP,-- {ppOutput = System.IO.hPutStrLn System.IO.stdout },-}
        logHook           = do
                            dynamicLogVia
                            logHook gnomeConfig,
        startupHook        = myStartupHook
    } `additionalKeysP` myEZKeys
