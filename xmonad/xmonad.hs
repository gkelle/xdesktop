import XMonad hiding ( (|||) )
import XMonad.Config.Xfce
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Config.Desktop

import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Fullscreen
import XMonad.Layout.IM
import XMonad.Layout.FixedColumn
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.LimitWindows
import XMonad.Layout.Minimize
import XMonad.Layout.Reflect
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane


import XMonad.Layout.DragPane
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.SimplestFloat

import XMonad.Actions.UpdatePointer
--import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.CopyWindow

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place

myWorkspaces = ["1:www","2:mail","3:shell","4:vm","5:chat","6:grid","7:float","8:tabbed","9:graphics"]

myManageHook = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doShift  "2:mail"   |   c   <- myMail   ] -- move mail to mail 
    , [className    =? c            --> doShift  "4:vm"   |   c   <- myVirtual  ] -- move vbox and kvm to vm
    , [className    =? c            --> doShift  "5:chat"   |   c   <- myChats  ] -- move chats and ims to chats
    , [name    =? n            --> doShift  "8:tabbed" <+> doF W.focusDown  |   n   <- myGames  ] -- move games to games
    , [className    =? "Gimp"           --> doShift  "9:graphics"  ] -- move graphical programs to graphics
    , [role    =? "gimp-toolbox"  <||> role =? "gimp-image-window"           --> unfloat  ] -- flatten certain gimp windows
    , [className    =? c            --> doFloat             |   c   <- myFloats ] -- float my floats
    , [className    =? c            --> doCenterFloat       |   c   <- myCenterClassNames  ] -- float my classnames
    , [name         =? n            --> doCenterFloat       |   n   <- myCenterNames  ] -- float my names
    , [name         =? n            --> doFloat             |   n   <- myNames  ] -- float my names
    , [role         =? r            --> resizeMiddleFloat       |   r   <- myRoles  ] -- float my roles
    , [isDialog                     --> doCenterFloat                           ] 
    , [isFullscreen                 --> (doF W.focusDown <+> doFullFloat)          ] -- YouTube fullscreen fix
    --, [className    =? c            --> placeHook myPlacement             |   c   <- myGames ] -- float my floats
    ])

    where
        unfloat = ask >>= doF . W.sink

	resizeMiddleFloat = doRectFloat $ W.RationalRect 0.1 0.25 0.8 0.5

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["MuPDF","Wine"]
	myVirtual = ["VirtualBox","Virt-manager"]
        myMail    = ["Thunderbird","Mail","Calendar"]
        myChats   = ["Pidgin","Skype","Slack","Rocket.Chat+"]
	myCenterClassNames = ["Xscreensaver-demo"]
	myGames = ["Steam"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer","xfce4-notifyd","Xfce4-notifyd","Xfdesktop"]
       -- names
        myCenterNames = ["Options"]
        myNames   = ["Oracle VM VirtualBox Manager","Virtual Machine Manager","cacaview","Guake!"]
        --roles
        myRoles   = ["Preferences","GtkFileChooserDialog"]

myLogHook = workspaceNamesPP xmobarPP{ppUrgent = xmobarColor "purple" "" . wrap "<" ">"} >>= dynamicLogString >>= xmonadPropLog

main = do 
--spawn "xmobar ~/.xmonad/xmobarcc"
	xmonad $ withUrgencyHook NoUrgencyHook $ xfceConfig {
	workspaces = myWorkspaces,
	logHook = myLogHook >> logHook desktopConfig >> updatePointer (0.5, 0.5) (0, 0),
	modMask = mod4Mask,
	layoutHook = myLayout,
	manageHook = myManageHook <+> fullscreenManageHook <+> manageHook xfceConfig,
	startupHook = do
		setWMName "LG3D"
	--	spawn "xsetroot -cursor_name left_ptr"
		spawnOnce "sleep 10 && /usr/local/bin/xmobar -x 0 ~/.xmonad/xmobarcc"
		spawnOnce "sleep 12 && /usr/local/bin/trayer-srg --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16 --monitor 0"
		spawnOnce "sleep 12 && /usr/bin/nm-applet"
       } `additionalKeysP` myKeys

myLayout = smartBorders $ workspaceLayouts
tiledLayout   = ResizableTall nmaster delta ratio []
  where
     nmaster = 1
     ratio = toRational (2/(1 + sqrt 5 :: Double))
     delta   = 3/100

workspaceLayouts =
  onWorkspace "1:www" webLayouts $
  onWorkspace "5:chat" chatLayouts $
  onWorkspace "4:vm" vmLayouts $
  onWorkspace "6:grid" gridLayouts $
  onWorkspace "7:float" floatLayouts $
  onWorkspace "8:tabbed" gameLayouts $
  onWorkspace "9:graphics" gimpLayouts $
  defaultLayouts
  where
    tabbedLayout = tabbedBottomAlways shrinkText defaultTheme
    floatLayouts = desktopLayoutModifiers $ simplestFloat ||| Full
    gimpLayouts = desktopLayoutModifiers $ minimize $ withIM (1/6) (Role "gimp-toolbox") $ reflectHoriz $ withIM (1/7) (Role "gimp-dock") (trackFloating $ tabbedBottomAlways shrinkText defaultTheme)
    webLayouts = desktopLayoutModifiers $ minimize $ Mirror tiledLayout ||| tiledLayout ||| Circle ||| tabbedLayout ||| noBorders Full

    chatLayouts = desktopLayoutModifiers $ minimize $ GridRatio (4/3) ||| tiledLayout ||| Mirror tiledLayout ||| tabbedLayout
    skypeLayouts = skypeLayout ||| tabbedLayout
    gameLayouts = tabbedLayout ||| Full
    vmLayouts = defaultLayouts
    gridLayouts = desktopLayoutModifiers $ minimize $ Grid ||| magnifiercz' 1.4 ( Grid )
    defaultLayouts = desktopLayoutModifiers $ minimize $ tiledLayout ||| Mirror tiledLayout ||| tabbedLayout ||| Circle ||| Grid ||| (limitWindows 3 $ magnifiercz' 1.4 $ Mirror (Tall 1 (3/100) (2/3))) ||| noBorders Full

    imLayout = withIM (1/6) (Or (Title "Buddy List")
                                (Title "Buddy List")) Grid

    skypeLayout = desktopLayoutModifiers $ withIM (1/6) skypeMainWindow Grid
    skypeMainWindow  = (ClassName "Skype") `And`
              (Not (Title "Options")) `And`
              (Not (Role "Chats"))    `And`
              (Not (Role "CallWindow"))    `And`
              (Not (Role "ConversationsWindow"))

currentLayoutName = dynamicLogString defaultPP { ppOrder= \(_:l:_:_) -> [l] }
myKeys = [
--For use with resizeable tall
      ("M-z", sendMessage MirrorShrink),
      ("M-a", sendMessage MirrorExpand),
--For use with Minimize      
      ("M-m", withFocused minimizeWindow),
      ("M-S-m", sendMessage RestoreNextMinimizedWin),
--For use with LayoutCombinator
      ("M-f", sendMessage $ JumpToLayout "Full")

--For use with ActionsCopyWindow
--      ("M-v", windows copyToAll),
--      ("M-S-v", killAllOtherCopies)
    ] 

--Allow a workspace to span multiple monitors
    ++ [  ("M-x", rescreen), ("M-S-x", layoutScreens 2 $ desktopLayoutModifiers (fixedLayout [Rectangle 2400 840 1920 1080, Rectangle 0 0 2400 1920 ]) ) ]
    ++ [  ("M-C-x", layoutSplitScreen 2 $ desktopLayoutModifiers (TwoPane 0.5 0.5) ) ]


--Switch layout and print out name of layout
    ++ [ ("M-S-<Space>",  sendMessage NextLayout >> (currentLayoutName >>= \d->spawn $"notify-send -u low -t 300 "++d)) ]

--Bind the wer keys to the correct screen
    ++ [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr)  <- zip "wer" [1,2,0] -- was [0..] *** change to match your screen order ***
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]
--Swap workspace positions
--    ++ [ ("M-C-" ++ [k], windows $ swapWithCurrent i) | (i, k) <- zip myWorkspaces "123456789"]
    ++ [ ("M-C-" ++ [k], swapWithCurrent i) | (i, k) <- zip myWorkspaces "123456789"]
