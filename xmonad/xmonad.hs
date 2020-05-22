import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import Text.Printf (printf)

import qualified XMonad.StackSet as W
import qualified XMonad.Layout.BoringWindows as BW

import XMonad.Actions.Minimize
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceNames

import XMonad.Hooks.DynamicLog --used to update xmobar
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName -- fixes java window issue
import XMonad.Hooks.UrgencyHook --used to update xmobar

import XMonad.Layout.Circle
import XMonad.Layout.Gaps
import XMonad.Layout.BorderResize
import XMonad.Layout.Fullscreen
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders -- smartBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

myManageHook = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? "Gimp"           --> doShift  "9:graphics"  ] -- move graphical programs to graphics
    , [role    =? "gimp-toolbox"  <||> role =? "gimp-image-window"           --> unfloat  ] -- flatten certain gimp windows
    , [className    =? c            --> doFloat             |   c   <- myFloats ] -- float my floats
    , [className    =? c            --> doCenterFloat       |   c   <- myCenterClassNames  ] -- float my classnames
    , [className    =? "mpv"        --> toprightFloat ]
    , [name         =? n            --> doCenterFloat       |   n   <- myCenterNames  ] -- float my names
    , [name         =? n            --> doFloat             |   n   <- myNames  ] -- float my names
    , [role         =? r            --> resizeMiddleFloat       |   r   <- myRoles  ] -- float my roles
    , [isDialog                     --> doCenterFloat                           ] 
    ])

    where
        unfloat = ask >>= doF . W.sink
        toprightFloat = doRectFloat $ W.RationalRect (3/4) 0 (1/4) (1/4)
	resizeMiddleFloat = doRectFloat $ W.RationalRect 0.1 0.25 0.8 0.5

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["MuPDF","Wine"]
	myCenterClassNames = ["Xscreensaver-demo"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer","xfce4-notifyd","Xfce4-notifyd","Xfdesktop","dzen2","netflix"]
       -- names
        myCenterNames = ["Options"]
        myNames   = ["Oracle VM VirtualBox Manager","Virtual Machine Manager","cacaview","Guake!"]
        --roles
        myRoles   = ["Preferences","GtkFileChooserDialog"]
myLogHook = workspaceNamesPP xmobarPP{ppUrgent = xmobarColor "purple" "" . wrap "<" ">", ppHidden = xmobarColor "gray" "" . noScratchPad, ppHiddenNoWindows = const "", ppOrder = \(ws:_:t:_) -> [ws,t] } >>= dynamicLogString >>= xmonadPropLog
  where
    noScratchPad ws = if ws == "NSP" then "" else ws

myWorkspaces = ["1:www","2:mail","3:shell","4:vm","5:chat","6:grid","7:float","8:tabbed","9:graphics"]

myStartupHook = do
  setWMName "LG3D"

defaultLayouts = avoidStruts ( Circle ||| tiled ||| tabbed ||| (gaps [(U,4), (D,4), (L,4), (R,4)] $ spacing 8 $ ThreeColMid 1 (20/100) (56/100)) )
  where
    tabbed = tabbedBottomAlways shrinkText def
    tiled   = spacing 0 $ ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = toRational (2/(1 + sqrt 5 :: Double))
    delta = 3/100

myLayouts = lessBorders OnlyScreenFloat $
            smartBorders $
            borderResize $
            minimize $ maximize $
            mkToggle (NOBORDERS ?? FULL ?? EOT) .
            mkToggle (single NBFULL) .
            mkToggle (single MIRROR) $
            defaultLayouts

main = xmonad
    $ docks
    $ withUrgencyHook dzenUrgencyHook {args = ["-bg", "darkgreen", "-xs", "1"], duration = 5000000 }
    $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "#ff0000" } urgencyConfig { suppressWhen = XMonad.Hooks.UrgencyHook.Focused }
    $ xfceConfig
    { borderWidth        = 2
    , terminal           = "urxvt"
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00" 
    , modMask = mod4Mask
    , workspaces = myWorkspaces
    , manageHook = myManageHook <+> namedScratchpadManageHook myScratchPads <+> manageDocks <+> fullscreenManageHook <+> manageHook xfceConfig
    , logHook = myLogHook >> logHook xfceConfig >> updatePointer(0.5, 0.5) (0, 0)
    , startupHook = myStartupHook
    , layoutHook = myLayouts
    , handleEventHook = fullscreenEventHook
    } `additionalKeysP` myKeys

myScratchPads = [
    NS "xterm" "xterm -xrm 'XTerm.vt100.allowTitleOps: false' -T scratchterm1" (title =? "scratchterm1") (customFloating $ W.RationalRect 0 0 1 (1/3))
   ,NS "music" "xterm -xrm 'XTerm.vt100.allowTitleOps: false' -T ncmpcpp_player -e ncmpcpp" (title =? "ncmpcpp_player") (customFloating $ W.RationalRect 0 0 (2/5) (1/3))
   ,NS "netflix" "firefox --kiosk --class netflix -P netflix --no-remote --new-window https://www.netflix.com" (className =? "netflix") (customFloating $ W.RationalRect (3/4) 0 (1/4) (1/4))
   ]

--curLayout :: X String
--curLayout = gets windowset >>= return . description . W.layout . W.workspace . W.current
currentLayoutName = dynamicLogString defaultPP { ppOrder= \(_:l:_:_) -> [l] }
myKeys = [
    ] 
--For use with Minimize      
    ++ [  ("M-m", withFocused minimizeWindow <+> windows W.focusDown)
         ,("M-S-m", withLastMinimized maximizeWindowAndFocus)
         ,("M-C-m", withFocused (sendMessage . maximizeRestore))
    ]

--Bind the wer keys to the correct screen
    ++ [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr)  <- zip "wer" [1,2,0] -- was [0..] *** change to match your screen order ***
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]
--Swap workspace positions
    ++ [ ("M-C-" ++ [k], swapWithCurrent i) | (i, k) <- zip myWorkspaces "123456789"]

--Toggle layout properties
    ++ [ ("M-f", sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
        ,("M-C-f", sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL)
        ,("M-C-S-m", sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
       ]
--Scratchpads
    ++ [
         ("M-C-i", namedScratchpadAction myScratchPads "music")
        ,("M-C-o", namedScratchpadAction myScratchPads "netflix")
       ]
    ++ [
         ("M-b", sendMessage ToggleStruts)
       ]
    ++ [
         ("<F12>", namedScratchpadAction myScratchPads "xterm")
       ]
    ++ [
         ("M-<Space>", sendMessage NextLayout >> (currentLayoutName >>= \d->spawn $ printf "echo %s | dzen2 -bg darkgreen -xs 1 -p 1" d))
       ]
