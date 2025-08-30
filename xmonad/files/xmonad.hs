import qualified Data.Map as M
import System.Environment

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks
import System.Exit
import qualified XMonad.StackSet as W
import Control.Monad
import Data.Bool  (bool)
import Data.Maybe
import System.IO
import XMonad.Util.Run(spawnPipe)

main = do
  xmproc <- spawnPipe "xmobar"
  -- Use different set of layouts if env var XMONAD_USE_VERTICAL_LAYOUT is 1. See
  -- https://stackoverflow.com/a/60715978/782130.
  isVertical <- lookupEnv "XMONAD_USE_VERTICAL_LAYOUT"
  case myLayout (fromMaybe "0" isVertical) of Layout l -> xmonad $ desktopConfig {
      terminal = "kitty -1",
      modMask = mod4Mask,
      workspaces = ["main", "web", "3", "4"],
      borderWidth = 3,
      focusedBorderColor = "#D04245",
      normalBorderColor = "#273644",
      layoutHook = spacingRaw True (Border 20 20 20 20) False (Border 20 20 20 20) True $ l,
      manageHook = myManageHook <+> manageHook desktopConfig,
      keys = myKeys,
      mouseBindings = myMouseBindings,
      logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc,
                  ppCurrent = xmobarColor "#ECA964" "" . wrap "[" "]",
                  -- ppTitle = xmobarColor "#8DC85F" "" . shorten 50,
                  ppOrder = \(spaces:layout:_) -> [spaces, layout] -- Don’t display window title.
                }
    }

-- https://stackoverflow.com/a/50427647
findWindows :: String -> X [Window]
findWindows name = do
  withWindowSet $ (\ws -> do
    forM (W.allWindows ws)
      (\w -> do
            s <- withDisplay $ \d -> fmap resClass . liftIO $ getClassHint d w
            return $ bool [] [w] (s == name) :: X [Window]
      ) >>= return . join
    )

-- https://stackoverflow.com/a/60715978/782130
myLayout :: Show a => String -> Layout a
myLayout vertical = if vertical /= "1"
  then Layout $ onWorkspace "web" (avoidStruts $ (multiCol [1] 1 0.02 (-0.5)) ||| Full) $
       (avoidStruts $ (ThreeColMid 1 0.02 (3/7)) ||| Full)
       -- (avoidStruts $ (ThreeColMid 1 0.02 (31/42)) ||| Full)
  else Layout $ onWorkspace "web" (avoidStruts $ Mirror (multiCol [1] 1 0.02 (-0.5)) ||| Full) $
       (avoidStruts $ Mirror (ThreeColMid 1 0.02 (1/2)) ||| Full)

myManageHook = composeAll [
    -- send applications to the right workspace
    className =? "Firefox-esr" --> doShift "web",
    className =? "Firefox" --> doShift "web",
    className =? "firefox" --> doShift "web",
    className =? "Navigator" --> doShift "web",
    className =? "Google-chrome" --> doShift "web" ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w
                                           >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_space), spawn "dmenu_run -i -p '>' -fn 'Iosevka SS09-10'") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window
    , ((modMask .|. controlMask, xK_3), spawn "maim -u ~/pictures/Screenshots/$(date '+%FT%H_%M_%S').png")
    , ((modMask .|. controlMask, xK_4), spawn "maim -su ~/pictures/Screenshots/partial-$(date '+%FT%H_%M_%S').png")

    , ((modMask, xK_grave ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    -- , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    -- , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    -- , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask .|. shiftMask, xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
    -- -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    -- Keys to switch to specific windows
    [ ((modMask, xK_o), do
          win' <- findWindows "kitty"
          when (length win' > 0) (windows $ W.focusWindow $ head win'))
    , ((modMask, xK_e), do
          win' <- findWindows "Emacs"
          when (length win' > 0) (windows $ W.focusWindow $ head win'))
    ]
