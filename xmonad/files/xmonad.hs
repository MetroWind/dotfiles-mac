import qualified Data.Map as M

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.ManageDocks
import System.Exit
import qualified XMonad.StackSet as W
import Control.Monad
import Data.Bool  (bool)
import System.IO
import XMonad.Util.Run(spawnPipe)

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ desktopConfig
    { terminal = "kitty -1",
      modMask = mod4Mask,
      workspaces = ["main", "web", "3", "4"],
      borderWidth = 3,
      focusedBorderColor = "#D04245",
      normalBorderColor = "#273644",
      layoutHook = myLayout,
      manageHook = myManageHook <+> manageHook desktopConfig,
      keys = myKeys,
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

myLayout =
  onWorkspace "web" (avoidStruts $ multiCol [1] 1 0.02 (-0.5) ||| Full) $
  (avoidStruts $ ThreeCol 1 0.02 (2/5) ||| Full)

myManageHook = composeAll [
    -- send applications to the right workspace
    className =? "Firefox-esr" --> doShift "web",
    className =? "Google-chrome" --> doShift "web" ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_space), spawn "dmenu_run -i -p '>' -fn 'Iosevka SS09-10'") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window
    , ((modMask .|. controlMask, xK_3), spawn "maim -u ~/Pictures/Screenshots/$(date '+%FT%T').png")
      -- The sleep is nessesary to make -s work.
    , ((modMask .|. controlMask, xK_4), spawn "maim -su ~/Pictures/Screenshots/partial-$(date '+%FT%T').png")
      -- Lock screen
    , ((modMask .|. controlMask, xK_l), spawn "xsecurelock")

    , ((modMask, xK_grave ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

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
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

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
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    -- Keys to switch to specific windows
    [ ((modMask, xK_o), do
          win' <- findWindows "kitty"
          when (length win' > 0) (windows $ W.focusWindow $ head win'))
    ]
