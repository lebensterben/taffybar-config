{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ImplicitParams #-}

import           Control.Monad                  ( void )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader     ( ReaderT(runReaderT)
                                                , ask
                                                )
import           Data.GI.Base
import qualified Data.GI.Gtk                   as Gtk
import qualified Data.Text                     as T
import qualified GI.Gdk                        as Gdk
import           GI.Gdk                         ( EventButton )
import           System.Process                 ( spawnProcess )
import           System.Taffybar.Context        ( Context
                                                , TaffyIO
                                                , runX11Def
                                                , subscribeToPropertyEvents
                                                , unsubscribe
                                                )
import           System.Taffybar.Hooks          ( NetworkInfoChan
                                                    ( NetworkInfoChan
                                                    )
                                                , getNetworkChan
                                                )
import           System.Taffybar.Information.CPU
                                                ( cpuLoad )
import           System.Taffybar.Information.Memory
                                                ( MemoryInfo(..)
                                                , parseMeminfo
                                                )
import           System.Taffybar.Information.Network
                                                ( sumSpeeds )
import           System.Taffybar.Information.X11DesktopInfo
                                                ( getAtom
                                                , readAsString
                                                , sendCommandEvent
                                                )
import           System.Taffybar.SimpleConfig   ( SimpleTaffyConfig(..)
                                                , StrutSize(..)
                                                , defaultSimpleTaffyConfig
                                                , simpleTaffybar
                                                )
import           System.Taffybar.Util           ( liftReader
                                                , postGUIASync
                                                )
import           System.Taffybar.Widget         ( ClockConfig(..)
                                                , WindowsConfig(..)
                                                , Workspace(..)
                                                , WorkspacesConfig(..)
                                                , defaultClockConfig
                                                , defaultNetFormat
                                                , defaultWindowsConfig
                                                , defaultWorkspacesConfig
                                                , networkGraphNew
                                                , showInfo
                                                , sniTrayNew
                                                , textClockNewWith
                                                , truncatedGetActiveLabel
                                                , truncatedGetMenuLabel
                                                , widgetSetClassGI
                                                , windowsNew
                                                , workspacesNew
                                                )
import           System.Taffybar.Widget.Generic.ChannelWidget
                                                ( channelWidgetNew )
import           System.Taffybar.Widget.Generic.Graph
                                                ( GraphConfig(..)
                                                , GraphDirection(..)
                                                , defaultGraphConfig
                                                )
import           System.Taffybar.Widget.Generic.PollingBar
                                                ( BarConfig
                                                    ( barBackgroundColor
                                                    , barBorderColor
                                                    )
                                                , defaultBarConfig
                                                , pollingBarNew
                                                )
import           System.Taffybar.Widget.Generic.PollingGraph
                                                ( pollingGraphNew )

mkRGBA :: Fractional a => (a, a, a, a) -> (a, a, a, a)
mkRGBA (r, g, b, a) = (r / 255, g / 255, b / 255, a)

blue, green, magenta, yellow1, yellow2 :: (Double, Double, Double, Double)
blue = mkRGBA (33, 150, 243, 1)
green = mkRGBA (77, 189, 140, 1)
magenta = mkRGBA (198, 120, 221, 1)
yellow1 = mkRGBA (236, 190, 123, 1)
yellow2 = mkRGBA (254, 204, 83, 1)
-- red = mkRGBA (255, 108, 107, 1)

colorGradient :: Double -> (Double, Double, Double)
colorGradient t
    | t < 0.5
    = ( (77 + (255 - 77) / 0.5 * t) / 255.0
      , (189 + (181 - 189) / 0.5 * t) / 255.0
      , (85 + (107 - 85) / 0.5 * t) / 255.0
      )
    | otherwise
    = (1.0, (181 + (107 - 181) / 0.5 * (t - 0.5)) / 255.0, 107 / 255.0)

readTemp :: IO Double
readTemp = (/ 100000.0) . read <$> readFile
    "/sys/devices/platform/coretemp.0/hwmon/hwmon3/temp1_input"

cpuCallback :: IO [Double]
cpuCallback = do
    (_, systemLoad, totalLoad) <- cpuLoad
    return [totalLoad, systemLoad]

memCallback :: IO [Double]
memCallback = do
    mi <- parseMeminfo
    return [memoryUsedRatio mi]

myGraphConfig :: GraphConfig
myGraphConfig = defaultGraphConfig
    { graphBackgroundColor = (0.1, 0.1, 0.1, 0.3)
    , graphBorderWidth     = 0
    , graphDirection       = RIGHT_TO_LEFT
    , graphWidth           = 75
    }

buttonOverlay :: Gtk.Widget -> T.Text -> IO () -> TaffyIO Gtk.Widget
buttonOverlay widget label action = lift $ do
    btn <- new Gtk.Button []
    _   <-
        on btn #enterNotifyEvent
        $  const
        $  set ?self [#label := label]
        >> return True
    _ <-
        on btn #leaveNotifyEvent
        $  const
        $  set ?self [#label := ""]
        >> return True
    _ <- on btn #buttonPressEvent $ \eButton -> do
        eType <- get eButton #type
        case eType of
            Gdk.EventType2buttonPress -> action >> return False
            _                         -> return True

    overlay <- new Gtk.Overlay []
    _       <- #add overlay widget
    _       <- #addOverlay overlay btn
    _       <- #showAll overlay

    Gtk.toWidget overlay

xLayoutProp :: String
xLayoutProp = "_XMONAD_CURRENT_LAYOUT"

dispatchButtonEvent :: Context -> EventButton -> IO Bool
dispatchButtonEvent context btn = do
    pressType    <- get btn #type
    buttonNumber <- get btn #button
    case pressType of
        Gdk.EventTypeButtonPress -> case buttonNumber of
            1 -> runReaderT (runX11Def () (switch (1 :: Integer))) context
                >> return True
            2 -> spawnProcess "xmonadctl" ["default-layout"] >> return True
            3 -> spawnProcess "xdotool" ["key", "Super+Shift+grave"]
                >> return True
            _ -> return False
        _ -> return False
    where
        switch n = do
            cmd <- getAtom xLayoutProp
            sendCommandEvent cmd (fromIntegral n)

main :: IO ()
main = do
    let sep = do
            sep' <- new
                Gtk.Label
                [ #label
                    := "<span size=\"larger\" weight=\"bold\" color=\"#6b6d70\">|</span>"
                , #useMarkup := True
                ]
            #show sep'
            Gtk.toWidget sep'

        workspaces = workspacesNew defaultWorkspacesConfig
            { widgetGap            = 2
            , maxIcons             = Just 3
            , showWorkspaceFn      = (/=) "NSP" . workspaceName
            , urgentWorkspaceState = True
            }

        layout = do
            ctx   <- ask
            label <- new Gtk.Label []
            _     <- widgetSetClassGI label "layout-label"

            let callback _ = liftReader postGUIASync $ do
                    layout' <- runX11Def "" $ readAsString Nothing xLayoutProp
                    let markup = T.pack layout'
                    Gtk.labelSetMarkup label markup

            subscription <- subscribeToPropertyEvents [xLayoutProp] callback

            do
                ebox <- new
                    Gtk.EventBox
                    [ #tooltipMarkup
                          := "left click:\tnext layout\nmiddle click:\tdefault layout\nright click:\ttoggle float"
                    ]
                _ <- #add ebox label
                _ <- on ebox #buttonPressEvent $ dispatchButtonEvent ctx
                _ <- #showAll ebox
                _ <- on ebox #unrealize $ flip runReaderT ctx $ unsubscribe
                    subscription
                Gtk.toWidget ebox

        title = windowsNew defaultWindowsConfig
            { getMenuLabel   = truncatedGetMenuLabel 50
            , getActiveLabel = truncatedGetActiveLabel 50
            }

        clock =
            textClockNewWith defaultClockConfig { clockFormatString = "%R" }

        cpu = do
            cpuGraph <- pollingGraphNew
                myGraphConfig { graphDataColors = [green, magenta] }
                0.5
                cpuCallback
            buttonOverlay
                cpuGraph
                "cpu"
                (void $ spawnProcess
                    "/usr/bin/alacritty"
                    $ words "--class AlacrittyFloat,AlacrittyFloat -o window.dimensions.columns=200 -o window.dimensions.lines=40 -e htop --sort-key PERCENT_CPU"
                )

        mem = do
            memGraph <- pollingGraphNew
                myGraphConfig { graphDataColors = [blue] }
                1
                memCallback
            buttonOverlay
                memGraph
                "mem"
                (void $ spawnProcess
                    "/usr/bin/alacritty"
                    $ words "--class AlacrittyFloat,AlacrittyFloat -o window.dimensions.columns=200 -o window.dimensions.lines=40 -e htop --sort-key PERCENT_MEM"
                )

        network = do
            netGraph <- networkGraphNew
                myGraphConfig { graphDataColors = [yellow1, yellow2] }
                Nothing
            NetworkInfoChan chan <- getNetworkChan
            network'             <- buttonOverlay
                netGraph
                "net"
                (void $ spawnProcess "nm-connection-editor" [])
            _ <- channelWidgetNew network' chan $ \speedInfo ->
                let (up, down)    = sumSpeeds $ map snd speedInfo
                    tooltipString = showInfo
                        defaultNetFormat
                        3
                        (fromRational down, fromRational up)
                in  postGUIASync
                        $ set network' [#tooltipMarkup := tooltipString]
            return network'

        temp = pollingBarNew
            ((defaultBarConfig colorGradient)
                { barBorderColor     = (0.1, 0.1, 0.1)
                , barBackgroundColor = const (0.1, 0.1, 0.1)
                }
            )
            0.5
            readTemp

    simpleTaffybar defaultSimpleTaffyConfig
        { barHeight     = ScreenRatio $ 1 / 36
        , barPadding    = 6
        , widgetSpacing = 4
        , startWidgets  = [workspaces, sep, layout, sep, title]
        , centerWidgets = [clock]
        , endWidgets    = [sniTrayNew, sep, temp, network, mem, cpu]
        }
