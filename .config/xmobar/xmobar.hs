import Xmobar

config :: Config
config =
    defaultConfig
        { font            = "FiraCode Nerd Font 20"
        , additionalFonts = [ "FiraCode Nerd Font Bold 20"   -- icon
                            , "FiraCode Nerd Bold 21"        -- sep
                            , "FiraCode Nerd Bold 18" ]      -- ws
        , alpha = 255
        , position = TopH 30
        -- , position = Static { xpos = 0, ypos = 1115, width = 1920, height = 35}
        , textOffset = 0
        , textOffsets = [0, 0, 0]
        , iconOffset = 0

        , commands = [ Run UnsafeStdinReader
                     , Run $ Com  "echo" ["<fn=1> \xf303 </fn>"] "arch" 360000

                     , Run $ Com  "echo" ["<fn=2><fc=#555555>\xe0b2</fc></fn>"] "left" 360000
                     , Run $ Com  "echo" ["<fn=2><fc=#555555>\xe0b0</fc></fn>"] "right" 360000

                     , Run $ Com  "echo" ["<fn=0> \xf287 </fn>"] "usb" 360000
                     , Run $ Com  "echo" ["<fn=0>\xf030 </fn>"] "cam" 360000

                     , Run $ Com  "/home/noutimbaler/.local/bin/statusbar/batinfo" [""] "bat" 50
                     , Run $ Com  "/home/noutimbaler/.local/bin/statusbar/internet-down" [""] "down" 40
                     , Run $ Com  "/home/noutimbaler/.local/bin/statusbar/int.sh" [""] "int" 5
                     , Run $ Com  "/home/noutimbaler/.local/bin/statusbar/vol.sh" [""] "vol" 5
                     , Run $ Date "\xf017 %2k:%M:%S" "date" 10
                     , Run $ Com  "/home/noutimbaler/.local/bin/statusbar/systraypad.sh" ["stalonetray"] "tray" 10

                     ]

        , sepChar = "%"
        , alignSep = "}{"


        , template = "<fc=#FFFFFF>%arch%</fc>\
                     \%left%<fn=3><fc=#FFFFFF,#555555> %UnsafeStdinReader%</fc></fn>%right%}\

                    \{<fn=0>\
                    \<fc=#FFFFFF,#555555>\

                    \%left%\
                        \<action='dmenuumount'>%usb% </action>\
                        \<action='screenshot.sh'>%cam% </action>\
                    \%right%\

                    \%left%\
                        \%down%\
                        \ <action='intr'>%int%</action> \
                    \%right%\

                    \%left%\
                        \%bat% \
                        \ <action='pavucontrol'>%vol%</action> \
                        \%date%\
                    \%right%\

                    \</fc>\
                        \%tray%\
                    \</fn>"

        }


main :: IO ()
main = xmobar config  

