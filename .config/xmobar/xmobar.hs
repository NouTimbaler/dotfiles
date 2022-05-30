
Config { font		= "xft:FiraCode Nerd Font:pixelsize=17:antialias=true:hinting=true"
       , additionalFonts = [ "xft:FiraCode Nerd Font:bold:pixelsize=30:antialias=true:hinting=true"
                           , "xft:FiraCode Nerd Font:bold:pixelsize=20:antialias=true:hinting=true"
                           , "xft:FiraCode Nerd Font:bold:pixelsize=17:antialias=true:hinting=true" ]


       , alpha = 255
      -- , position = Top
       , position = Static { xpos = 1685, ypos = 5, width = 1910, height = 30}
       , textOffset = -1
       , textOffsets = [-1, 21, -1]
       , iconOffset = -1


       , allDesktops = True

       , commands = [ Run UnsafeStdinReader
                    , Run Com  "echo" ["<fn=1> \xf303  </fn>"] "arch" 360000
                    , Run Com  "/home/noutimbaler/.local/bin/layout.sh" [""] "lay" 5

                    , Run Com  "echo" ["<fn=2><fc=#555555>\xe0b2</fc></fn>"] "left" 360000
                    , Run Com  "echo" ["<fn=2><fc=#555555>\xe0b0</fc></fn>"] "right" 360000

                    , Run Com  "echo" ["<fn=0> \xf287 </fn>"] "usb" 360000
                    , Run Com  "echo" ["<fn=0>\xf030 </fn>"] "cam" 360000

                    , Run Com  "/home/noutimbaler/.local/bin/statusbar/batinfo" [""] "bat" 50
                    , Run Com  "/home/noutimbaler/.local/bin/statusbar/internet-down" [""] "down" 40
                    , Run Com  "/home/noutimbaler/.local/bin/statusbar/int.sh" [""] "int" 5
                    , Run Com  "/home/noutimbaler/.local/bin/statusbar/vol.sh" [""] "vol" 5
                    , Run Date "\xf017 %2k:%M:%S" "date" 10
                    , Run Com  "/home/noutimbaler/.local/bin/statusbar/systraypad.sh" ["stalonetray"] "tray" 10
                    ]

       , sepChar = "%"
       , alignSep = "}{"


        , template = "<fc=#FFFFFF>%arch%</fc>\

                      \<fn=3><fc=#FFFFFF,#555555> %UnsafeStdinReader%</fc></fn>}\


                      \{<fn=0>\

                      \%left%\
                      \<fc=#FFFFFF,#555555>\
                      \<action='dmenuumount'>%usb% </action>\
                      \<action='screenshot.sh'>%cam% </action>\
                      \</fc>\
                      \%right%\

                      \%left%\
                      \<fc=#FFFFFF,#555555>\
                      \%bat%\
                      \%down%\
                      \<action='intr'>%int%</action> \
                      \ <action='pavucontrol'>%vol%</action> \
                      \ %date% \
                      \</fc>\
                      \%right%\

                      \ %tray% \

                      \ </fn>"

       }
