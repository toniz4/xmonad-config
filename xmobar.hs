Config { font = "xft:tewi:pixelsize=9"
       , additionalFonts = []
       , borderColor = "#444444"
       , border = NoBorder
       , bgColor = "#E1FAFF"
       , fgColor = "#5a5a4c"
       , alpha = 255
       , position = Static { xpos = 0 , ypos = 0, width = 1366, height = 13 }
       , textOffset = 10
       , iconOffset = 1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Date " %Y %b %d (%a)  %H:%M " "date" 30
                    , Run Alsa "default" "Master"
                      [  "-t", "<status> <volume>%"
                       , "--"
                       , "-c", "#000000"
                       , "-C", "#000000"
                       , "-O", ""
                       , "-o", ""
                       , "-h", ""
                       , "-m", ""
                       , "-l", "" ]
                    , Run BatteryP ["BAT0"]
                      [ "-t", " <left>%<leftipat>"  
                         , "--"
                         , "--on-icon-pattern", ""
                         , "--off-icon-pattern", ""
                         , "--idle-icon-pattern", ""
                         , "-A", "10"
                         , "-a", "notify-send -u critical 'Battery running out!!'"
                         , "-l", "#c6414d"] 100
                    , Run UnsafeStdinReader
                    , Run CoreTemp [   "-t", " <core0>C"
                                     , "-L", "40", "-H", "83"
                                     , "-h", "#c6414d" ] 70
                    , Run Com "internet" [] "internet" 100
                    , Run PipeReader " :/tmp/cmus-pipe" "barmusic"  
                    , Run PipeReader " :/tmp/capture-pipe" "barrec"
        ]
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%UnsafeStdinReader% }\
                    \<action=`toggleDunst` button=1>%date%</action>\
                    \%barrec% {\
                    \<action=`cmus-remote -u` button=1>\
                    \<action=`nowplaying`     button=3>\
                    \<action=`cmus-remote -n` button=4>\
                    \<action=`cmus-remote -r` button=5>\
                        \%barmusic% </action></action></action></action>\
                    \%internet% %coretemp% %battery% \
                    \<action=`vol mute` button=1>\
                    \<action=`st -t float -e alsamixer` button=3>\
                    \<action=`vol up 1` button=4>\
                    \<action=`vol down 1` button=5>\
                        \%alsa:default:Master%</action></action></action></action> "
                      }
