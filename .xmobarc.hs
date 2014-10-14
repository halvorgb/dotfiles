Config { --font = "-*-dejavu sans mono-medium-r-*-*-20-*-*-*-*-*-*-*",
         font = "xft:Inconsolata:size=16:antialias=true"
       , bgColor = "#2A2A2A"
       , fgColor = "gray"
       , position = Top
       , lowerOnStart = True

       ,commands = [
          Run MultiCpu [] 10,
          Run Memory [] 10,
          Run BatteryP ["BAT0"]
                  ["-t", "<acstatus> (<left>%)",
                   "-L", "10", "-H", "80", "-p", "3",
                   "--", "-O", "<fc=green>On</fc> - ",
                   "-L", "-15", "-H", "-5",
                   "-l", "red", "-m", "blue", "-h", "green"]
                  600,

          Run Com "/home/halvor/.xmobar/getvolume.sh" [] "myVolume" 10,
          Run Com "/home/halvor/.xmobar/getcoretemp.sh" [] "coreTemp" 100,
          Run Com "/home/halvor/.xmobar/wireless.sh" [] "wifi" 30,
          Run Date "%a %_d. %b %H:%M" "date" 100,

          Run StdinReader

                   ]

       , sepChar = "%"

       , alignSep = "}{"

       , template = "%StdinReader% }{  %multicpu% %coreTemp% | %memory% | %battery% | Vol: %myVolume% | %date% |           "
--       , template = "%StdinReader% }{  %battery% | %date% |   "

       }
