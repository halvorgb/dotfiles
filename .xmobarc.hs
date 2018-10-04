Config { font = "xft:Inconsolata:size=11:bold:antialias=true"
       , bgColor = "#2A2A2A"
       , fgColor = "gray"
       , position = Top
       , lowerOnStart = True

       , commands = [ Run MultiCpu [] 10
                    , Run Memory [] 10
                    , Run BatteryP ["BAT0"]
                      ["-t", "<acstatus> (<left>%)",
                       "-L", "10", "-H", "80", "-p", "3",
                       "--", "-O", "<fc=green>On</fc> - ",
                       "-L", "-15", "-H", "-5",
                       "-l", "red", "-m", "blue", "-h", "green"]
                      600

                    , Run Date "%a %_d. %b %H:%M" "date" 100

                    , Run StdinReader

                    ]

       , sepChar = "%"

       , alignSep = "}{"

       , template = "%StdinReader% }{  %multicpu% | %memory% | %battery% | %date% |           "

       }
