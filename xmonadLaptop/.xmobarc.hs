Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
                        
       ,commands = [ Run MultiCpu [] 10
                     --, Run CoreTemp ["-t", "C C", "-L", "40", "-H", "60", "-l", "lightblue", "-n", "white", "-h", "red"] 50

                   , Run Memory [] 10

                   , Run Swap [] 10

                   , Run Network "wlan0" [] 10

--                   , Run Battery ["-t","Battery: %","-L","25","-H","75","--low","#FF0000","--normal","#F9FF00","--high","#00FF00"] 10
                   ,Run BatteryP ["BAT0"]
                    ["-t", "<acstatus><watts> (<left>%)",
                     "-L", "10", "-H", "80", "-p", "3",
                     "--", "-O", "<fc=green>On</fc> - ",
                     "-L", "-15", "-H", "-5",
                     "-l", "red", "-m", "blue", "-h", "green"] 600

                   , Run Com "/home/halvor/.xmobar/getvolume.sh" [] "myVolume" 10
                   , Run Com "/home/halvor/.xmobar/getcoretemp.sh" [] "coreTemp" 10                     
                   , Run Com "/home/halvor/.xmobar/wireless.sh" [] "wifi" 30
                   , Run Date "%a %b %_d %I:%M %p" "date" 10

                   , Run StdinReader

                   ]

       , sepChar = "%"

       , alignSep = "}{"

       , template = "%StdinReader% }{ %multicpu% | Temp: %coreTemp% | %memory% %swap% | %wlan0% - %wifi% | %battery% | Vol: %myVolume% | %date%"

       }

