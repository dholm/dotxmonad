Config { font = "xft:Monospace:size=9:bold:antialias=true"
       , bgColor = "#002b36"
       , fgColor = "#839496"
       , position = Static { xpos = 0
                           , ypos = 0
                           , width = 1290
                           , height = 16
                           }
       , commands = [ Run MultiCpu [ "-t", "Cpu: <total>%", "-L", "3", "-H", "50", "--normal", "#93a1a1", "--high", "#dc322f" ] 10
                    , Run Memory [ "-t", "Mem: <usedratio>%", "--normal", "#93a1a1", "--high", "#dc322f" ] 10
                    , Run Swap [ "-t", "Swap: <usedratio>%", "-H", "1024", "-L", "512", "--normal", "#93a1a1", "--high", "#dc322f" ] 10
                    , Run Network "eth0" [ "-L", "0", "-H", "70", "--normal", "#93a1a1", "--high", "#dc322f" ] 10
                    , Run Network "wlan0" [ "-L", "0", "-H", "32", "--normal", "#93a1a1", "--high", "#dc322f" ] 10
                    , Run Com "uname" [ "-s", "-r" ] "" 36000
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run Battery [ "-L", "50", "-H", "75", "--high", "#839496", "--normal", "#93a1a1", "--low", "#dc322f" ] 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %multicpu% | %memory% | %swap% | %eth0% | %wlan0% | <fc=#cb4b16>%date%</fc> | %battery% "
       }
