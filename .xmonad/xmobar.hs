Config { font = "xft:Monospace:size=9:bold:antialias=true"
       , bgColor = "#000000"
       , fgColor = "grey"
       , position = Static { xpos = 0 , ypos = 0, width = 1290, height = 16 }
       , commands = [ Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Network "eth0" ["-L","0","-H","70","--normal","green","--high","red"] 10
                    , Run Network "wlan0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    , Run Battery ["-L","50","-H","75","--high","green","--normal","yellow", "--low", "red"] 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %memory% | %eth0% | %wlan0% | <fc=#ee9a00>%date%</fc> | %battery% "
       }
