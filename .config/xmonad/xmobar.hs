Config {
   -- appearance
   font = "xft:San Francisco Display:pixelsize=22:hinting=true"
   , additionalFonts = []
   , bgColor =  "#073642"
   , fgColor =  "#839496"
   , alpha =    255
	 , position =  TopW L 95
   , border =   BottomB
   , borderColor = "#646464"

	 -- layout
   , sepChar =  "%"
   , alignSep = "}{"
   , template = "%StdinReader% }{ %multicpu% | %coretemp% | %batt% | %memory% | %wlp3s0% | %volume% | %brightness% | %date%"

	 -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
	 , persistent =       True		-- enable/disable hiding (True = disabled)
   , commands =
        [
				-- Display Brightness status
				Run Com "sh" ["/home/caninodev/.xmonad/backlight.sh"] "brightness" 10

				-- Display volume status
				, Run Com "sh" ["/home/caninodev/.xmonad/volume.sh"] "volume" 10

        -- network monitor
        ,	Run Wireless "wlp3s0"       [ "--template" , "<essid> <quality>"
                                      , "--Low"      , "50"       -- units: kB/s
                                      , "--High"     , "75"       -- units: kB/s
                                      , "--low"      , "#2aa198"
                                      , "--normal"   , "#cb4b16"
                                      , "--high"     , "#dc322f"
                                    ] 10

        -- cpu activity monitor
        , Run MultiCpu       [ "--template" , "Cpu: <total0>%<total1>%"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "#2aa198"
														 , "--normal"   , "#cb4b16"
                             , "--high"     , "#dc322f"
                             ] 10

        -- cpu core temperature monitor
        , Run CoreTemp       [ "--template" , "Temp: <core0>°C|<core1>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , "#2aa198"
                             , "--normal"   , "#cb4b16"
                             , "--high"     , "#dc322f"
                             ] 50

        -- memory usage monitor
        , Run Memory         [ "--template" ,"Mem: <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#2aa198"
                             , "--normal"   , "#cb4b16"
                             , "--high"     , "#dc322f"
                             ] 10

        -- battery monitor
        , Run Battery        [ "--template" , " Batt: <acstatus>"
                             , "--Low"      , "15"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#dc322f"
                             , "--normal"   , "#cb4b16"
                             , "--high"     , "#2aa198"

                             , "--" -- battery specific option
														 -- discharging status
                             , "-o"	        , "<left>% (<timeleft>)"
                             -- AC "on" status
                             , "-O"	        , "<fc=#dAA520>Charging</fc> <left>%"
                             -- charged status
                             , "-i"	        , "<fc=#006000>Charged</fc>"
                             ] 50
        -- stdin reader
        , Run StdinReader

        -- datetime indicator
        , Run Date           "<fc=#ABABAB>%F (%a) %T</fc>" "date" 10

				-- Weather
				, Run Weather "KSFO" [ "-t", "<station>: <tempF>F"
				                     ] 3600
        ]
   }
