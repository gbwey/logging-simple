let x = ./corelog.dhall
in {
     LogFile = Some { Prefix = "def", LongName = True, Level = x.debug, Dir = "testdir" }
   , Screen = Some { ScreenType = x.stdout, Level = x.info }
   , Email = Some { SmtpServer = "testsmtpserver"
                      , SmtpPort = Some 21
                      , SmtpTo = "testsmtpto"
                      , SmtpFrom = "testsmtpfrom"
                      }
   , Debug = True
}

