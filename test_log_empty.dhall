let x = ./corelog.dhall
in {
     LogFile = x.nofile
   , Screen = x.noscreen
   , Email = x.noemail
   , Debug = False
}

