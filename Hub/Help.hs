module Hub.Help(help) where

help :: String
help = unlines
    [ "      Hub Help Page"
    , "  "
    , "      --usage   is an aliase for the 'usage'   command"
    , "      --help    is an aliase for the 'help'    command"
    , "      --version is an aliase for the 'version' command"
    , ""
    , "hub usage"
    , ""
    , "      Lists the syntax of all the hub commands."
    , ""
    , "hub help    [<hub-command>]"
    , ""
    , "      Lists the help for a command or all commands if none specified."
    , ""
    , "      See \"hub usage\" for a command-syntax summary."
    , ""
    , "hub version"
    , ""
    , "      Lists the version information."
    , ""
    , "hub default [<g-hub>|-]"
    , ""
    , "      (Needs to be run as root.)"
    , "    "
    , "      If no arguments are given then this command lists the  the default global"
    , "      hub for the system (i.e., the default global hub used to set up each"
    , "      user's 'home' hub)."
    , "       "
    , "      If a global hub <g-hub> is specified then <g-hub> will become the"
    , "      default global hub."
    , "      "
    , "      If a '-' is specified then any older default settings are discarded and"
    , "      the system default re-established."
    , ""
    , "hub ls"
    , ""
    , "      Lists your user hubs and all of the global hubs."
    , ""
    , "hub set     [<hub>|-]"
    , ""
    , "      Sets the 'current' hub for a directory and its sub-directories."
    , "      "
    , "      The HUB environment variable can be set to a hub name to override this"
    , "      setting."
    , ""
    , "hub info    [<hub>]"
    , ""
    , "      Lists the vital stats for the named or current hub."
    , "      "
    , "      (See 'hub set' on how to set the current hub.)"
    , ""
    , "hub name"
    , ""
    , "      Lists the name of the current hub."
    , "    "
    , "      (See 'hub set' on how to set the current hub.)"
    , ""
    , "hub path    [<hub>]"
    , ""
    , "      Lists the  path of the XML file defining the named or current hub."
    , "    "
    , "      (See 'hub set' on how to set the current hub.)"
    , ""
    , "hub xml     [<hub>]"
    , ""
    , "      Lists the contents of the XML file defining the named or current hub."
    , "    "
    , "      (See 'hub set' on how to set the current hub.)"
    , ""
    , "hub init    [<hub>]   <u-hub'>"
    , ""
    , "      Creates an empty user hub, <u-hub'>. The new hub inherits the global"
    , "      hub of <hub> (or the current hub)."
    , "    "
    , "      (See 'hub set' on how to set the current hub.)"
    , ""
    , "hub cp      [<u-hub>] <u-hub'>"
    , ""
    , "      Duplicates the user <u-hub> (or the current hib) in <u-hub'>."
    , "    "
    , "      (See 'hub set' on how to set the current hub.)"
    , ""
    , "hub mv      [<u-hub>] <u-hub'>"
    , ""
    , "      Renames user hub <u-hub> (or the current hub) to <u-hub'>."
    , "    "
    , "      (See 'hub set' on how to set the current hub.)"
    , ""
    , "hub rm       <u-hub>"
    , ""
    , "      Deletes user hub <hub>."
    , ""
    , "hub swap    [<u-hub>] <u-hub'>"
    , ""
    , "      Swaps the contents of user hub <u-hub> (or the current hub) with"
    , "      user hub <u-hub'>."
    , "    "
    , "      (See 'hub set' on how to set the current hub.)"
    ]
