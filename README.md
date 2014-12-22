btjchm
======

My very own IRC bot

Build with:
```
$ cabal configure
$ cabal build
```

Most of the important data types are currently in Parsers.hs, so if
you want to understand the code you should probably start there. This
should probably be changed, since now every other module has to import 
Parsers.

Main.hs is mostly code to just tie the ends together. Save.hs contains
some helper functions to save the messages the bot should remember.
The tellAll function in Tell.hs is used to determine which messages
the bot should deliver and to delete those messages from the list. In
TimedActions.hs we have some code to determine which scheduled
actions should be executed and to update the list of timed
actions. And lastly we have all the incoming messages from the server
being parsed in Parsers.hs
