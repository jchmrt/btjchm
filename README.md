btjchm
======

My very own IRC bot

Build with:
```
$ cabal configure
$ cabal build
```

Most of the important data types are in Core.hs, so if
you want to understand the code you should probably start there. 
Currently every other module imports Core.hs.

Main.hs is mostly code to just tie the ends together. Save.hs contains
some helper functions to save the messages the bot should remember.
The tellAll function in Tell.hs is used to determine which messages
the bot should deliver and to delete those messages from the list. In
TimedActions.hs we have some code to determine which scheduled
actions should be executed and to update the list of timed
actions. And lastly we have all the incoming messages from the server
being parsed in Parsers.hs

List of commands:

| Command    | Description                                               |
|------------|-----------------------------------------------------------|
| !tell      | Sends a message to a user when they return                |
| !afk       | Marks a user Away From Keyboard                           |
| !back      | Marks a user back                                         |
| !where     | Checks where a user is (AFK or Back)                      |
| !choose    | Chooses one of the given options                          |
| !answer    | (kindof)                                                  |
| !remind    | Reminds a user of something after a given amount of time  |
| !waitforit | stupid reference joke command                             |
| !whatsnew  | New functions in the bot                                  |
| !say       | Make the bot send a message                               |
| !rejoin    | Rejoins the channel                                       |
| !ascii     | Turns a string into ASCII-art                             |
| !ok        | Prints "OK" in ASCII-art                                  |
| !pls       | Prints "PLS" in ASCII-art                                 |
| n1         | Prints "N1" in ASCII-art                                  |
| !lenny     | Prints a lenny face:  ( ͡° ͜ʖ ͡°)                            |
| !tableflip | Prints a table flip: (╯°□°）╯︵ ┻━┻                       |
