{-# LANGUAGE OverloadedStrings #-}
module Ascii ( toAscii
             , messageOk
             , messagePls
             , messageN1  ) where

import Core

messageOk :: [IRCAction]
messageOk = 
  [ PrivMsg "/\\ |/"
  , PrivMsg "\\/ |\\"] 

messagePls :: [IRCAction]
messagePls = 
  [ PrivMsg " _     __"
  , PrivMsg "|_| | |__"
  , PrivMsg "|   |_ __|"]

messageN1 :: [IRCAction]
messageN1 = 
  [ PrivMsg "|\\  |  /|"
  , PrivMsg "| \\ |   |"
  , PrivMsg "|  \\|  _|_"]

toAscii :: Char -> [String]
toAscii 'a' = [ "     "
              , " /\\  "
              , "/--\\ "]
toAscii 'b' = [ " _  "
              , "|_) "
              , "|_) "]
toAscii 'c' = [ " __ "
              , "|   "
              , "|__ "]
toAscii 'd' = [ "__  "
              , "| \\ "
              , "|_/ "]
toAscii 'e' = [ " __ "
              , "|__ "
              , "|__ "]
toAscii 'f' = [ " __ "
              , "|__ "
              , "|   "]
toAscii 'g' = [ " __ "
              , "/   "
              , "\\_] "]
toAscii 'h' = [ "    "
              , "|_| "
              , "| | "]
toAscii 'i' = [ " . "
              , " | "
              , " | "]
toAscii 'j' = [ " .  "
              , " |  "
              , "_/  "]
toAscii 'k' = [ "   "
              , "|/ "
              , "|\\ "]
toAscii 'l' = [ "    "
              , "|   "
              , "|__ "]
toAscii 'm' = [ " _    _  "
              , "| \\  / | "
              , "|  \\/  | "]
toAscii 'n' = [ " _    "
              , "| \\ | "
              , "|  \\| "]
toAscii 'o' = [ " _  "
              , "/ \\ "
              , "\\_/ "]
toAscii 'p' = [ " _  "
              , "|_| "
              , "|   "]
toAscii 'q' = [ " _  "
              , "|_| "
              , "  | "]
toAscii 'r' = [ " _  "
              , "|_| "
              , "|\\  "]
toAscii 's' = [ " __  "
              , "|__  "
              , " __| "]
toAscii 't' = [ "___ "
              , " |  "
              , " |  "]
toAscii 'u' = [ "     "
              , "|  | "
              , "|__| "]
toAscii 'v' = [ "    "
              , "\\ / "
              , " V  "]
toAscii 'w' = [ "         "
              , "\\  /\\  / "
              , " \\/  \\/  "]
toAscii 'x' = [ "    "
              , " \\/ "
              , " /\\ "]
toAscii 'y' = [ "    "
              , " \\/ "
              , " /  "]
toAscii 'z' = [ "__ "
              , " / "
              , "/_ "]
toAscii ' ' = [ "  ", "  ", "  "]
toAscii '.' = [ "  ", "  ", ". "]
toAscii '!' = [ " |  "
              , " |  "
              , " .  "]
toAscii '?' = [ " /\\ "
              , "  / "
              , "  . "]
toAscii '1' = [ "    "
              , "  | "
              , "  | "]
toAscii '2' = [ " _  "
              , " _| "
              , "|_  "]
toAscii '3' = [ " _  "
              , " _| "
              , " _| "]
toAscii '4' = [ "    "
              , "|_| "
              , "  | "]
toAscii '5' = [ " _  "
              , "|_  "
              , " _| "]
toAscii '6' = [ " _  "
              , "|_  "
              , "|_| "]
toAscii '7' = [ " _  "
              , "  | "
              , "  | "]
toAscii '8' = [ " _  "
              , "|_| "
              , "|_| "]
toAscii '9' = [ " _  "
              , "|_| "
              , " _| "]
toAscii '0' = [ " _  "
              , "| | "
              , "|_| "]
toAscii '+' = [ "    "
              , "_|_ "
              , " |  "]
toAscii '-' = [ "    "
              , "___ "
              , "    "]
toAscii '_' = [ "    "
              , "    "
              , "___ "]
toAscii '=' = [ "    "
              , "--- "
              , "--- "]
toAscii '(' = [ " / "
              , "|  "
              , " \\ "]
toAscii ')' = [ " \\ "
              , "  |"
              , " / "]
toAscii '%' = [ " O / "
              , "  /  "
              , " / O "]
toAscii '\"' = [ "|| "
              , "   "
              , "   "]
toAscii '\'' = [ "| "
              , "  "
              , "  "]
toAscii '>' = [ "    "
              , " \\  "
              , " /  "]
toAscii '<' = [ "    "
              , " /  "
              , " \\  "]
toAscii '/' = [ "  / "
              , " /  "
              , "/   "]
toAscii '|' = [ " | "
              , " | "
              , " | "]
--toAscii '\\' = [ "\   "
--              , " \  "
--              , "  \\ "]
toAscii ':' = [ "   "
              , " . "
              , " . "]
toAscii ';' = [ "   "
              , " . "
              , " , "]
toAscii _   = [ "", "", ""]
