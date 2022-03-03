module RenameFields
  where

mapEntryFieldRename :: String -> String
mapEntryFieldRename "block_type" = "type"
mapEntryFieldRename name = name

stateFieldRename :: String -> String
stateFieldRename "turns_remaining" = "turns-remaining"
stateFieldRename name = name
