module RunJoinListEditor where

import JoinList
import Sized
import Exercise3
import Buffer
import Exercise4()
import Editor

initialBuffer :: JoinList (Score, Size) String
initialBuffer = fromString "This is the initial editor buffer\nNothing to see here...\nHonestly"

joinListEditorMain :: IO ()
joinListEditorMain = runEditor editor initialBuffer
