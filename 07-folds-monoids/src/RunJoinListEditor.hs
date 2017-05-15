module RunJoinListEditor where

import JoinList
import Sized
import Exercise3
import Exercise4()
import Editor

joinListEditorMain :: IO ()
joinListEditorMain = runEditor editor (Single (Score 3, Size 1) "Rat")
