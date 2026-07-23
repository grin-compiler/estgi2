module ThreadScheduler

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
runScheduler : List Atom -> ScheduleReason -> M (List Atom)
runScheduler result sr = do
  tid <- gets ssCurrentThreadId
  case sr of
    {-
    SR_ThreadFinished -> do
      -- set thread status to finished
      ts <- getThreadState tid
      updateThreadState tid ts {tsStatus = ThreadFinished}
      mylog $ show tid ++ " ** SR_ThreadFinished"
      yield result
    -}
    SR_ThreadFinishedMain => do
      -- set thread status to finished
      ts <- getThreadState tid
      updateThreadState tid $ {tsStatus := ThreadFinished, tsCurrentResult := result} ts
      pure result

    SR_ThreadFinishedFFICallback => do
      -- set thread status to finished
      ts <- getThreadState tid
      updateThreadState tid $ {tsStatus := ThreadFinished, tsStack := [], tsCurrentResult := result} ts
      mylog $ show tid ++ " ** SR_ThreadFinishedFFICallback"
      pure result
    {-
    SR_ThreadBlocked  -> yield result

    SR_ThreadYield    -> yield result
    -}
    x => stg_error $ "runScheduler TODO: " ++ show x
