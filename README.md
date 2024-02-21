# rio-log-effectful

Logger like RIO for Effectful

## Examples

```haskell
import RIO (stderr)
import Effectful
import Effectful.Logger.Static

main :: IO ()
main = do
  let isVerbose = True
  logOptions <- logOptionsHandle stderr isVerbose
  runEff $ runLog logOptions $ do
    logDebug "Debug message"
    logInfo "Info message"
    logWarn "Warn message"
    logError "Error message"
```