import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
type Eval = ReaderT [Int] (ExceptT String (StateT Int Identity)) Int

runEval :: [Int] -> Int -> Eval -> (Either String Int, Int)
runEval env st ev = runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

eval :: Eval
eval = do
   l <- ask
   case l of
     [] -> get
     h : t -> do
         modify (h ^)
         local (const t) eval

test :: (Either String Int, Int)
test = runEval [1, 2, 3] 2 eval
