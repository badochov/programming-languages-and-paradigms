import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe

type Name = String

type Value = String

type Env = Map.Map Name Value

type Eval a = ReaderT Env (ExceptT String Identity) a

main = do
  print "Siema"