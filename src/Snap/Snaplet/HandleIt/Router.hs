module Snap.Snaplet.HandleIt.Router where

import Control.Monad.State(State, runState, put, get, MonadState)
import Snap.Snaplet.HandleIt.Header(Handling(..), HDL(..), Restful(..))

type Routing  = [(Restful, HDL)]
type Router a = State Routing a
type Routes   = Router ()

routing :: Routes -> Routing
routing a = snd $ runState a []

handleIt :: (Restful, HDL) -> Routes
handleIt a = get >>= put . (a:)

resources :: Handling s => s -> Router [()]
resources a = mapM (setSingle a)
              [ RIndex  , RShow
              , RNew    , REdit
              , RCreate , RUpdate
              , RDelete ]

setSingle  :: Handling s => s -> Restful -> Routes
setSingle a r = handleIt (r,  HDL a)
