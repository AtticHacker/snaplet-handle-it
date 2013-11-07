module Test.Snap.Snaplet.HandleIt.Router where

import Test.HUnit
import Snap.Snaplet.HandleIt.Router
import Snap.Snaplet.HandleIt.Header
-- Environment

data User    = User    deriving (Show, Eq)
data Item = Item deriving (Show, Eq)

instance Handling User
instance Handling Item where
    handleName Item = "it"

routes :: Router ()
routes = do
    resources User
    setSingle Item IndexR
    setSingle Item ShowR


twoSingleRoutes :: Routing
twoSingleRoutes = routing $ do
    setSingle User IndexR
    setSingle Item ShowR

-- / Environment

-- Tests

routingSpec :: [Test]
routingSpec =
    [ "7 items" ~:
      (length . routing $ resources User) ~=? 7
    , "Only one item" ~:
      (length . routing $ setSingle User IndexR) ~=? 1
    , "2 items" ~:
      (length . routing $ do
            setSingle User IndexR
            setSingle User ShowR) ~=? 2
    , "2 items with 2 handlers" ~:
      length twoSingleRoutes ~=? 2
    , "Two different singles are not the same" ~:
      map fst twoSingleRoutes ~=? [ShowR, IndexR]
    , "Expecting not to match" ~:
      map fst twoSingleRoutes == [IndexR, ShowR] ~=? False
    ]


restfulToURLSpec :: [Test]
restfulToURLSpec =
    [ "Index user url"   ~: restfulToURL IndexR User   ~=? "/user"
    , "Show user url"    ~: restfulToURL ShowR User    ~=? "/user/:id"
    , "New user url"     ~: restfulToURL NewR User     ~=? "/user/new"
    , "Edit user url"    ~: restfulToURL EditR User    ~=? "/user/:id/edit"
    , "Create user url"  ~: restfulToURL CreateR User  ~=? "/user/create"
    , "Destroy user url" ~: restfulToURL DestroyR User ~=? "/user/:id/destroy"

    , "Index item url"   ~: restfulToURL IndexR Item   ~=? "/it"
    , "Show item url"    ~: restfulToURL ShowR Item    ~=? "/it/:id"
    , "New item url"     ~: restfulToURL NewR Item     ~=? "/it/new"
    , "Edit item url"    ~: restfulToURL EditR Item    ~=? "/it/:id/edit"
    , "Create item url"  ~: restfulToURL CreateR Item  ~=? "/it/create"
    , "Destroy item url" ~: restfulToURL DestroyR Item ~=? "/it/:id/destroy"
    ]


tests :: [Test]
tests = concat [ routingSpec
               , restfulToURLSpec
               ]