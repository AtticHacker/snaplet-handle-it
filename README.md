snaplet-handle-it
=================

This is a snaplet for managing your Handlers in a Restful manner.

(In development)

Simply add this snaplet to your App:

Application.hs
```
data App = App {
     _handleIt :: Snaplet (HandleIt Routing)
}
```

Site.hs
```
import Routes

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h  <- nestSnaplet "" heist $ heistInit "templates"
    hi <- nestSnaplet "" handleIt $ handleItInit routes
    return $ App h hi
```


Next we create a (for example) user handler


Handler/User.hs
```
module Handler.User where

data User = User
instance Handling User
```

And the Routing file

``` Routes.hs

module Routes where
import Handler.User

routes = do
    resources User

```

These will generate restful routes accordingly. The Index, Show, New and Edit pages will use the following templates:

- Index : heist/templates/user/index.tpl
- Show  : heist/templates/user/show.tpl
- New   : heist/templates/user/new.tpl
- Edit  : heist/templates/user/edit.tpl

We can also add functionality to our handlers


Handler/User.hs
```
module Handler.User where

data User = User
instance Handling User where
    indexH User = liftIO $ putStrLn "Hello Index"
```

Whenever you visit the user index page, "Hello Index" will be printed in the console.

More functionality coming soon.
