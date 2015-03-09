import Graphics.Vty.Widgets.All

import qualified Data.Text as T

main :: IO ()
main = do
    e <- editWidget
    ui <- centered e
    fg <- newFocusGroup
    _ <- addToFocusGroup fg e

    c <- newCollection
    _ <- addToCollection c ui fg

    e `onActivate` \this ->
        getEditText this >>= (error . ("You entered: "++) . T.unpack)

    runUi c defaultContext
