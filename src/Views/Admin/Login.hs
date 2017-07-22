{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Admin.Login(
  render
)where

import Control.Monad
import qualified Data.Text.Lazy as LT
import Data.String

import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Utils.BlazeExtra.Attributes as EA
import Text.Blaze.Html.Renderer.Text

import qualified Views.Admin.Layout as VL

render :: LT.Text
render =
    VL.renderLogin [renderForm]
  where
    renderForm =
      H.form ! A.class_ "ui large form" ! A.action "/admin/login" ! A.method "POST" $
        H.div ! A.class_ "ui stacked segment" $ do
          H.div ! A.class_ "field" $
            H.div ! A.class_ "ui left icon input" $ do
              H.i ! A.class_ "lock icon" $ ""
              H.input ! A.type_ "password" ! A.name "password"
          H.button ! A.class_ "ui fluid large teal submit button"
            ! A.type_ "submit" $ "登录"
