module Sugar where

import qualified Data.Text as T

data Sugar
  = Identifier T.Text
  | List [Sugar]
  | Number Integer
  | String T.Text
  deriving Show
