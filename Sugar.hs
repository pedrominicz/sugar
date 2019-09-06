module Sugar where

import qualified Data.Map as M

type Context = M.Map String Sugar

data Sugar
  = Identifier String
  | List [Sugar]
  | Number Integer
  | String String
  deriving Show
