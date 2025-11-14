module Document
  ( Document (..),
  )
where

import qualified Data.Text as T

data Document = Document
  { text :: T.Text,
    name :: T.Text
  }
