module Errors.ImporterError where

-- An error that is a result of an error in the parac compiler logic
newtype ImporterError = ImporterError {message :: String}
  deriving (Eq, Ord)

instance Show ImporterError where
  show (ImporterError message) = "Importer Error: " ++ message
