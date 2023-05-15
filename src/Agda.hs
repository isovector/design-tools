{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Agda where

import           Control.Monad
import           Data.Foldable (asum)
import           Data.List
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Pandoc
import           Utils
import Combinators
import Data.Maybe (fromMaybe)

data AgdaCmd
  = AgdaCmdAuto
  | AgdaCmdLoad
  | AgdaCmdNormalise
  | AgdaCmdMakeCase
  | AgdaCmdSolve
  | AgdaCmdRefine
  | AgdaCmdGotoDefinition
  | AgdaCmdTypeContext
  | AgdaCmdElaborate
  | AgdaCmdDescribeChar
  deriving (Eq, Ord, Show, Enum, Bounded)

parseAgdaCmd :: Text -> Maybe AgdaCmd
parseAgdaCmd "Load" = Just AgdaCmdLoad
parseAgdaCmd "Auto" = Just AgdaCmdAuto
parseAgdaCmd "Normalise" = Just AgdaCmdNormalise
parseAgdaCmd "Normalize" = Just AgdaCmdNormalise
parseAgdaCmd "MakeCase" = Just AgdaCmdMakeCase
parseAgdaCmd "Solve" = Just AgdaCmdSolve
parseAgdaCmd "Refine" = Just AgdaCmdRefine
parseAgdaCmd "GotoDefinition" = Just AgdaCmdGotoDefinition
parseAgdaCmd "TypeContext" = Just AgdaCmdTypeContext
parseAgdaCmd "Elaborate" = Just AgdaCmdElaborate
parseAgdaCmd "DescribeChar" = Just AgdaCmdDescribeChar
parseAgdaCmd _ = Nothing


data AgdaMode
  = AgdaModeNormalise
  deriving (Eq, Ord, Show, Enum, Bounded)

parseAgdaMode :: Text -> Maybe AgdaMode
parseAgdaMode "Normalise" = Just AgdaModeNormalise
parseAgdaMode "Normalize" = Just AgdaModeNormalise
parseAgdaMode _ = Nothing


data AgdaCmdInvocation = AgdaCmdInvocation
  { ac_cmd :: AgdaCmd
  , ac_arg :: Maybe Text
  , ac_mode :: Maybe AgdaMode
  }
  deriving (Eq, Ord, Show)

parseAgdaCmdInvoc :: Text -> Maybe AgdaCmdInvocation
parseAgdaCmdInvoc (T.split (\c -> c == ':' || c == '/') -> args) =
  case args of
    [tcmd] -> AgdaCmdInvocation <$> parseAgdaCmd tcmd <*> pure Nothing <*> pure Nothing
    [tcmd, targ] -> do
      cmd <- fmap AgdaCmdInvocation $ parseAgdaCmd tcmd
      asum
        [ do
            mode <- parseAgdaMode targ
            pure $ cmd Nothing $ Just mode
        , pure $ cmd (Just targ) Nothing
        ]
    [tcmd, targ, tmode] -> do
      cmd <- parseAgdaCmd tcmd
      mode <- parseAgdaMode tmode
      pure $ AgdaCmdInvocation cmd (Just targ) $ Just mode
    _ -> Nothing

data Key
  = Key Char
  | Ctrl Key
  | MetaK Key
  deriving (Eq, Ord, Show)

instance IsString Key where
  fromString [c] = Key c

showKey :: Key -> Text
showKey (Key c) = T.pack [c]
showKey (Ctrl k) = "C-" <> showKey k
showKey (MetaK k) = "M-" <> showKey k



emacsKeysFor :: AgdaCmd -> [Key]
emacsKeysFor AgdaCmdLoad           = fmap Ctrl ["c", "l"]
emacsKeysFor AgdaCmdAuto           = fmap Ctrl ["c", "a"]
emacsKeysFor AgdaCmdNormalise      = fmap Ctrl ["c", "n"]
emacsKeysFor AgdaCmdMakeCase       = fmap Ctrl ["c", "c"]
emacsKeysFor AgdaCmdSolve          = fmap Ctrl ["c", "s"]
emacsKeysFor AgdaCmdRefine         = fmap Ctrl ["c", "r"]
emacsKeysFor AgdaCmdGotoDefinition = [MetaK "."]
emacsKeysFor AgdaCmdTypeContext    = fmap Ctrl ["c", ","]
emacsKeysFor AgdaCmdElaborate      = fmap Ctrl ["c", "m"]
emacsKeysFor AgdaCmdDescribeChar   = [Ctrl "u", Ctrl "x", "="]

emacsModFor :: AgdaMode -> [Key]
emacsModFor AgdaModeNormalise = fmap Ctrl ["u", "u"]

emacsAllKeys :: AgdaCmdInvocation -> Text
emacsAllKeys c
  = T.unwords
  $ fmap showKey
  $ mconcat
    [ fromMaybe mempty (fmap emacsModFor (ac_mode c))
    , emacsKeysFor (ac_cmd c)
    ]

agdaCmdInvocToInline :: Format -> AgdaCmdInvocation -> Inline
agdaCmdInvocToInline f cmd = Span ("", [], []) $
  [ mkInline f "AgdaCmd" $ T.pack $ drop 7 $ show $ ac_cmd cmd
  , mkInline f "AgdaEmacsInput" $ emacsAllKeys cmd
  ] <>
  [ mkInline f "AgdaCmdArg" arg
  | Just arg <- [ac_arg cmd]
  ]


agdaCmd :: Format -> Inline -> Inline
agdaCmd format = \case
  Link _ (Strs (parseAgdaCmdInvoc -> Just cmd)) ("AgdaCmd", _)
    -> agdaCmdInvocToInline format cmd
  t -> t

