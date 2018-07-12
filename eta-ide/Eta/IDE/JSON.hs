{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Eta.IDE.JSON where

import Eta.Main.GHC (TyThing)
import Eta.BasicTypes.ConLike (ConLike)
import Eta.BasicTypes.DataCon (
  DataCon, DataConRep, StrictnessMark, HsImplBang, HsSrcBang, SrcUnpackedness,
  SrcStrictness)
import Eta.BasicTypes.MkId (DataConBoxer)
import Eta.BasicTypes.Name (Name)
import Eta.BasicTypes.PatSyn (PatSyn)
import Eta.BasicTypes.IdInfo (IdInfo, IdDetails, TickBoxOp)
import Eta.BasicTypes.Unique (Unique)
import Eta.BasicTypes.Var (Var)
import Eta.BasicTypes.BasicTypes (RecFlag)
import Eta.BasicTypes.Module (Module, ModuleName, UnitId)
import Eta.BasicTypes.BasicTypes (InlinePragma)
import Eta.Types.TyCon (TyCon, TyConParent, AlgTyConRhs)
import Eta.Types.TypeRep (Type, TyLit)
import Eta.BasicTypes.SrcLoc
import Eta.Types.CoAxiom (Branched, Unbranched, CoAxiom, CoAxiomRule, Role)
import Eta.Types.Coercion (Coercion, LeftOrRight)
import Eta.Utils.FastString
import Eta.Prelude.PrimOp (PrimOp)
import Eta.Types.Class (Class, ClassMinimalDef)
import Eta.Prelude.ForeignCall (ForeignCall, CCallSpec, Safety, CCallTarget, CCallConv, CType)
import Eta.Core.CoreSyn (Unfolding, AltCon)

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Text as T

$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyThing)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Var)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyCon)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Unique)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataCon)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataConRep)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''StrictnessMark)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsImplBang)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataConBoxer)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Coercion)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Type)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyLit)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''LeftOrRight)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CoAxiomRule)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CoAxiom)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ConLike)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Name)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RecFlag)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IdInfo)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyConParent)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''HsSrcBang)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IdDetails)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''AlgTyConRhs)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''PrimOp)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ForeignCall)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SrcUnpackedness)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TickBoxOp)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CCallSpec)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Safety)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CCallTarget)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Module)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SrcStrictness)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Class)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Unfolding)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''AltCon)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CCallConv)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''UnitId)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ModuleName)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ClassMinimalDef)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InlinePragma)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''PatSyn)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Role)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CType)
-- $(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IdScope)
-- $(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''NameSort)

instance ToJSON FastString where
  toJSON str = toJSON $ unpackFS str

instance FromJSON FastString where
  parseJSON = withText "FastString" $ pure . mkFastString . T.unpack


-- TODO: Do we actually need these? They are phantom types, but the TH complains without them.
instance ToJSON   Branched   where toJSON    = undefined
instance FromJSON Branched   where parseJSON = undefined
instance ToJSON   Unbranched where toJSON    = undefined
instance FromJSON Unbranched where parseJSON = undefined

instance ToJSON RealSrcSpan where
  toJSON x = object
    [ "file" .= srcSpanFile x
    , "span" .=
        [ [srcSpanStartLine x, srcSpanStartCol x]
        , [srcSpanEndLine x, srcSpanEndCol x]
        ]
    ]

instance FromJSON RealSrcSpan where
  parseJSON v = withObject "RealSrcSpan" f v
    where
    mkSpan file sL sC eL eC = mkRealSrcSpan (mkRealSrcLoc file sL sC) (mkRealSrcLoc file eL eC)
    f o = do
      file <- o .: "file"
      span <- o .: "span"
      case span of
        [[sL, sC], [eL, eC]] -> pure $ mkSpan file sL sC eL eC
        _                    -> typeMismatch "span should be a 2x2 array" v

instance ToJSON SrcSpan where
  toJSON srcSpan = case srcSpan of
    UnhelpfulSpan s -> object ["unhelpful" .= toJSON s]
    RealSrcSpan x -> toJSON x

instance FromJSON SrcSpan where
  parseJSON v =
        (UnhelpfulSpan <$> parseJSON v)
    <|> (RealSrcSpan <$> parseJSON v)
