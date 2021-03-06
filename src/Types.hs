{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time.Calendar (Day)
import Opaleye ( Column, Nullable, Table, table, tableColumn
               , PGInt2, PGInt8, PGText, PGDate, PGFloat8, PGBool
               )
import Prelude hiding (sum)


type Sector = Maybe String


data Issuer' a b  = Issuer { issuerName :: a, sector :: b }
type Issuer       = Issuer' String Sector
type IssuerColumn = Issuer' (Column PGText) (Column (Nullable PGText))


data Manager' a    = Manager { managerName :: a }
type Manager       = Manager' String
type ManagerColumn = Manager' (Column PGText)


data Security' a b c = Security { securityName :: a, securityType :: b, issuer :: c }
type Security        = Security' String SecurityType String
type SecurityColumn  = Security' (Column PGText) (Column PGBool) (Column PGText)
-- TODO: Figure out how to make this a sum type and play nice with Opaleye
type SecurityType = Bool -- True = Stock, False = Option.


data  QuarterlyRecord' a b c d e f
    = QuarterlyRecord
    { manager     :: a
    , security    :: b
    , year        :: c
    , quarter     :: d
    , quantity    :: e
    , marketValue :: f
    }
type QuarterlyRecord       = QuarterlyRecord' String String Day Int Int Double
type QuarterlyRecordColumn = QuarterlyRecord'
                               (Column PGText)
                               (Column PGText)
                               (Column PGDate)
                               (Column PGInt2)
                               (Column PGInt8)
                               (Column PGFloat8)


$(makeAdaptorAndInstance "pIssuer"          ''Issuer')
$(makeAdaptorAndInstance "pManager"         ''Manager')
$(makeAdaptorAndInstance "pSecurity"        ''Security')
$(makeAdaptorAndInstance "pQuarterlyRecord" ''QuarterlyRecord')


sectorTable :: Table (Column PGText)
                     (Column PGText)
sectorTable = table "sectorTable"
                (tableColumn "sectorName")


issuerTable :: Table IssuerColumn IssuerColumn
issuerTable = table "issuerTable"
    ( pIssuer Issuer
        { issuerName = tableColumn "issuerName"
        , sector     = tableColumn "sector" }
    )


managerTable :: Table ManagerColumn ManagerColumn
managerTable = table "managerTable"
    ( pManager Manager { managerName = tableColumn "managerName"} )


securityTable :: Table SecurityColumn SecurityColumn
securityTable = table "securityTable"
    ( pSecurity Security
        { securityName   = tableColumn "securityName"
        , securityType   = tableColumn "securityType"
        , issuer         = tableColumn "issuer"
        }
    )


quarterlyRecordTable :: Table QuarterlyRecordColumn QuarterlyRecordColumn
quarterlyRecordTable = table "securityTable"
    ( pQuarterlyRecord QuarterlyRecord
        { manager     = tableColumn "manager"
        , security    = tableColumn "security"
        , year        = tableColumn "year"
        , quarter     = tableColumn "quarter"
        , quantity    = tableColumn "quantity"
        , marketValue = tableColumn "marketValue"
        }
    )
