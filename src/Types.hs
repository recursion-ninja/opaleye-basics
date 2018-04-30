{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Tables where

import           Control.Arrow
import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye (Column, Nullable, matchNullable, isNull,
                          Table, table, tableColumn, queryTable,
                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                          (.===),
                          (.++), ifThenElse, pgString, aggregate, groupBy,
                          count, avg, sum, leftJoin, runQuery,
                          showSqlForPostgres, Unpackspec,
                          PGInt2, PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)
import           Prelude hiding (sum)


type Sector = Maybe String


data Issuer' a b  = Issuer { issuerName :: a, sector :: b }
type Issuer       = Issuer' String Sector
type IssuerColumn = Issuer' (Column PGText) (Column (Nullable PGText))


data Manager' a    = Manager { managerName :: a }
type Manager       = Manager' String
type ManagerColumn = Manager' (Column PGText)


data Security' a b  = Security { securityName :: a, securityType :: b }
type Security       = Security' String SecurityType
type SecurityColumn = Security' (Column PGText) (Column PGBool)
-- TODO: Figure out how to make this a sum type and play nice with Opaleye
type SecurityType = Bool -- True = Stock, False = Option.


data  QuarterlyRecord' a b c d e f
    = QuarterlyRecord
    { manager     :: a
    , securtiy    :: b
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
        { securityName = tableColumn "securityName"
        , securityType = tableColumn "securityType"
        }
    )


quarterlyRecordTable :: Table QuarterlyRecordColumn QuarterlyRecordColumn
quarterlyRecordTable = table "securityTable"
    ( pQuarterlyRecord QuarterlyRecord
        { manager     = tableColumn "manager"
        , securtiy    = tableColumn "security"
        , year        = tableColumn "year"
        , quarter     = tableColumn "quarter"
        , quantity    = tableColumn "quantity"
        , marketValue = tableColumn "marketValue"
        }
    )
