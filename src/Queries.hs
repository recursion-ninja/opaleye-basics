{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Queries where

import           Control.Arrow
import           Data.Profunctor.Product (p2, p3, p5)
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
import           Types


queryOne
  :: Query
      (QuarterlyRecord'
        (Column PGText)
        (Column PGText)
        (Column PGDate)
        (Column PGInt2)
        (Column PGFloat8)
        (Column PGFloat8)
      )
queryOne = proc () -> do
    manRec   <- managerTotals  -< ()
    secRec   <- securityTotals -< ()
    restrict -< eqRecord manRec secRec
    returnA  -< manRec { quantity = marketValue manRec / marketValue secRec }
  where
    eqRecord x y = security x .== security y
               .&& year     x .== year     y
               .&& quarter  x .== quarter  y
    
    managerTotals = aggregate
      ( pQuarterlyRecord
          QuarterlyRecord
            { manager     = groupBy
            , security    = groupBy
            , year        = groupBy
            , quarter     = groupBy
            , quantity    = sum
            , marketValue = sum
            }
      )
      (queryTable quarterlyRecordTable)

    securityTotals = aggregate
      ( pQuarterlyRecord
          QuarterlyRecord
            { manager     = pure ()
            , security    = groupBy
            , year        = groupBy
            , quarter     = groupBy
            , quantity    = sum
            , marketValue = sum
            }
      )
      (queryTable quarterlyRecordTable)


--queryTwo :: Query ()
queryTwo = aggregate
      (p5 (groupBy, groupBy, groupBy, sum, groupBy))
      subQuery
  where
    subQuery = proc () -> do
        issuerRec    <- sectorNormalizer <$> queryTable issuerTable -< ()
        securityRec  <- queryTable securityTable -< ()
        restrict     -< issuer securityRec .== issuerName issuerRec
        quarterlyRec <- queryTable quarterlyRecordTable -< ()
        restrict     -< security quarterlyRec .== securityName securityRec
        returnA      -< ( manager quarterlyRec
                        , year quarterlyRec
                        , quarter quarterlyRec
                        , marketValue quarterlyRec
                        , sector issuerRec
                        )
      where
        sectorNormalizer :: IssuerColumn -> Issuer' (Column PGText) (Column PGText)
        sectorNormalizer record = record { sector = sectorName }
          where
            sectorName = matchNullable (pgString "unknown sector") id (sector record)
