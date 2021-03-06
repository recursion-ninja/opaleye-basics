{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Queries where

import Control.Arrow
import Data.Profunctor.Product (p5)
import Opaleye ( Column, Query, matchNullable, queryTable, restrict, (.==), (.&&)
               , pgString, aggregate, groupBy, sum, PGInt2, PGText, PGDate, PGFloat8)
import Prelude hiding (sum)
import Types


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


queryTwo :: Query (Column PGText, Column PGDate, Column PGInt2, Column PGFloat8, Column PGText)
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
