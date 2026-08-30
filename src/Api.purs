-- | the console batch API installed at `globalThis.partyCarlo`.
-- | shapes crossing into JS are flat: strings, numbers, arrays and records only.
module PartyCarlo.Api where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Aff (error, throwError)
import PartyCarlo.Api.Csv as Csv
import PartyCarlo.Api.Error (ApiError(..))
import PartyCarlo.Api.Logs (ApiLog(..), log)
import PartyCarlo.Api.Types (Column, IntervalRow)
import PartyCarlo.Capability.Now (nowDateTime)
import PartyCarlo.Capability.Sleep (sleep)
import PartyCarlo.Core as Core
import PartyCarlo.Data.Display (display)
import PartyCarlo.Data.Result (Result)
import PartyCarlo.Env as Env
import PartyCarlo.ProdM (ProdM, runProdMAff)


type Api =
    { parse :: String -> Effect (Promise (Array Column))
    , run :: Fn2 (Array Column) Number (Effect (Promise (Array IntervalRow)))
    , toCsv :: Array IntervalRow -> String
    , runCsv :: Fn2 String Number (Effect (Promise String))
    , defaultExperiments :: Int
    }

foreign import installImpl :: Api -> Effect Unit

install :: Effect Unit
install = installImpl api

api :: Api
api =
    { parse: \csv -> runApi (parseM csv)
    , run: mkFn2 \cols n -> runApi (runM cols n)
    , toCsv: Csv.toCsv
    , runCsv: mkFn2 \csv n -> runApi (runCsvM csv n)
    , defaultExperiments: Core.experimentCount
    }

-- | run an api stage in ProdM, rejecting the promise with a native Error on failure
runApi :: forall a. ProdM (Either ApiError a) -> Effect (Promise a)
runApi m = Promise.fromAff do
    res <- runProdMAff { env: Env.env } m
    case res of
        Left e -> throwError (error (display e))
        Right a -> pure a

parseM :: String -> ProdM (Either ApiError (Array Column))
parseM csv = do
    log ParsingCsv
    case Csv.parseCsv csv of
        Left e -> do
            log (ApiParsingFailed e)
            pure (Left e)
        Right cols -> do
            log (ParsedGrid
                { labels: Array.length cols
                , people: maybe 0 (Array.length <<< _.probabilities) (Array.head cols)
                })
            pure (Right cols)

runM :: Array Column -> Number -> ProdM (Either ApiError (Array IntervalRow))
runM cols nRaw = case Csv.validateExperiments nRaw of
    Left e -> pure (Left e)
    -- every column is validated up front: a bad value in the last column should not cost the
    -- Monte Carlo runs of all the ones before it. the reported error is unchanged - still the
    -- first bad value in reading order.
    Right n -> case traverse prepare cols of
        Left e -> pure (Left e)
        Right prepared -> go n prepared 0 []
    where
    prepare col = { label: col.label, dist: _ } <$> Csv.validateColumn col

    go n prepared i acc = case Array.index prepared i of
        Nothing -> pure (Right acc)
        Just col -> do
            log (RunningLabel { label: col.label, index: i + 1, total: Array.length prepared, experiments: n })
            -- yield to the browser so progress is visible between labels
            sleep (Milliseconds 0.0)
            start <- nowDateTime
            Core.runExperiments n col.dist >>= case _ of
                Left _ -> do
                    log (RunFailedLog col.label)
                    pure (Left (RunFailed col.label))
                Right r -> do
                    end <- nowDateTime
                    log (LabelDuration col.label start end)
                    go n prepared (i + 1) (acc <> [toIntervalRow col.label r])

runCsvM :: String -> Number -> ProdM (Either ApiError String)
runCsvM csv n = parseM csv >>= either (pure <<< Left) (\cols -> map (map Csv.toCsv) (runM cols n))

toIntervalRow :: String -> Result -> IntervalRow
toIntervalRow label r =
    { label
    , p90_low: fst r.p90
    , p90_high: snd r.p90
    , p95_low: fst r.p95
    , p95_high: snd r.p95
    , p99_low: fst r.p99
    , p99_high: snd r.p99
    , p999_low: fst r.p999
    , p999_high: snd r.p999
    }
