module PartyCarlo.Components.HTML.Graph where

import Prelude

import Data.Array (cons, length, uncons, zip)
import Data.Foldable (foldr)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, uncurry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import PartyCarlo.Data.Display (display)
import PartyCarlo.Data.Result (Interval(..), Result)
import PartyCarlo.Data.SortedArray (SortedArray)
import PartyCarlo.Data.SortedArray as SortedArray


graph :: ∀ i action. (Interval -> action) -> Result -> HTML i action
graph f result = 
  let
    heights' = Int.toNumber <<< fst <$> (group result.dist)
    maxHeight = 250.0
    maxWidth = 700.0
    barCount = length heights
    barWidth = (maxWidth / Int.toNumber barCount) + 1.0
    factor = maxHeight / arrayMax 0.0 heights'
    heights = (factor * _) <$> heights'
    xoffsets = iterate (barCount) (_ + maxWidth / Int.toNumber barCount) 0.0
  in
    HH.div [ HP.class_ (H.ClassName "chart-div") ]
      [ SE.svg 
        [ SA.class_ (H.ClassName "chart"), SA.viewBox 0.0 0.0 maxWidth maxHeight, SA.preserveAspectRatio Nothing SA.Meet ]
        (append 
          (uncurry (barWithHeight barWidth maxHeight) <$> zip heights xoffsets)
          (riemannBars maxHeight barWidth result.showBars heights))
      , HH.div [ HP.class_ (H.ClassName "hcontainer") ] 
          [ HH.div
          [ HP.class_ (H.ClassName "p")
          , HP.id "p90" 
          , HE.onMouseOver (\_ -> f P90)]
          [ HH.text $ "p90\n" <> display result.p90 ] 
          , HH.div
          [ HP.class_ (H.ClassName "p")
          , HP.id "p95" 
          , HE.onMouseOver (\_ -> f P95) ]
          [ HH.text $ "p95\n" <> display result.p95 ] 
          , HH.div
          [ HP.class_ (H.ClassName "p")
          , HP.id "p99" 
          , HE.onMouseOver (\_ -> f P99) ]
          [ HH.text $ "p99\n" <> display result.p99 ] 
          , HH.div
          [ HP.class_ (H.ClassName "p")
          , HP.id "p999" 
          , HE.onMouseOver (\_ -> f P999) ]
          [ HH.text $ "p99.9\n" <> display result.p999 ] ] ]

riemannBars :: ∀ a b. Number -> Number -> Maybe Interval -> Array Number -> Array (HTML a b)
riemannBars h barWidth interval dist = let
    total = foldr (+) 0.0 dist
  in
    case interval of
      Nothing -> []
      Just P90 -> [ riemannBar (barWidth * (riemannBound (total * 0.1) dist)) h
                  , riemannBar (barWidth * (riemannBound (total - (total * 0.1))) dist) h ]
      Just P95 -> [ riemannBar (barWidth * (riemannBound (total * 0.05) dist)) h
                  , riemannBar (barWidth * (riemannBound (total - (total * 0.05)) dist)) h ]
      Just P99 -> [ riemannBar (barWidth * (riemannBound (total * 0.01) dist)) h
                  , riemannBar (barWidth * (riemannBound (total - (total * 0.01)) dist)) h ]
      Just P999 -> [ riemannBar (barWidth * (riemannBound (total * 0.001) dist)) h
                   , riemannBar (barWidth * (riemannBound (total - (total * 0.001)) dist)) h ]

riemannBar :: ∀ a b. Number -> Number -> HTML a b
riemannBar x h =  SE.rect [SA.class_ (H.ClassName "marker"), SA.x x, SA.y 0.0, SA.width 4.0, SA.height h]

barWithHeight :: ∀ a b. Number -> Number -> Number -> Number -> HTML a b
barWithHeight barWidth maxHeight barHeight xoffset = 
  SE.rect [SA.class_ (H.ClassName "bar"), SA.x xoffset, SA.y (maxHeight - barHeight), SA.width barWidth, SA.height barHeight]

group :: ∀ a. Eq a => SortedArray a -> Array (Tuple Int a)
group xs = foldr foo [] (SortedArray.toArray xs) where
  foo x ys = case uncons ys of
    Nothing -> [(Tuple 1 x)]
    Just { head: (Tuple i x'), tail: t } -> if x == x' 
                                            then cons (Tuple (i + 1) x') t 
                                            else cons (Tuple 1 x) ys

iterate :: ∀ a. Int -> (a -> a) -> a -> Array a
iterate count _ _ | count <= 0 = []
iterate count f x = cons x' (iterate (count - 1) f x')
  where x' = f x

arrayMax :: ∀ a. Ord a => a -> Array a -> a
arrayMax = foldr max

riemannBound :: Number -> Array Number -> Number
riemannBound x xs = case uncons xs of
  Nothing -> 0.0
  Just { head: h, tail: t } -> if h < x 
                               then 1.0 + riemannBound (x - h) t 
                               else x / h
