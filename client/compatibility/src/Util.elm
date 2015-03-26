module Util where
import Time
import Text
import Signal
import Mouse

after : Time.Time -> Signal.Signal a -> Signal.Signal Time.Time
after t reset =
  let
    fpsRate = Time.second / t
    time = Signal.map Just (Time.fps fpsRate)
    zero = Signal.map (always Nothing) reset
    combine update since = case update of
      Nothing -> 0
      Just elapsed -> elapsed + since
  in Signal.foldp combine 0 (Signal.merge time zero)