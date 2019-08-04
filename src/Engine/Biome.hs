module Engine.Biome
  ( getBiome
  ) where
 
import Linear.V2
import Numeric.Noise.Perlin


import Engine.Types

bNoiseLattice :: Double
bNoiseLattice = 4.0

bNoiseShift :: Double
bNoiseShift = 2**10

getBiome :: NoiseRandom -> HorizontalPos -> Int
getBiome (NoiseRandom cs co csc cp) (V2 x y) = if (f > 0) then 1 else 2
  where
    f = noiseValue perlinNoise ((fromIntegral $ x)/bNoiseLattice + bNoiseShift, (fromIntegral $ y)/bNoiseLattice + bNoiseShift, 0.0)
    perlinNoise = perlin cs co csc cp
  
