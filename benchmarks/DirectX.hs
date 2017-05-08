import Criterion.Main
import qualified Data.ByteString.Char8 as B

import Engine.Mesh
import Data.DirectX

main :: IO ()
main = do
  let parseTemplates = runParser $ directX False
  templateContents <- B.readFile "data/xtemplates/templates.x"
  templates <- xTemplates <$> either fail return (parseTemplates templateContents)
  let parseData = runParser $ directX' True templates
  dataContents <- B.readFile "data/xobjects/lzom.x"

  defaultMain [ bench "templates.x" $ whnf parseTemplates templateContents
              , bench "lzom.x" $ whnf parseData dataContents
              ]
