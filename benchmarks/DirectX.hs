import Criterion.Main
import qualified Data.ByteString.Char8 as B

import Engine.Mesh
import Data.DirectX

main :: IO ()
main = do
  templateContents <- B.readFile "data/xtemplates/templates.x"
  let parseTemplates = runParser $ directX False
  defaultMain [ bench "templates.x" $ whnf parseTemplates templateContents ]
  templates <- xTemplates <$> either fail return (parseTemplates templateContents)

  dataContents <- B.readFile "data/xobjects/lzom.x"
  let parseData = runParser $ directX' True templates
  defaultMain [ bench "lzom.x" $ whnf parseData dataContents ]
