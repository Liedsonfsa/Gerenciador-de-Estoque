-- aqui a gente deixa a definição do tipo produto
module Produto (
    Produto(..)
) where

-- não sei se vão ter mais campos no tipo de dado
data Produto = Produto {
    nome :: String,
    quantidade :: Int,
    preco :: Double
} deriving (Show)
