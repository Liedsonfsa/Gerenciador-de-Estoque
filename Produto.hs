module Produto (
    Produto(..)
) where

-- Definição do tipo
data Produto = Produto {
    nome :: String,
    quantidade :: Int,
    preco :: Double
} deriving (Show)
