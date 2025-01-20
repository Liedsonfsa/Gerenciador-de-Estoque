-- aqui a gente deixa as funções do estoque
-- funções básicas (não sei se vão ter mais)
module Estoque (
    adicionarProduto,
    -- removerProduto,
    -- procurarProduto,
    -- atualizarQuantidade,
    imprimirProdutosNoEstoque
) where

import Produto

-- Adiciona um produto ao estoque
adicionarProduto :: [Produto] -> Produto -> [Produto]
adicionarProduto estoque novoProduto =
    if any (\p -> nome p == nome novoProduto) estoque
        then map (\p -> if nome p == nome novoProduto 
                        then p { quantidade = quantidade p + quantidade novoProduto }
                        else p) estoque
        else novoProduto : estoque


imprimirProdutosNoEstoque :: [Produto] -> IO ()
imprimirProdutosNoEstoque estoque = do
    putStrLn "Estoque Atual:"
    mapM_ (\p -> putStrLn $ "Nome: " ++ nome p ++ ", Quantidade: " ++ show (quantidade p) ++ ", Preço: " ++ show (preco p)) estoque
