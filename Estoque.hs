-- aqui a gente deixa as funções do estoque
-- funções básicas (não sei se vão ter mais)
module Estoque (
    adicionarProduto,
    removerProduto,
    procurarProduto,
    atualizarQuantidade,
    imprimirProdutosNoEstoque
) where

-- Adiciona um produto ao estoque
adicionarProduto :: [Produto] -> Produto -> [Produto]
adicionarProduto estoque novoProduto =
    if any (\p -> nome p == nome novoProduto) estoque
        then map (\p -> if nome p == nome novoProduto 
                        then p { quantidade = quantidade p + quantidade novoProduto }
                        else p) estoque
        else novoProduto : estoque
