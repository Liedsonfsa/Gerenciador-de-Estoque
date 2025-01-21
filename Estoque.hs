-- aqui a gente deixa as funções do estoque
-- funções básicas (não sei se vão ter mais)
module Estoque (
    adicionarProduto,
    -- removerProduto,
    procurarProduto,
    formatarProduto,
    atualizarQuantidade,
    imprimirProdutosNoEstoque
) where

import Produto
import Data.List (find)

-- Adiciona um produto ao estoque
adicionarProduto :: [Produto] -> Produto -> [Produto]
adicionarProduto estoque novoProduto =
    if any (\p -> nome p == nome novoProduto) estoque
        then map (\p -> if nome p == nome novoProduto 
                        then p { quantidade = quantidade p + quantidade novoProduto }
                        else p) estoque
        else novoProduto : estoque

-- Procura um produto no estoque.
procurarProduto :: [Produto] -> String -> IO () -- Assinatura da função
procurarProduto estoque nomeProduto =
    maybe (putStrLn "Produto não encontrado.") -- Se o resultado da busca for `Nothing`, imprime "Produto não encontrado."
          (\produto -> putStrLn $ "Produto encontrado: " ++ formatarProduto produto) -- Se encontrar o produto, imprime uma mensagem formatada com os detalhes do produto.
          (find (\p -> nome p == nomeProduto) estoque) -- Usa `find` para buscar um produto dentro da lista `estoque`.

-- Função auxiliar para formatar um único produto.
formatarProduto :: Produto -> String
formatarProduto p = "Nome: " ++ nome p ++ ", Quantidade: " ++ show (quantidade p) ++ ", Preço: " ++ show (preco p)

imprimirProdutosNoEstoque :: [Produto] -> IO ()
imprimirProdutosNoEstoque estoque = do
    putStrLn "Estoque Atual:"
    mapM_ (\p -> putStrLn $ "Nome: " ++ nome p ++ ", Quantidade: " ++ show (quantidade p) ++ ", Preço: " ++ show (preco p)) estoque


-- Atualiza a quantidade do estoque.
atualizarQuantidade :: [Produto] -> String -> Int -> [Produto]
atualizarQuantidade estoque nomeProduto novaQuantidade =
    -- Verifica se existe algum produto no estoque com o nome fornecido.
    if any (\p -> nome p == nomeProduto) estoque
        -- Se o produto for encontrado, utiliza map para atualizar a quantidade.
        then map (\p -> 
                    -- Para cada produto p, verifica se o nome do produto corresponde ao fornecido.
                    if nome p == nomeProduto
                        -- Se o nome corresponder, retorna o produto com a quantidade atualizada.
                        then p { quantidade = novaQuantidade }
                        -- Caso contrário, retorna o produto original, sem alterações.
                        else p) estoque
        -- Caso nenhum produto com o nome fornecido seja encontrado.
        else error "Produto não encontrado no estoque."
