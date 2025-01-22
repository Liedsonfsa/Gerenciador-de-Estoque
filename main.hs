module Main where

import Menu ( mostrarMenu, executarOpcaoEscolhida, lerProdutos, lerInt )

import Estoque

import Produto ( Produto )

-- | menu representa o loop do menu
menu :: [Produto] -> IO ()
menu estoque = loop estoque
  where
    loop estoque = do
        mostrarMenu
        opcao <- lerInt
        novoEstoque <- executarOpcaoEscolhida estoque opcao
        if opcao == 6 then
            return ()
        else do
            loop novoEstoque 


main :: IO ()
main = do
    estoqueInicial <- lerProdutos "estoque.txt"
    menu estoqueInicial
