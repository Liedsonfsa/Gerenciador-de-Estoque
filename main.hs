module Main where

import Menu ( mostrarMenu, executarOpcaoEscolhida, lerProdutos )

import Estoque

import Produto ( Produto )

-- | menu representa 
menu :: [Produto] -> IO ()
menu estoque = loop estoque
  where
    loop estoque = do
        mostrarMenu
        opcao <- readLn :: IO Int
        if opcao == 6 then do
            novoEstoque <- executarOpcaoEscolhida estoque opcao
            return ()
        else do
            novoEstoque <- executarOpcaoEscolhida estoque opcao
            loop novoEstoque 


main :: IO ()
main = do
    estoqueInicial <- lerProdutos "estoque.txt"
    menu estoqueInicial

-- aparentemente, vai ser esse rolê todo pra poder rodar
-- ghc -c Produto.hs
-- ghc -c Estoque.hs
-- ghc -c Menu.hs
-- ghc main.hs Produto.o Estoque.o Menu.o -o estoque
-- ./estoque