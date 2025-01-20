module Main where

import Menu

import Produto

menu :: [Produto] -> IO ()
menu estoque = loop estoque
  where
    loop estoque = do
        mostrarMenu
        opcao <- readLn :: IO Int
        if opcao == 6 then
            return ()
        else do
            novoEstoque <- executarOpcaoEscolhida estoque opcao
            loop novoEstoque 


main :: IO ()
main = do
    let estoqueInicial = []
    menu estoqueInicial

-- aparentemente, vai ser esse rolê todo pra poder rodar
-- ghc -c Produto.hs
-- ghc -c Estoque.hs
-- ghc -c Menu.hs
-- ghc main.hs Produto.o Estoque.o Menu.o -o estoque
-- ./estoque