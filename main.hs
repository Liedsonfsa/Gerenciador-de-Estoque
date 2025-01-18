module Main where

import Menu

import Produto

menu :: [Produto] -> IO ()
menu estoque = do
    mostrarMenu
    opcao <- lerInt
    if opcao == 6 then
        return ()
    else do
        novoEstoque <- executarOpcaoEscolhida estoque opcao

main :: IO ()
main = do
    let estoqueInicial = []
    menu estoqueInicial

-- aparentemente, vai ser esse rolê todo pra poder rodar
-- ghc -c Produto.hs
-- ghc -c Estoque.hs
-- ghc -c Menu.hs
-- ghc Main.hs Produto.o Estoque.o Menu.o -o estoque
-- ./estoque