-- aqui a gente vai deixar o menu e opções
module Menu (
    mostrarMenu,
    executarOpcaoEscolhida
) where

import Estoque
import Produto
import System.IO

-- | mostrarMenu mostra o menu de opções que o usuário pode escolher
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n-- Menu de Opções --"
    putStrLn "1 - Adicionar Produto"
    putStrLn "2 - Remover Produto"
    putStrLn "3 - Consultar Produto"
    putStrLn "4 - Atualizar Quantidade de Produto"
    putStrLn "5 - Imprimir Estoque"
    putStrLn "6 - Sair"

-- | lerString vai ler qualquer string informada pelo usuário
lerString :: IO String
lerString = getLine

-- | lerInt vai ler um valor inteiro
lerInt :: IO Int
lerInt = do
    readLn :: IO Int

salvarInformacoes :: [Produto] -> IO ()
salvarInformacoes estoque = do
    let produtos = unlines [nome p ++ "," ++ show (quantidade p) ++ "," ++ show (preco p) | p <- estoque]
    writeFile "estoque.txt" produtos

    putStrLn "Produtos salvos"

-- executarOpcaoEscolhida vai executar a opção escolhida pelo usuário
executarOpcaoEscolhida :: [Produto] -> Int -> IO [Produto]
executarOpcaoEscolhida estoque opcao = case opcao of 
    1 -> do
        putStrLn "Digite o nome do produto: "
        nomeProduto <- lerString
        putStrLn "Digite a quantidade do produto: "
        quantidade <- lerInt
        putStrLn "Digite o preço do produto: "
        precoProduto <- readLn :: IO Double
        let novoProduto = Produto nomeProduto quantidade precoProduto
        let novoEstoque = adicionarProduto estoque novoProduto
        
        putStrLn "Produto adicionado com sucesso!"
        return novoEstoque
    2 -> do
        putStrLn "Digite o nome do produto para remover: "
        nomeProduto <- lerString
        
        -- implementar a remoção do produto
        
        putStrLn "Produto removido com sucesso!"
        -- quando implementar a função, troque o return estoque por return novoEstoque
        -- return novoEstoque
        return estoque
    3 -> do
        putStrLn "Digite o nome do produto para consultar: "
        nomeProduto <- lerString

        -- Chama a função procurarProduto para exibir o resultado
        procurarProduto estoque nomeProduto
        
        return estoque
    4 -> do
        putStrLn "Digite o nome do produto para atualizar a quantidade: "
        nomeProduto <- lerString
        putStrLn "Digite a nova quantidade do produto: "
        novaQuantidade <- lerInt

        -- implementar a atualização do estoque

        putStrLn "Quantidade atualizada com sucesso!"
        -- quando implementar a função, troque o return estoque por return novoEstoque
        -- return novoEstoque
        return estoque
    5 -> do
        -- implementar a função de imprimir estoque
        imprimirProdutosNoEstoque estoque
        return estoque
    6 -> do
        salvarInformacoes estoque
        putStrLn "Saindo do sistema..."
        return estoque
    _ -> do
        putStrLn "Opção inválida. Por favor, escolha novamente."
        return estoque