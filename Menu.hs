-- aqui a gente vai deixar o menu e opções
module Menu (
    mostrarMenu,
    lerOpcao,
    executarOpcaoEscolhida
) where

import Estoque

import Produto

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
lerString :: String
lerString = do
    getLine

-- | lerInt vai ler um valor inteiro
lerInt :: IO Int
lerInt = do
    readLn :: IO Int

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
        let novoProduto = Produto nomeProduto quantidadeProduto precoProduto

        -- implementar a função de adicionar produto
        
        putStrLn "Produto adicionado com sucesso!"
        return novoEstoque
    2 -> do
        putStrLn "Digite o nome do produto para remover: "
        nomeProduto <- lerString
        
        -- implementar a remoção do produto
        
        putStrLn "Produto removido com sucesso!"
        return novoEstoque
    3 -> do
        putStrLn "Digite o nome do produto para consultar: "
        nomeProduto <- lerString

        -- implementar a busca pelo produto
        
        return estoque
    4 -> do
        putStrLn "Digite o nome do produto para atualizar a quantidade: "
        nomeProduto <- lerString
        putStrLn "Digite a nova quantidade do produto: "
        novaQuantidade <- lerInt

        -- implementar a atualização do estoque

        putStrLn "Quantidade atualizada com sucesso!"
        return novoEstoque
    5 -> do
        -- implementar a função de imprimir estoque
        return estoque
    6 -> do
        putStrLn "Saindo do sistema..."
        return estoque
    _ -> do
        putStrLn "Opção inválida. Por favor, escolha novamente."
        return estoque