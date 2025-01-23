module Menu (
    mostrarMenu,
    executarOpcaoEscolhida,
    lerProduto,
    lerProdutos,
    lerInt,
    lerDouble,
    lerString
) where

import Estoque
    ( adicionarProduto,
      removerProduto,
      procurarProduto,
      imprimirProdutosNoEstoque,
      atualizarQuantidade )
import Produto ( Produto(..) )
import System.IO ()
import System.Directory (doesFileExist)
import qualified Data.Text as T

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
lerString = do
    input <- getLine
    if null input
        then do
            putStrLn "Entrada inválida. A string não pode estar vazia. Tente novamente."
            lerString
        else return input

-- | lerInt vai ler um valor inteiro
lerInt :: IO Int
lerInt = do
    input <- getLine
    case reads input :: [(Int, String)] of
        [(n, "")] -> return n
        _ -> do
            putStrLn "Por favor, informe um número inteiro."
            lerInt


-- | lerDouble vai ler um valor double
lerDouble :: IO Double
lerDouble = do
    input <- getLine
    case reads input :: [(Double, String)] of
        [(n, "")] -> return n
        _ -> do
            putStrLn "Por favor, informe somente números."
            lerDouble

-- | lerProdutos lê os produtos que estão no arquivo estoque.txt
lerProdutos :: FilePath -> IO [Produto]
lerProdutos arquivo = do
    existe <- doesFileExist arquivo
    if not existe
        then do
            writeFile arquivo ""
            return []
        else map lerProduto . lines <$> readFile arquivo

-- | lerProduto lê a linha as informações da linha em que o produto está usando Data.Text.
lerProduto :: String -> Produto
lerProduto linha =
    let [nome, quantidade, preco] = map T.unpack $ T.splitOn (T.pack ",") (T.pack linha)
    in Produto nome (read quantidade) (read preco)

-- | salvarInformacoes
salvarInformacoes :: [Produto] -> IO ()
salvarInformacoes estoque = do
    let produtos = unlines [nome p ++ "," ++ show (quantidade p) ++ "," ++ show (preco p) | p <- estoque]
    writeFile "estoque.txt" produtos

    putStrLn "Produtos salvos!"

-- | executarOpcaoEscolhida vai executar a opção escolhida pelo usuário
executarOpcaoEscolhida :: [Produto] -> Int -> IO [Produto]
executarOpcaoEscolhida estoque opcao = case opcao of 
    1 -> do
        putStrLn "Digite o nome do produto: "
        nomeProduto <- lerString
        putStrLn "Digite a quantidade do produto: "
        quantidade <- lerInt
        putStrLn "Digite o preço do produto: "
        precoProduto <- lerDouble
        let novoProduto = Produto nomeProduto quantidade precoProduto
        let novoEstoque = adicionarProduto estoque novoProduto
        
        putStrLn "Produto adicionado com sucesso!"
        return novoEstoque
    2 -> do
       putStrLn "Digite o nome do produto para remover: "
       nomeProduto <- lerString
       let novoEstoque = removerProduto estoque nomeProduto

       putStrLn "Produto removido com sucesso!"
       return novoEstoque  
    3 -> do
        putStrLn "Digite o nome do produto para consultar: "
        nomeProduto <- lerString
        procurarProduto estoque nomeProduto
        
        return estoque
    4 -> do
        putStrLn "Digite o nome do produto: "
        nomeProduto <- lerString
        putStrLn "Digite a nova quantidade do produto: "
        novaQuantidade <- lerInt

        let novoEstoque = atualizarQuantidade estoque nomeProduto novaQuantidade
        putStrLn "Quantidade atualizada com sucesso!"

        return novoEstoque
    5 -> do
        imprimirProdutosNoEstoque estoque
        return estoque
    6 -> do 
        salvarInformacoes estoque
        putStrLn "Saindo do sistema..."
        return []
    _ -> do
        putStrLn "Opção inválida. Por favor, escolha novamente."
        return estoque