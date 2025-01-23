# Gerenciador de Estoque em Haskell
Trabalho final da disciplina de Programação Funcional, do sexto período do curso de Sistemas de Informação na Universidade Federal do Piauí.

### Estrutura do projeto
```bash
Gerenciador-de-Estoque/
                    Estoque.hs
                    estoque.txt
                    Menu.hs
                    main.hs
                    Produto.hs
```

### Clonando e acessando o repositório: 
```bash
git clone https://github.com/Liedsonfsa/Gerenciador-de-Estoque.git
cd Gerenciador-de-Estoque
```


### Para rodar o programa, faça:
```bash
$ ghc -c Produto.hs
$ ghc -c Estoque.hs
$ ghc -c Menu.hs
$ ghc main.hs Produto.o Estoque.o Menu.o -o estoque
$ ./estoque
```