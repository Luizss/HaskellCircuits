# HaskellCircuits
Transforming haskell to hardware

## Idéia

![Alt text](./img/CORE.png?raw=true "Data flow")

### Explicação

A etapa 1 é composta por simplicação das funções, retirada de escopos locais, type checking e retirada de polimorfismo. Após esta etapa chega-se na linguagem CORE (B), que é mais fácil de ser analisada e manipulada. A partir do CORE, todos as transformações posteriores são chamadas de síntese. A primeira síntese (2) tem a função de transformar tipos ADT's de alto nível em tipos padrões de baixo nível mais relacionados as entradas e saídas de hardwares. O resultado, a linguagem CORE C ainda é  chamada de CORE por ter o mesmo formato da linguagem CORE B. A partir daí a linguagem CORE (C) é analisada e manipulada (3) para gerar um tipo de dado que representa o hardware, formado por componentes. Por fim, a etapa 4 transforma esses componentes em código SystemC.

## Versões do CORE

As versões do sistema serão baseadas nas versões e nas funcionalidades implementadas pela linguagem CORE. A etapa 1 e a linguagem A serão implementadas posteriormente enquanto a linguagem CORE (B ou C) será o ponto de entrada do sistema nas versões iniciais. A versão do CORE só aumenta quando todo o processo 3 e 4 forem implementados para todas as funcionalidades da versão do CORE.

### CORE 0.0 (versão atual)

CORE com tipos baixo nível (ou seja, representa a linguagem CORE C). Suporta aplicação de funções, tipos de baixo nível atemporais (não recursivos), funções primitivas built-in (como adição, por exemplo). Não suporta condições e recursão.
Ao fim da implementação dos processos 3 e 4 será possível gerar código para sistemas combinacionais de funções.

### CORE 1.0

Adição de condições.

### CORE 2.0

Adição de recursão ainda sem tipos temporais (recursivos).

### CORE 3.0

Adição de tipos temporais (recursivos) ainda de baixo nível.

### CORE 4.0

Implementação da etapa 2 de síntese de tipos. CORE vira CORE B (tipos agora são de alto nível). 

### HaskellCircuits 1.0

Implementação da linguagem Haskell e suas transformaçes até o CORE.

### HaskellCircuits 5.0

Otimizações e melhoramentos??????
