# Conversor de Expressões Regulares para NFA

Este projeto implementa em OCaml a conversão de **expressões regulares (ER)** para um **autómato não-determinístico (NFA)**, utilizando construções padrão como concatenação, escolha e estrela de Kleene.

## Como funciona

1. O utilizador insere uma expressão regular como entrada.
2. O programa gera o NFA correspondente:

   * Estados iniciais e finais.
   * Transições (incluindo transições epsilon).
3. O autómato é impresso no terminal no seguinte formato:

   * Número total de estados
   * Número de estados iniciais
   * Estados iniciais
   * Número de estados finais
   * Estados finais
   * Número de transições
   * Transições
