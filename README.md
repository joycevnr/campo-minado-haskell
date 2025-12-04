# 💣 Campo Minado em Haskell

Este projeto é uma implementação do clássico jogo **Campo Minado** (Minesweeper) desenvolvido inteiramente em **Haskell**. O objetivo é aplicar os conceitos de Paradigma Funcional (imutabilidade, recursão, funções puras) em uma aplicação interativa via terminal.

---

## 🚀 Funcionalidades do Projeto

O sistema foi desenhado para rodar no terminal, onde o jogador navega pelo teclado para revelar casas e marcar bombas.

1.  **Menu Principal:** Seleção de dificuldade e início de jogo.
2.  **Mecânica de Jogo:** Navegação via cursor (Setas/WASD).
3.  **Lógica:** Algoritmos de distribuição de bombas e cálculo de vizinhança.
4.  **Estados:** Vitória, Derrota e contagem de bombas restantes.
5.  **Ranking:** Sistema de pontuação com persistência (melhor tempo).

---


## 🛠️ Configuração e Instalação

Este projeto utiliza o Haskell Stack para gerenciar dependências e garantir que todos rodem na mesma versão.

### Pré-requisitos
Ter o [Stack](https://docs.haskellstack.org/en/stable/README/) instalado na máquina.

### Como rodar
1. Clone o repositório:
   ```bash
   git clone [https://github.com/SEU-USUARIO/campo-minado-haskell.git](https://github.com/SEU-USUARIO/campo-minado-haskell.git)
   cd campo-minado-haskell