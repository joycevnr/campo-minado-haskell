# 💣 Campo Minado em Haskell

Este projeto é uma implementação do clássico jogo **Campo Minado** (Minesweeper) desenvolvido inteiramente em **Haskell**. O objetivo é aplicar conceitos do **Paradigma Funcional** (imutabilidade, recursão, funções puras) em uma aplicação interativa via terminal.

---

## 🚀 Funcionalidades do Projeto

O jogo é totalmente executado no terminal e permite que o jogador navegue usando o teclado.

1. **Menu Principal:** Seleção de dificuldade (Fácil, Médio, Difícil) e opção de sair.
2. **Navegação:** Cursor controlado pelas setas do teclado.
3. **Lógica de Jogo:** Distribuição de bombas aleatória, cálculo de vizinhos e detecção de vitória/derrota.
4. **Estados do Jogo:** Vitória, Derrota e contagem de bombas restantes.
5. **Ranking:** Sistema de pontuação com persistência de melhores tempos.
6. **Interface:** Desenho do tabuleiro em ASCII com destaque para a célula selecionada.

---

## 🛠️ Estrutura do Projeto

O projeto está organizado em módulos Haskell para melhor manutenção e legibilidade:

- `Main.hs` – Ponto de entrada do jogo, verifica tamanho do terminal e inicia o menu.
- `Interface.PreparaJogo.hs` – Configuração do terminal, menu principal, seleção de dificuldade e desenho do tabuleiro.
- `Interface.Cursor.hs` – Controle do cursor no tabuleiro, incluindo movimento e destaque da célula.
- `Jogo.Jogo.hs` – Loop principal do jogo e instruções ao jogador.
- `Jogo.Logica.hs` – Funções auxiliares de entrada do usuário (`getKey`) e lógica de captura de comandos.

---

## 🛠️ Configuração e Instalação

Este projeto utiliza **Haskell Stack** para gerenciar dependências e garantir compatibilidade.

### Pré-requisitos

- [Stack](https://docs.haskellstack.org/en/stable/README/) instalado
- Terminal que suporte ANSI escape codes (Linux, macOS ou Windows PowerShell / WSL)

### Como rodar

1. Clone o repositório:

   ```bash
   git clone https://github.com/SEU-USUARIO/campo-minado-haskell.git
   cd campo-minado-haskell


2. Compile e execute com Stack:
   ```bash
   stack setup
   stack build
   stack run
