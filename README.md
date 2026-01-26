<div align="center">

# 💣 Campo Minado em Haskell

<img src="https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white" alt="Haskell" />
<img src="https://img.shields.io/badge/Paradigm-Functional-orange?style=for-the-badge" alt="Functional" />
<img src="https://img.shields.io/badge/UFCG-PLP-blue?style=for-the-badge&logo=google-scholar&logoColor=white" alt="UFCG" />
<img src="https://img.shields.io/badge/License-MIT-yellow?style=for-the-badge" alt="License" />

<br />

<p align="center">
  <b>Uma implementação puramente funcional do clássico jogo de estratégia.</b><br>
  Desenvolvido para a disciplina de <i>Paradigmas de Linguagens de Programação</i> da <b>UFCG</b>.
</p>

[Funcionalidades](#funcionalidades-do-projeto) • [Estrutura](#estrutura-do-projeto) • [Instalação](#configuração-e-instalação) • [Autores](#autores)

</div>

<div style="border-bottom: 3px solid #5e5086;"></div>

---

## Visão Geral

Este projeto é uma implementação do clássico jogo **Campo Minado** (Minesweeper) desenvolvido inteiramente em **Haskell**. O objetivo principal é aplicar conceitos fundamentais do **Paradigma Funcional** — como imutabilidade, recursão e funções puras — em uma aplicação interativa executada via terminal.

---

## Funcionalidades do Projeto

O jogo é totalmente executado no terminal (`CLI`) e conta com as seguintes características:

1. **Menu Principal:** Seleção de dificuldade (Fácil, Médio, Difícil) e opção de sair.
2. **Navegação:** Cursor controlado pelas setas do teclado.
3. **Lógica de Jogo:** Distribuição de bombas aleatória, cálculo de vizinhos e detecção de vitória/derrota.
4. **Estados do Jogo:** Vitória, Derrota e contagem de bombas restantes.
5. **Ranking:** Sistema de pontuação com persistência de melhores tempos.
6. **Interface:** Desenho do tabuleiro em ASCII com destaque para a célula selecionada.

---

## Estrutura do Projeto

O código foi modularizado para garantir legibilidade e facilitar a manutenção:

| Módulo | Descrição |
| :--- | :--- |
| `Main.hs` | Ponto de entrada (Entry Point). Inicializa o terminal e o menu. |
| `Interface.PreparaJogo` | Configuração do terminal, renderização do menu e do tabuleiro. |
| `Interface.Cursor` | Lógica de movimentação e destaque visual da célula selecionada. |
| `Jogo.Jogo` | Loop principal (*Game Loop*) e gerenciamento de estados. |
| `Jogo.Logica` | Funções puras de lógica, entrada de dados (`getKey`) e regras. |

---

## Configuração e Instalação

Este projeto utiliza o **Haskell Stack** para gerenciamento de dependências.

### Pré-requisitos

* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) instalado.
* Terminal com suporte a *ANSI escape codes* (Linux, macOS ou Windows via WSL/PowerShell).

### Passo a Passo

1.  **Clone o repositório:**

    ```bash
    git clone [https://github.com/SEU-USUARIO/campo-minado-haskell.git](https://github.com/SEU-USUARIO/campo-minado-haskell.git)
    cd campo-minado-haskell
    ```

2.  **Compile e execute o projeto:**

    ```bash
    stack setup
    stack build
    stack run
    ```

---

## Autores

Este projeto foi desenvolvido pelos alunos:

<table>
  <tr>
    <td align="center">
      <a href="https://github.com/annegmsilva">
        <img src="https://github.com/annegmsilva.png" width="100px;" alt="Foto de Anne Grazieli"/><br>
        <sub><b>Anne Grazieli</b></sub>
      </a>
    </td>
    <td align="center">
      <a href="https://github.com/joycevnr">
        <img src="https://github.com/joycevnr.png" width="100px;" alt="Foto de Joyce Vitória"/><br>
        <sub><b>Joyce Vitória</b></sub>
      </a>
    </td>
    <td align="center">
      <a href="https://github.com/Eduarda-Cabral">
        <img src="https://github.com/Eduarda-Cabral.png" width="100px;" alt="Foto de Maria Eduarda"/><br>
        <sub><b>Maria Eduarda</b></sub>
      </a>
    </td>
    <td align="center">
      <a href="https://github.com/Pedroz007">
        <img src="https://github.com/Pedroz007.png" width="100px;" alt="Foto de Pedro Henrique"/><br>
        <sub><b>Pedro Henrique</b></sub>
      </a>
    </td>
     <td align="center">
      <a href="https://github.com/Thiago-Barbos">
        <img src="https://github.com/Thiago-Barbos.png" width="100px;" alt="Foto de Thiago Barbosa"/><br>
        <sub><b>Thiago Barbosa</b></sub>
      </a>
    </td>
  </tr>
</table>
