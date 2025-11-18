# aider.el

**aider.el** provides an Emacs interface for Aider, an AI pair programming tool that can directly edit your code with git integration.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Features](#features)
- [Setup](#setup)
- [Usage Examples](#usage-examples)
- [Workflows](#workflows)
- [Resources](#resources)

## Overview

aider.el brings Aider's powerful AI pair programming capabilities into Emacs, enabling AI-assisted code editing with automatic git commits.

### Key Capabilities

- AI-powered code editing with git integration
- Multi-file context awareness
- Automatic commit generation
- Support for multiple LLM providers (OpenAI, Anthropic, local models)
- Interactive chat interface within Emacs
- Code refactoring and improvement

## Architecture

### Component Architecture

```mermaid
graph TB
    subgraph "Emacs Layer"
        UI[Interactive Commands]
        Buffer[Aider Buffer]
        FileBuffers[File Buffers]
    end

    subgraph "aider.el"
        Interface[Aider Interface]
        Process[Process Manager]
        IO[Input/Output Handler]
    end

    subgraph "Aider CLI"
        Core[Aider Core]
        Parser[Command Parser]
        Editor[Code Editor]
        Git[Git Manager]
    end

    subgraph "External Services"
        LLM[LLM APIs]
        Repo[Git Repository]
    end

    UI --> Interface
    Buffer --> Interface
    FileBuffers --> Interface

    Interface --> Process
    Process --> IO

    IO -.-> Core
    Core --> Parser
    Core --> Editor
    Core --> Git

    Core -.-> LLM
    Git -.-> Repo

    Editor -.-> FileBuffers
    Git -.-> Repo

    style UI fill:#e1f5ff
    style Buffer fill:#e1f5ff
    style Interface fill:#d4edda
    style Process fill:#d4edda
```

### Process Communication Flow

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant aiderel as aider.el
    participant Process as Aider Process
    participant Git
    participant LLM

    User->>Emacs: M-x aider
    Emacs->>aiderel: Start aider
    aiderel->>Process: spawn aider CLI
    Process->>Git: Check repository
    Git-->>Process: Repo status

    Process-->>aiderel: Ready prompt
    aiderel->>Emacs: Display in buffer
    Emacs-->>User: Show aider interface

    User->>aiderel: Send command
    aiderel->>Process: Write to stdin
    Process->>LLM: Generate code
    LLM-->>Process: Code changes

    Process->>Git: Stage changes
    Process->>Git: Create commit
    Git-->>Process: Commit created

    Process-->>aiderel: Output changes
    aiderel->>Emacs: Update buffer
    Emacs->>Emacs: Reload file buffers
    Emacs-->>User: Show updated files
```

### Git Integration Flow

```mermaid
graph TB
    subgraph "Aider Actions"
        A[Code Change Request]
        B[Generate Code]
        C[Apply Edits]
    end

    subgraph "Git Operations"
        D[Stage Files]
        E[Create Commit]
        F[Commit Message]
    end

    subgraph "Repository"
        G[Working Tree]
        H[Git History]
        I[Branches]
    end

    A --> B
    B --> C
    C --> D

    D --> E
    E --> F

    F --> G
    G --> H
    H --> I

    style A fill:#e1f5ff
    style G fill:#d4edda
    style H fill:#d4edda
```

## Features

### Core Capabilities

| Feature | Description | Command |
|---------|-------------|---------|
| Interactive Chat | Chat with AI about code | In aider buffer |
| File Management | Add/remove files from context | `/add`, `/drop` |
| Code Editing | AI makes direct code changes | Natural language requests |
| Git Integration | Automatic commits | Enabled by default |
| Multi-file Support | Edit multiple files | Add multiple files |
| Undo Support | Revert changes | `/undo` |

### Aider Commands

```mermaid
graph TB
    Commands[Aider Commands]

    Commands --> Add[/add file.el]
    Commands --> Drop[/drop file.el]
    Commands --> Undo[/undo]
    Commands --> Clear[/clear]
    Commands --> Help[/help]
    Commands --> Commit[/commit]

    Add --> Context[Add to context]
    Drop --> Remove[Remove from context]
    Undo --> Revert[Revert last change]
    Clear --> Reset[Clear history]
    Help --> Info[Show help]
    Commit --> Save[Force commit]

    style Commands fill:#e1f5ff
    style Add fill:#d4edda
    style Drop fill:#d4edda
    style Undo fill:#d4edda
```

### LLM Provider Support

```mermaid
graph LR
    Aider[Aider CLI]

    Aider --> OpenAI[OpenAI API]
    Aider --> Anthropic[Anthropic API]
    Aider --> Ollama[Ollama Local]
    Aider --> Custom[Other Providers]

    OpenAI --> GPT4[GPT-4]
    OpenAI --> GPT35[GPT-3.5]

    Anthropic --> Claude3[Claude 3]
    Anthropic --> ClaudeOpus[Claude Opus]

    Ollama --> Llama[Llama 3]
    Ollama --> CodeLlama[CodeLlama]

    style Aider fill:#e1f5ff
```

## Setup

### Prerequisites

1. Install Aider CLI:

```bash
# Using pip
pip install aider-chat

# Or using pipx (recommended)
pipx install aider-chat
```

2. Configure API keys (choose one or more):

```bash
# OpenAI
export OPENAI_API_KEY=your-key-here

# Anthropic
export ANTHROPIC_API_KEY=your-key-here

# Or use local models with Ollama
ollama serve
```

### Emacs Configuration

```elisp
;; Install aider.el
;; Note: May need manual installation as it's not in MELPA yet
(use-package aider
  :load-path "path/to/aider.el"
  :config
  ;; Set aider executable path if needed
  (setq aider-executable "aider")

  ;; Optional: Set default arguments
  (setq aider-args '("--no-auto-commits"))

  ;; Key binding
  :bind ("C-c a" . aider))
```

### Advanced Configuration

```elisp
;; Use specific model
(setq aider-args '("--model" "gpt-4"))

;; Use Claude
(setq aider-args '("--model" "claude-3-opus-20240229"))

;; Use local Ollama
(setq aider-args '("--model" "ollama/codellama"))

;; Disable auto-commits
(setq aider-args '("--no-auto-commits"))

;; Custom git settings
(setq aider-args '("--no-auto-commits" "--no-dirty-commits"))
```

## Usage Examples

### Example 1: Add Feature

```elisp
;; Start aider
M-x aider

;; In aider buffer:
/add my-package.el

;; Request feature:
"Add error handling to the fetch-data function"

;; Aider generates code and commits
```

### Example 2: Refactor Code

```elisp
;; Add file to context
/add old-code.el

;; Request refactoring
"Refactor this code to use lexical binding and modern Elisp conventions"

;; Review changes and continue conversation
```

### Example 3: Multi-file Change

```elisp
;; Add multiple files
/add main.el
/add utils.el
/add tests.el

;; Request coordinated changes
"Move the helper functions from main.el to utils.el and update the tests"

;; Aider edits all files and creates commit
```

## Workflows

### Feature Development Workflow

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant aider as aider.el
    participant Aider as Aider CLI
    participant Git
    participant LLM

    Dev->>aider: Start aider session
    aider->>Aider: Launch process

    Dev->>Aider: /add feature.el
    Aider->>Aider: Add to context

    Dev->>Aider: Implement feature X
    Aider->>LLM: Request code
    LLM-->>Aider: Generated code

    Aider->>Aider: Apply edits
    Aider->>Git: Stage changes
    Aider->>Git: Commit with message

    loop Iteration
        Dev->>Aider: Refine feature
        Aider->>LLM: Request changes
        LLM-->>Aider: Updated code
        Aider->>Git: New commit
    end

    Dev->>Aider: /done
```

### Bug Fix Workflow

```mermaid
graph TB
    Start[Identify Bug]

    Start --> AddFiles[/add relevant files]
    AddFiles --> Describe[Describe bug]
    Describe --> Generate[Aider generates fix]

    Generate --> Review{Review Fix}
    Review -->|Good| Commit[Auto-commit]
    Review -->|Needs work| Refine[Request refinement]

    Refine --> Generate
    Commit --> Test[Run tests]

    Test -->|Pass| Done[Complete]
    Test -->|Fail| Describe

    style Start fill:#e1f5ff
    style Done fill:#d4edda
```

### Code Review Workflow

```mermaid
sequenceDiagram
    participant Dev
    participant Aider
    participant LLM
    participant Git

    Dev->>Aider: /add code.el
    Dev->>Aider: Review this code for issues

    Aider->>LLM: Request code review
    LLM-->>Aider: List of issues

    Dev->>Aider: Fix the issues you found
    Aider->>LLM: Request fixes
    LLM-->>Aider: Fixed code

    Aider->>Git: Create commit
    Git-->>Dev: Changes committed

    Dev->>Aider: Add tests for these fixes
    Aider->>LLM: Generate tests
    LLM-->>Aider: Test code
    Aider->>Git: Commit tests
```

## Data Flow

### Context Management

```mermaid
graph LR
    subgraph "Context Sources"
        A[Added Files]
        B[File Contents]
        C[Git Diff]
        D[Chat History]
    end

    subgraph "Context Building"
        E[Read Files]
        F[Build Context]
        G[Apply Limits]
    end

    subgraph "LLM Request"
        H[System Prompt]
        I[Code Context]
        J[User Request]
    end

    A --> E
    B --> E
    C --> E
    D --> F

    E --> F
    F --> G

    G --> H
    G --> I
    G --> J
```

### Code Application

```mermaid
graph TB
    subgraph "LLM Response"
        A[Generated Code]
        B[Diff Blocks]
        C[File Paths]
    end

    subgraph "Processing"
        D[Parse Response]
        E[Extract Edits]
        F[Validate Changes]
    end

    subgraph "Application"
        G[Apply Diffs]
        H[Write Files]
        I[Stage Changes]
    end

    subgraph "Git"
        J[Create Commit]
        K[Update History]
    end

    A --> D
    B --> D
    C --> D

    D --> E
    E --> F

    F --> G
    G --> H
    H --> I

    I --> J
    J --> K
```

## Advanced Features

### Repository Mapping

```mermaid
graph TB
    subgraph "Repository Analysis"
        A[Scan Files]
        B[Build Tree]
        C[Create Map]
    end

    subgraph "Context Enhancement"
        D[Relevant Files]
        E[Dependencies]
        F[Imports]
    end

    subgraph "Smart Context"
        G[Auto-add Related]
        H[Suggest Files]
        I[Better Edits]
    end

    A --> B
    B --> C
    C --> D

    D --> E
    E --> F

    F --> G
    G --> H
    H --> I

    style C fill:#d4edda
    style I fill:#d4edda
```

### Commit Management

```mermaid
sequenceDiagram
    participant Aider
    participant Git

    Note over Aider,Git: Automatic Commit Flow

    Aider->>Aider: Generate code changes
    Aider->>Git: git add changed-files
    Aider->>Aider: Generate commit message
    Aider->>Git: git commit -m "message"

    Note over Aider: User can undo

    alt User undoes
        Aider->>Git: git reset HEAD~1
        Aider->>Git: git checkout changed-files
    else User continues
        Aider->>Aider: Continue with next change
    end
```

## Integration with Emacs

### File Buffer Synchronization

```mermaid
sequenceDiagram
    participant Aider
    participant Files
    participant Emacs

    Aider->>Files: Write changes
    Files-->>Emacs: File changed

    Note over Emacs: Detect external change

    Emacs->>Emacs: Prompt to reload
    Emacs->>Files: Read new content
    Files-->>Emacs: Updated content

    Emacs->>Emacs: Update buffer
    Note over Emacs: User sees changes
```

## Advantages

- **Git Integration**: Automatic commit creation
- **Multi-file Editing**: Coordinated changes across files
- **Smart Context**: Repository-aware suggestions
- **Undo Support**: Easy to revert changes
- **Multiple Providers**: Support for various LLMs

## Limitations

- **External Process**: Requires aider CLI installation
- **Git Required**: Must be in a git repository
- **Learning Curve**: More complex than simple chat tools
- **Resource Usage**: May consume significant tokens for large contexts

## Best Practices

1. **Start Small**: Add only relevant files to context
2. **Incremental Changes**: Make one change at a time
3. **Review Commits**: Check generated commits before pushing
4. **Use Undo**: Don't hesitate to undo and retry
5. **Clear Context**: Use `/clear` to reset when changing tasks

## Resources

### Documentation

- [aider.el GitHub Repository](https://github.com/tninja/aider.el)
- [Aider Documentation](https://aider.chat/docs/)
- [Aider GitHub](https://github.com/paul-gauthier/aider)
- [Demo Project](../../tree/master/demo-aider-el)

### Aider Resources

- [Aider Installation Guide](https://aider.chat/docs/install.html)
- [Aider Usage Examples](https://aider.chat/docs/usage.html)
- [Supported Models](https://aider.chat/docs/llms.html)

### Related Documentation

- [Elisp Development Guide](../../blob/master/docs/elisp-development.md)
- [Git Best Practices](https://git-scm.com/book/en/v2)

---

**Navigation**: [Home](Home) | [Architecture](Architecture) | [gptel](Gptel) | [elisp-dev-mcp](Elisp-Dev-MCP)
