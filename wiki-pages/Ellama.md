# Ellama

**Ellama** is an Emacs tool for interacting with Large Language Models using Ollama, providing seamless integration with local AI models.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Features](#features)
- [Setup](#setup)
- [Usage Examples](#usage-examples)
- [Workflows](#workflows)
- [Resources](#resources)

## Overview

Ellama brings Ollama-powered local LLMs to Emacs, enabling privacy-focused AI assistance without requiring cloud API keys.

### Key Capabilities

- Interactive LLM sessions directly in Emacs
- Code generation and refactoring
- Text translation and summarization
- Context-aware assistance using local models
- Integration with Ollama model ecosystem
- No API keys required

## Architecture

### Component Architecture

```mermaid
graph TB
    subgraph "User Interface"
        Commands[Interactive Commands]
        Buffer[Chat Buffers]
        Region[Region Operations]
    end

    subgraph "Ellama Core"
        Session[Session Manager]
        Request[Request Builder]
        Context[Context Manager]
        Response[Response Handler]
    end

    subgraph "Ollama Integration"
        Client[Ollama Client]
        Process[Process Handler]
        Stream[Stream Parser]
    end

    subgraph "Ollama Server"
        Server[Ollama Server]
        Models[Local Models]
        GPU[GPU Acceleration]
    end

    Commands --> Session
    Buffer --> Session
    Region --> Request

    Session --> Request
    Request --> Context
    Context --> Client

    Client --> Process
    Process --> Stream
    Stream --> Response

    Client -.-> Server
    Server --> Models
    Models -.-> GPU

    Response --> Buffer

    style Commands fill:#e1f5ff
    style Buffer fill:#e1f5ff
    style Session fill:#d4edda
    style Request fill:#d4edda
    style Context fill:#d4edda
```

### Request Flow

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant Ellama
    participant Ollama as Ollama Server
    participant Model as Local LLM

    User->>Emacs: M-x ellama-chat
    Emacs->>Ellama: Initialize session
    Ellama->>Ollama: Check server status
    Ollama-->>Ellama: Server ready

    Ellama->>Emacs: Create chat buffer
    Emacs-->>User: Display buffer

    User->>Ellama: Type message
    Ellama->>Ollama: POST /api/generate
    Note over Ollama,Model: Request with prompt and context

    Ollama->>Model: Load model if needed
    Model-->>Ollama: Model ready

    loop Token Generation
        Ollama->>Model: Generate next token
        Model-->>Ollama: Token
        Ollama-->>Ellama: Stream token
        Ellama->>Emacs: Insert token
        Emacs-->>User: Display update
    end

    Ollama-->>Ellama: Generation complete
    Ellama-->>User: Ready for next input
```

### Model Loading Flow

```mermaid
graph LR
    subgraph "Model Selection"
        A[User Choice]
        B[Model Name]
        C[Model Config]
    end

    subgraph "Ollama Server"
        D[Check Cached]
        E[Load from Disk]
        F[Model in Memory]
    end

    subgraph "Generation"
        G[Ready for Inference]
        H[GPU Acceleration]
    end

    A --> B
    B --> C
    C --> D

    D -->|Cached| F
    D -->|Not Cached| E
    E --> F

    F --> G
    G --> H
```

## Features

### Supported Models

Ellama works with any Ollama-compatible model:

| Model Family | Example Models | Use Case |
|--------------|----------------|----------|
| Llama 3 | llama3, llama3.2 | General purpose, coding |
| Mistral | mistral, mixtral | Fast, efficient |
| CodeLlama | codellama | Code generation |
| Phi | phi3 | Lightweight, fast |
| Gemma | gemma | Google's models |

### Interactive Commands

```mermaid
graph TB
    Ellama[Ellama Commands]

    Ellama --> Chat[ellama-chat]
    Ellama --> Ask[ellama-ask-about]
    Ellama --> Code[ellama-code]
    Ellama --> Translate[ellama-translate]
    Ellama --> Summarize[ellama-summarize]
    Ellama --> Improve[ellama-improve-text]

    Chat --> NewBuffer[New chat buffer]
    Ask --> Region[Ask about region]
    Code --> Generate[Generate code]
    Translate --> Lang[Translate text]
    Summarize --> Summary[Create summary]
    Improve --> Enhanced[Enhance text]

    style Ellama fill:#e1f5ff
    style Chat fill:#d4edda
    style Ask fill:#d4edda
    style Code fill:#d4edda
    style Translate fill:#d4edda
    style Summarize fill:#d4edda
    style Improve fill:#d4edda
```

### Operation Modes

| Command | Mode | Input | Output |
|---------|------|-------|--------|
| `ellama-chat` | Interactive | User prompts | Chat buffer |
| `ellama-ask-about` | Query | Region/buffer | New buffer |
| `ellama-code` | Generation | Prompt | Code block |
| `ellama-translate` | Transform | Region | Translation |
| `ellama-summarize` | Reduction | Region/buffer | Summary |
| `ellama-improve-text` | Enhancement | Region | Improved text |

## Setup

### Prerequisites

1. Install Ollama:

```bash
# macOS
brew install ollama

# Linux
curl -fsSL https://ollama.com/install.sh | sh

# Windows
# Download from https://ollama.com/download
```

2. Start Ollama server:

```bash
ollama serve
```

3. Pull a model:

```bash
ollama pull llama3.2
# or
ollama pull mistral
# or
ollama pull codellama
```

### Emacs Configuration

```elisp
;; Install Ellama
(use-package ellama
  :ensure t
  :config
  ;; Set provider to Ollama
  (require 'llm-ollama)

  (setq ellama-provider
        (make-llm-ollama
         :chat-model "llama3.2"
         :embedding-model "nomic-embed-text"))

  ;; Optional: Configure additional settings
  (setq ellama-language "English")
  (setq ellama-sessions-directory
        (expand-file-name "ellama-sessions" user-emacs-directory)))
```

### Advanced Configuration

```elisp
;; Multiple models for different tasks
(setq ellama-providers
      '((chat . ,(make-llm-ollama :chat-model "llama3.2"))
        (code . ,(make-llm-ollama :chat-model "codellama"))
        (translate . ,(make-llm-ollama :chat-model "mistral"))))

;; Custom naming template
(setq ellama-naming-scheme 'ellama-generate-name-by-llm)

;; Auto-save sessions
(setq ellama-auto-save t)
```

## Usage Examples

### Example 1: Chat Session

```elisp
;; Start a chat
M-x ellama-chat

;; In the chat buffer, type your questions:
;; "Explain how closures work in Emacs Lisp"
;; Press RET to send

;; The response streams in real-time
```

### Example 2: Code Generation

```elisp
;; Generate code with a prompt
M-x ellama-code

;; Prompt: "Write a function to sort a list of numbers"
;; Ellama generates the code in a new buffer
```

### Example 3: Text Improvement

```elisp
;; Select a region of text
;; Run: M-x ellama-improve-text

;; Ellama rewrites the text with improvements
```

### Example 4: Translation

```elisp
;; Select text to translate
M-x ellama-translate

;; Choose target language: Spanish
;; Ellama provides translation
```

## Workflows

### Code Development Workflow

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant Ellama
    participant Ollama

    Note over User,Ollama: Code Generation Workflow

    User->>Ellama: M-x ellama-code
    Ellama->>User: Prompt for description

    User->>Ellama: "Parse JSON with error handling"
    Ellama->>Ollama: Send with code context

    Ollama-->>Ellama: Stream code
    loop Code generation
        Ellama->>Emacs: Insert code line
        Emacs-->>User: Display
    end

    User->>Ellama: ellama-improve-code
    Note over Ellama: Refine generated code

    Ollama-->>Ellama: Improved version
    Ellama->>Emacs: Replace code
```

### Document Workflow

```mermaid
graph TB
    Draft[Write Draft]

    Draft --> Select[Select Region]
    Select --> Improve[ellama-improve-text]
    Improve --> Review[Review Changes]

    Review -->|Accept| Next[Next Section]
    Review -->|Retry| Improve

    Next --> Select2[Select Next Region]
    Select2 --> Summarize[ellama-summarize]
    Summarize --> Final[Final Document]

    style Draft fill:#e1f5ff
    style Final fill:#e1f5ff
```

### Research Workflow

```mermaid
sequenceDiagram
    participant User
    participant Ellama
    participant Ollama

    User->>Ellama: ellama-chat "Research topic X"
    Ollama-->>Ellama: Initial information

    User->>Ellama: "Elaborate on aspect Y"
    Ollama-->>Ellama: Detailed explanation

    User->>Ellama: "Provide examples"
    Ollama-->>Ellama: Examples

    User->>Ellama: "Summarize key points"
    Ollama-->>Ellama: Summary

    Note over User: Save chat session for later
```

## Data Flow

### Context Building

```mermaid
graph LR
    subgraph "Input Sources"
        A[User Prompt]
        B[Selected Region]
        C[Buffer Content]
        D[Chat History]
    end

    subgraph "Context Assembly"
        E[Combine Inputs]
        F[Add System Prompt]
        G[Format for Ollama]
    end

    subgraph "Ollama Request"
        H[Model Name]
        I[Prompt Text]
        J[Parameters]
    end

    A --> E
    B --> E
    C --> E
    D --> E

    E --> F
    F --> G

    G --> H
    G --> I
    G --> J
```

### Response Processing

```mermaid
graph LR
    subgraph "Ollama Response"
        A[JSON Stream]
        B[Token Data]
    end

    subgraph "Processing"
        C[Parse JSON]
        D[Extract Token]
        E[Decode UTF-8]
    end

    subgraph "Display"
        F[Accumulate Text]
        G[Insert to Buffer]
        H[Update Display]
    end

    A --> C
    C --> B
    B --> D
    D --> E

    E --> F
    F --> G
    G --> H
```

## Advanced Features

### Session Management

```mermaid
graph TB
    subgraph "Session Lifecycle"
        Create[Create Session]
        Chat[Chat Interactions]
        Save[Auto-save]
        Load[Load Later]
    end

    subgraph "Session Storage"
        Dir[Sessions Directory]
        Files[Session Files]
        Meta[Metadata]
    end

    Create --> Chat
    Chat --> Save
    Save --> Dir
    Dir --> Files
    Files --> Meta

    Meta --> Load
    Load --> Chat
```

### Model Switching

```mermaid
sequenceDiagram
    participant User
    participant Ellama
    participant Ollama

    User->>Ellama: Start with llama3.2
    Note over Ellama: Using general model

    User->>Ellama: Switch to codellama
    Ellama->>Ollama: Check model available
    Ollama-->>Ellama: Model ready

    Note over Ellama: Using code-specialized model

    User->>Ellama: Ask code question
    Ollama-->>Ellama: Better code response
```

## Performance Characteristics

### Response Time Factors

```mermaid
graph LR
    subgraph "Performance Factors"
        A[Model Size]
        B[Hardware]
        C[Context Length]
        D[Response Length]
    end

    subgraph "Impact"
        E[Loading Time]
        F[Inference Speed]
        G[Memory Usage]
    end

    A --> E
    A --> G
    B --> F
    B --> G
    C --> F
    D --> F
```

### Optimization Tips

- Use smaller models for faster responses (phi3, mistral)
- Use specialized models for specific tasks (codellama for code)
- Limit context length when possible
- Use GPU acceleration if available
- Keep frequently-used models loaded

## Advantages

- **Privacy**: All processing happens locally
- **No Cost**: No API fees
- **Offline**: Works without internet
- **Customizable**: Use any Ollama model
- **Fast**: Local inference, no network latency

## Limitations

- **Hardware**: Requires sufficient RAM/GPU
- **Model Quality**: Depends on local model capabilities
- **Setup**: Requires Ollama installation
- **Single Provider**: Only works with Ollama

## Resources

### Documentation

- [Ellama GitHub Repository](https://github.com/s-kostyaev/ellama)
- [Ollama Documentation](https://github.com/ollama/ollama)
- [Ollama Model Library](https://ollama.com/library)
- [Demo Project](../../tree/master/demo-ellama)

### Ollama Resources

- [Ollama Installation](https://ollama.com/download)
- [Model Quantization Guide](https://github.com/ollama/ollama/blob/main/docs/quantization.md)
- [GPU Support](https://github.com/ollama/ollama/blob/main/docs/gpu.md)

### Related Documentation

- [Elisp Development Guide](../../blob/master/docs/elisp-development.md)
- [LLM.el Documentation](https://github.com/ahyatt/llm)

---

**Navigation**: [Home](Home) | [Architecture](Architecture) | [gptel](Gptel) | [org-ai](Org-AI)
