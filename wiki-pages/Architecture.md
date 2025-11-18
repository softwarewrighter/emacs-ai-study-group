# Architecture Overview

This page provides architectural diagrams and documentation for the Emacs AI Study Group project.

## Table of Contents

- [System Architecture](#system-architecture)
- [Component Comparison](#component-comparison)
- [Integration Patterns](#integration-patterns)
- [Data Flow](#data-flow)
- [Common Workflows](#common-workflows)

## System Architecture

### High-Level Component Diagram

```mermaid
graph TB
    User[Emacs User]

    subgraph "Emacs AI Tools"
        gptel[gptel]
        orgai[org-ai]
        ellama[Ellama]
        aider[aider.el]
        mcp[elisp-dev-mcp]
    end

    subgraph "LLM Providers"
        openai[OpenAI API]
        anthropic[Anthropic API]
        google[Google AI API]
        ollama[Ollama Local]
    end

    subgraph "External Tools"
        aidertool[Aider CLI]
        mcpclient[MCP Clients]
    end

    User --> gptel
    User --> orgai
    User --> ellama
    User --> aider
    User --> mcp

    gptel --> openai
    gptel --> anthropic
    gptel --> google
    gptel --> ollama

    orgai --> openai

    ellama --> ollama

    aider --> aidertool
    aidertool --> openai
    aidertool --> anthropic

    mcp --> mcpclient
    mcpclient --> openai
    mcpclient --> anthropic

    style User fill:#e1f5ff
    style gptel fill:#d4edda
    style orgai fill:#d4edda
    style ellama fill:#d4edda
    style aider fill:#d4edda
    style mcp fill:#d4edda
```

### Layered Architecture

```mermaid
graph TB
    subgraph "Presentation Layer"
        UI[Emacs UI]
        Buffers[Buffers & Windows]
        OrgMode[Org-mode Documents]
    end

    subgraph "Application Layer"
        gptel[gptel]
        orgai[org-ai]
        ellama[Ellama]
        aider[aider.el]
        mcp[elisp-dev-mcp]
    end

    subgraph "Integration Layer"
        HTTP[HTTP Clients]
        Process[Process Management]
        JSON[JSON Parsing]
        Stream[Streaming Handlers]
    end

    subgraph "External Layer"
        APIs[LLM APIs]
        Local[Local Models]
        Tools[External Tools]
    end

    UI --> gptel
    UI --> orgai
    UI --> ellama
    UI --> aider
    Buffers --> gptel
    Buffers --> ellama
    OrgMode --> orgai

    gptel --> HTTP
    orgai --> HTTP
    ellama --> Process
    aider --> Process
    mcp --> Process

    HTTP --> JSON
    Process --> Stream

    JSON --> APIs
    Stream --> APIs
    Process --> Local
    Process --> Tools

    style UI fill:#e1f5ff
    style Buffers fill:#e1f5ff
    style OrgMode fill:#e1f5ff
```

## Component Comparison

### Feature Matrix

| Feature | gptel | org-ai | Ellama | aider.el | elisp-dev-mcp |
|---------|-------|--------|--------|----------|---------------|
| Multi-Provider | ✅ | ❌ (OpenAI) | ❌ (Ollama) | ✅ | ✅ |
| Local Models | ✅ | ❌ | ✅ | ✅ | ❌ |
| Org Integration | ⚠️ (Basic) | ✅ | ❌ | ❌ | ❌ |
| Code Generation | ✅ | ✅ | ✅ | ✅ | ✅ |
| Git Integration | ❌ | ❌ | ❌ | ✅ | ❌ |
| Streaming | ✅ | ✅ | ✅ | ✅ | N/A |
| Testing Support | ✅ | ❌ | ❌ | ❌ | ❌ |
| MCP Protocol | ❌ | ❌ | ❌ | ❌ | ✅ |

### Decision Tree

```mermaid
graph TD
    Start{Need AI in Emacs?}
    Start -->|Yes| OrgQ{Using Org-mode?}
    Start -->|No| End[No tool needed]

    OrgQ -->|Yes| orgai[Use org-ai]
    OrgQ -->|No| LocalQ{Need local models?}

    LocalQ -->|Yes| OnlyOllama{Only Ollama?}
    LocalQ -->|No| MultiQ{Multiple providers?}

    OnlyOllama -->|Yes| ellama[Use Ellama]
    OnlyOllama -->|No| gptel1[Use gptel]

    MultiQ -->|Yes| gptel2[Use gptel]
    MultiQ -->|No| GitQ{Need git integration?}

    GitQ -->|Yes| aider[Use aider.el]
    GitQ -->|No| MCPQ{Need MCP?}

    MCPQ -->|Yes| mcp[Use elisp-dev-mcp]
    MCPQ -->|No| gptel3[Use gptel]

    style orgai fill:#d4edda
    style ellama fill:#d4edda
    style gptel1 fill:#d4edda
    style gptel2 fill:#d4edda
    style gptel3 fill:#d4edda
    style aider fill:#d4edda
    style mcp fill:#d4edda
```

## Integration Patterns

### Pattern 1: Direct API Integration (gptel, org-ai)

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant Tool as gptel/org-ai
    participant API as LLM API

    User->>Emacs: Type prompt
    Emacs->>Tool: Send request
    Tool->>Tool: Build HTTP request
    Tool->>API: POST /chat/completions
    API-->>Tool: Stream response
    Tool->>Tool: Parse JSON chunks
    Tool-->>Emacs: Insert text
    Emacs-->>User: Display response
```

### Pattern 2: Local Model Integration (Ellama)

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant Ellama
    participant Ollama as Ollama Server
    participant Model as Local LLM

    User->>Emacs: M-x ellama-chat
    Emacs->>Ellama: Initialize session
    Ellama->>Ollama: Check server
    Ollama-->>Ellama: Server ready
    User->>Ellama: Send prompt
    Ellama->>Ollama: POST /api/generate
    Ollama->>Model: Load model
    Model-->>Ollama: Generate tokens
    Ollama-->>Ellama: Stream response
    Ellama-->>Emacs: Display incrementally
```

### Pattern 3: External Tool Integration (aider.el)

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant aiderel as aider.el
    participant Aider as Aider CLI
    participant Git
    participant API as LLM API

    User->>Emacs: M-x aider
    Emacs->>aiderel: Start session
    aiderel->>Aider: spawn process
    Aider->>Git: Check repo
    Git-->>Aider: Repo info
    User->>aiderel: Send request
    aiderel->>Aider: Send via stdin
    Aider->>API: Chat request
    API-->>Aider: Code suggestions
    Aider->>Git: Create commit
    Aider-->>aiderel: Display output
    aiderel-->>Emacs: Show changes
```

### Pattern 4: MCP Server (elisp-dev-mcp)

```mermaid
sequenceDiagram
    participant Client as MCP Client
    participant MCP as elisp-dev-mcp
    participant Emacs
    participant Elisp as Elisp Runtime

    Client->>MCP: Connect via stdio/HTTP
    MCP->>Emacs: Initialize connection
    Client->>MCP: List tools
    MCP-->>Client: Available tools
    Client->>MCP: Call tool
    MCP->>Emacs: Execute command
    Emacs->>Elisp: Eval code
    Elisp-->>Emacs: Result
    Emacs-->>MCP: Return data
    MCP-->>Client: JSON response
```

## Data Flow

### Request/Response Flow

```mermaid
graph LR
    subgraph "User Input"
        A[User Prompt]
        B[Context/Region]
        C[Buffer Content]
    end

    subgraph "Processing"
        D[Build Request]
        E[Add Context]
        F[Send to LLM]
    end

    subgraph "Response"
        G[Receive Stream]
        H[Parse Tokens]
        I[Insert Text]
    end

    subgraph "Post-Processing"
        J[Syntax Highlight]
        K[Validation]
        L[User Feedback]
    end

    A --> D
    B --> E
    C --> E
    D --> F
    E --> F
    F --> G
    G --> H
    H --> I
    I --> J
    J --> K
    K --> L
```

### Context Management Flow

```mermaid
graph TD
    Start[User Request]

    subgraph "Context Collection"
        Buffer[Current Buffer]
        Region[Selected Region]
        File[File Contents]
        Git[Git State]
        Docs[Documentation]
    end

    subgraph "Context Building"
        Combine[Combine Context]
        Limit[Apply Token Limit]
        Format[Format Prompt]
    end

    subgraph "LLM Processing"
        Send[Send Request]
        Receive[Receive Response]
    end

    Start --> Buffer
    Start --> Region
    Start --> File
    Start --> Git
    Start --> Docs

    Buffer --> Combine
    Region --> Combine
    File --> Combine
    Git --> Combine
    Docs --> Combine

    Combine --> Limit
    Limit --> Format
    Format --> Send
    Send --> Receive
```

## Common Workflows

### Workflow 1: Code Generation with gptel

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant gptel
    participant API as LLM API
    participant Buffer

    User->>Emacs: Select region
    User->>gptel: M-x gptel-send
    gptel->>Buffer: Read region
    gptel->>gptel: Build prompt
    gptel->>API: Send request
    API-->>gptel: Stream response
    loop For each token
        gptel->>Buffer: Insert text
        Buffer-->>User: Show incremental update
    end
    gptel->>User: Request complete
```

### Workflow 2: Org-mode AI Interaction

```mermaid
sequenceDiagram
    participant User
    participant Org as org-mode
    participant orgai as org-ai
    participant API as OpenAI API

    User->>Org: Create #+begin_ai block
    User->>Org: Add prompt
    User->>orgai: C-c C-c (execute)
    orgai->>Org: Read block content
    orgai->>orgai: Parse options
    orgai->>API: Send request
    API-->>orgai: Response
    orgai->>Org: Insert after block
    Org-->>User: Display result
```

### Workflow 3: Ollama Chat with Ellama

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant Ellama
    participant Ollama

    User->>Ellama: M-x ellama-chat
    Ellama->>Emacs: Create chat buffer
    Emacs-->>User: Show chat buffer
    User->>Ellama: Type message
    Ellama->>Ollama: Send to local model
    Ollama-->>Ellama: Stream response
    loop Streaming
        Ellama->>Emacs: Append text
        Emacs-->>User: Show update
    end
    User->>Ellama: Continue conversation
```

### Workflow 4: AI Pair Programming with aider.el

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant aiderel as aider.el
    participant Aider
    participant Git

    User->>aiderel: M-x aider
    aiderel->>Aider: Start process
    Aider->>Git: Load repo context
    User->>aiderel: /add file.el
    aiderel->>Aider: Add to context
    User->>aiderel: Implement feature X
    Aider->>Aider: Generate code
    Aider->>Git: Stage changes
    Aider->>Git: Create commit
    Aider-->>aiderel: Show diff
    aiderel-->>User: Display result
```

### Workflow 5: Testing with Mock Framework (gptel)

```mermaid
sequenceDiagram
    participant Test as ERT Test
    participant Mock as Mock Framework
    participant gptel
    participant Buffer

    Test->>Mock: Enable mocking
    Test->>Mock: Add mock response
    Test->>gptel: Send request
    gptel->>Mock: Check if mocked
    Mock-->>gptel: Return mock response
    gptel->>Buffer: Insert mock text
    Test->>Mock: Verify call
    Mock-->>Test: Call recorded
    Test->>Test: Assert result
```

## Component Details

For detailed information about each component:

- [gptel Architecture](Gptel#architecture)
- [org-ai Architecture](Org-AI#architecture)
- [Ellama Architecture](Ellama#architecture)
- [aider.el Architecture](Aider-el#architecture)
- [elisp-dev-mcp Architecture](Elisp-Dev-MCP#architecture)

## Performance Considerations

### Latency Comparison

```mermaid
graph LR
    subgraph "Response Time"
        A[Local Ollama: ~2-5s]
        B[OpenAI GPT-4: ~3-8s]
        C[Claude API: ~2-6s]
        D[Gemini: ~2-5s]
    end

    subgraph "Factors"
        E[Model Size]
        F[Network Latency]
        G[Context Length]
        H[Hardware]
    end

    E -.-> A
    F -.-> B
    F -.-> C
    F -.-> D
    G -.-> A
    G -.-> B
    G -.-> C
    G -.-> D
    H -.-> A
```

---

**Navigation**: [Home](Home) | [Component Comparison](#component-comparison)
