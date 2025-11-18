# gptel

**gptel** is a simple, no-frills LLM client for Emacs that supports multiple AI providers including ChatGPT, Claude, Gemini, and local models.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Features](#features)
- [Setup](#setup)
- [Usage Examples](#usage-examples)
- [Testing Framework](#testing-framework)
- [Resources](#resources)

## Overview

gptel provides a unified interface for interacting with various LLM providers from within Emacs. It supports streaming responses, multiple backends, and includes a comprehensive testing framework with mock support.

### Key Capabilities

- Multi-provider support (OpenAI, Anthropic, Google, local models)
- Streaming responses with real-time display
- Region-based interactions
- Customizable backends
- Mock testing framework
- Integration testing with Ollama

## Architecture

### Component Architecture

```mermaid
graph TB
    subgraph "User Interface"
        UI[Interactive Commands]
        Region[Region Selection]
        Buffer[Chat Buffer]
    end

    subgraph "gptel Core"
        Request[Request Builder]
        Backend[Backend Manager]
        Stream[Stream Handler]
        Response[Response Processor]
    end

    subgraph "Backends"
        OpenAI[OpenAI Backend]
        Anthropic[Anthropic Backend]
        Google[Google Backend]
        Ollama[Ollama Backend]
        Custom[Custom Backends]
    end

    subgraph "HTTP Layer"
        Curl[Curl Process]
        JSON[JSON Parser]
        SSE[SSE Decoder]
    end

    subgraph "External APIs"
        OAPI[OpenAI API]
        AAPI[Anthropic API]
        GAPI[Google API]
        Local[Local Ollama]
    end

    UI --> Request
    Region --> Request
    Buffer --> Request

    Request --> Backend
    Backend --> OpenAI
    Backend --> Anthropic
    Backend --> Google
    Backend --> Ollama
    Backend --> Custom

    OpenAI --> Curl
    Anthropic --> Curl
    Google --> Curl
    Ollama --> Curl

    Curl --> JSON
    JSON --> SSE
    SSE --> Stream

    Stream --> Response
    Response --> Buffer

    OpenAI -.-> OAPI
    Anthropic -.-> AAPI
    Google -.-> GAPI
    Ollama -.-> Local

    style UI fill:#e1f5ff
    style Request fill:#d4edda
    style Backend fill:#d4edda
    style Stream fill:#d4edda
```

### Request Flow Sequence

```mermaid
sequenceDiagram
    participant User
    participant Emacs
    participant gptel
    participant Backend
    participant HTTP
    participant API as LLM API

    User->>Emacs: Select region
    User->>gptel: M-x gptel-send
    gptel->>gptel: Read region/buffer
    gptel->>Backend: Get active backend
    Backend-->>gptel: Backend config
    gptel->>gptel: Build request payload
    gptel->>HTTP: Create curl process
    HTTP->>API: POST request
    API-->>HTTP: Stream chunks
    loop For each chunk
        HTTP->>gptel: SSE data
        gptel->>gptel: Parse JSON
        gptel->>Emacs: Insert text
        Emacs-->>User: Display update
    end
    HTTP->>gptel: Connection closed
    gptel-->>User: Complete
```

### Backend Configuration Flow

```mermaid
graph LR
    subgraph "Configuration"
        Config[User Config]
        Defaults[Default Settings]
    end

    subgraph "Backend Registry"
        Register[Backend Registry]
        OpenAI[OpenAI Config]
        Claude[Claude Config]
        Gemini[Gemini Config]
        Ollama[Ollama Config]
    end

    subgraph "Runtime"
        Active[Active Backend]
        Switch[Switch Backend]
    end

    Config --> Register
    Defaults --> Register

    Register --> OpenAI
    Register --> Claude
    Register --> Gemini
    Register --> Ollama

    OpenAI --> Active
    Claude --> Active
    Gemini --> Active
    Ollama --> Active

    Active --> Switch
    Switch -.-> Active
```

### Testing Architecture

```mermaid
graph TB
    subgraph "Test Suite"
        Unit[Unit Tests]
        Mock[Mock Tests]
        Integration[Integration Tests]
    end

    subgraph "Mock Framework"
        Enable[Enable Mock]
        Store[Mock Store]
        Spy[Spy Framework]
    end

    subgraph "Test Helpers"
        Sync[Sync Request Helper]
        Check[Availability Checker]
        Validate[Response Validator]
    end

    subgraph "gptel Core"
        Request[gptel-request]
        Backend[Backend System]
    end

    Unit --> Enable
    Mock --> Enable
    Integration --> Check

    Enable --> Store
    Enable --> Spy

    Mock --> Request
    Integration --> Request

    Store -.-> Request
    Spy -.-> Request

    Request --> Backend

    Sync --> Request
    Check --> Backend
    Validate --> Request

    style Unit fill:#fff3cd
    style Mock fill:#d1ecf1
    style Integration fill:#d4edda
```

## Features

### Multi-Provider Support

| Provider | Streaming | Models | API Key Required |
|----------|-----------|--------|------------------|
| OpenAI | ✅ | GPT-4, GPT-3.5, etc. | ✅ |
| Anthropic | ✅ | Claude 3, etc. | ✅ |
| Google | ✅ | Gemini Pro, etc. | ✅ |
| Ollama | ✅ | llama3, mistral, etc. | ❌ |
| Llama.cpp | ✅ | Local models | ❌ |

### Interactive Commands

- `gptel-send` - Send region or buffer to LLM
- `gptel` - Start a new chat session
- `gptel-menu` - Transient menu for options
- `gptel-set-backend` - Switch LLM provider

### Response Modes

```mermaid
graph LR
    Input[User Input]

    subgraph "Processing Modes"
        Insert[Insert Mode]
        Replace[Replace Mode]
        Chat[Chat Mode]
    end

    subgraph "Output"
        Buffer[Current Buffer]
        NewBuf[New Buffer]
        Region[Replace Region]
    end

    Input --> Insert
    Input --> Replace
    Input --> Chat

    Insert --> Buffer
    Replace --> Region
    Chat --> NewBuf
```

## Setup

### Basic Configuration

```elisp
;; Install from MELPA
(use-package gptel
  :ensure t
  :config
  ;; Set default backend
  (setq gptel-backend 'chatgpt)

  ;; Configure API key
  (setq gptel-api-key "your-api-key-here"))
```

### Multi-Backend Setup

```elisp
;; OpenAI
(setq gptel-backend
      (gptel-make-openai "ChatGPT"
        :key "your-openai-key"
        :models '("gpt-4" "gpt-3.5-turbo")))

;; Anthropic Claude
(setq gptel-backend
      (gptel-make-anthropic "Claude"
        :key "your-anthropic-key"
        :models '("claude-3-opus" "claude-3-sonnet")))

;; Local Ollama
(setq gptel-backend
      (gptel-make-ollama "Ollama"
        :host "localhost:11434"
        :models '("llama3.2" "mistral")
        :stream t))
```

See [demo-gptel/elisp/setup.el](../../blob/master/demo-gptel/elisp/setup.el) for complete configuration examples.

## Usage Examples

### Example 1: Code Generation

```elisp
;; Select a region with a comment like:
;; "Write a function to calculate fibonacci numbers"

;; Then run:
M-x gptel-send

;; gptel will insert the generated code below
```

### Example 2: Code Review

```elisp
;; Select your code region
;; Set a directive: "Review this code for bugs"
M-x gptel-send
```

### Example 3: Chat Session

```elisp
;; Start a chat
M-x gptel

;; Type your questions in the buffer
;; Press C-c RET to send
```

### Workflow Diagram

```mermaid
sequenceDiagram
    participant User
    participant Buffer
    participant gptel
    participant LLM

    Note over User,LLM: Code Generation Workflow

    User->>Buffer: Write comment/prompt
    User->>Buffer: Select region
    User->>gptel: M-x gptel-send
    gptel->>Buffer: Read selection
    gptel->>LLM: Send prompt
    LLM-->>gptel: Stream code
    loop Streaming
        gptel->>Buffer: Insert tokens
        Buffer-->>User: Show progress
    end
    gptel-->>User: Generation complete
    User->>Buffer: Review & edit
```

## Testing Framework

The demo-gptel project includes a comprehensive testing framework with three types of tests:

### Test Types

```mermaid
graph TD
    Tests[Test Suite]

    Tests --> Unit[Unit Tests]
    Tests --> Mock[Mock Tests]
    Tests --> Integration[Integration Tests]

    Unit --> Fast[Fast execution]
    Unit --> NoDeps[No dependencies]

    Mock --> Predictable[Predictable responses]
    Mock --> NoAPI[No API calls]

    Integration --> Real[Real Ollama]
    Integration --> Slow[Slower execution]

    style Unit fill:#fff3cd
    style Mock fill:#d1ecf1
    style Integration fill:#d4edda
```

### Mock Testing Flow

```mermaid
sequenceDiagram
    participant Test
    participant Mock as Mock Framework
    participant gptel
    participant Buffer

    Test->>Mock: Enable mocking
    Test->>Mock: Add mock response
    Note over Mock: "What is 2+2?" → "4"

    Test->>gptel: gptel-request "What is 2+2?"
    gptel->>Mock: Check for mock
    Mock-->>gptel: Return "4"
    gptel->>Buffer: Insert "4"

    Test->>Mock: Verify call
    Mock-->>Test: Call recorded ✓
    Test->>Test: Assert buffer content
```

### Running Tests

```bash
# Unit tests only (fast, no dependencies)
cd demo-gptel/tests
emacs -Q -batch -L . -l gptel-ollama-ert.el -f ert-run-tests-batch-and-exit

# Integration tests (requires Ollama)
ollama serve  # In another terminal
emacs -Q --eval "(progn
  (load-file \"gptel-ollama-ert.el\")
  (gptel-test-run-integration))"
```

See [demo-gptel/tests/SETUP-GUIDE.md](../../blob/master/demo-gptel/tests/SETUP-GUIDE.md) for detailed testing documentation.

## Data Flow

### Request Building

```mermaid
graph LR
    subgraph "Input Sources"
        A[User Prompt]
        B[Selected Region]
        C[Buffer Context]
        D[System Directive]
    end

    subgraph "Request Construction"
        E[Combine Inputs]
        F[Format for Backend]
        G[Add Metadata]
    end

    subgraph "API Request"
        H[HTTP Headers]
        I[JSON Payload]
        J[Stream Settings]
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

    H --> K[Send Request]
    I --> K
    J --> K
```

### Response Processing

```mermaid
graph LR
    subgraph "Incoming Data"
        A[SSE Stream]
        B[JSON Chunks]
    end

    subgraph "Parsing"
        C[Extract Events]
        D[Parse JSON]
        E[Decode Content]
    end

    subgraph "Processing"
        F[Accumulate Tokens]
        G[Apply Filters]
        H[Format Text]
    end

    subgraph "Display"
        I[Insert to Buffer]
        J[Update Markers]
        K[Notify User]
    end

    A --> C
    C --> D
    D --> B
    B --> E

    E --> F
    F --> G
    G --> H

    H --> I
    I --> J
    J --> K
```

## Advanced Features

### Custom Directives

```elisp
;; Set a system directive
(setq gptel-directives
      '((default . "You are a helpful assistant.")
        (code-review . "Review code for bugs and improvements.")
        (refactor . "Suggest refactorings for better code quality.")))
```

### Context Management

```mermaid
graph TB
    subgraph "Context Sources"
        Buffer[Current Buffer]
        Region[Selection]
        History[Conversation History]
        Files[Included Files]
    end

    subgraph "Context Builder"
        Collect[Collect Context]
        Truncate[Apply Token Limits]
        Format[Format Messages]
    end

    subgraph "Result"
        Prompt[Final Prompt]
    end

    Buffer --> Collect
    Region --> Collect
    History --> Collect
    Files --> Collect

    Collect --> Truncate
    Truncate --> Format
    Format --> Prompt
```

## Resources

### Documentation

- [gptel GitHub Repository](https://github.com/karthink/gptel)
- [gptel Wiki](https://github.com/karthink/gptel/wiki)
- [Demo Project](../../tree/master/demo-gptel)
- [Setup Configuration](../../blob/master/demo-gptel/elisp/setup.el)

### Testing Resources

- [ERT Test Suite](../../blob/master/demo-gptel/tests/gptel-ollama-ert.el)
- [Testing Guide](../../blob/master/demo-gptel/tests/SETUP-GUIDE.md)
- [Test Results](../../blob/master/demo-gptel/tests/results)

### Related Documentation

- [Elisp Development Guide](../../blob/master/docs/elisp-development.md)
- [gptel Babel Demo](../../blob/master/demo-gptel/docs/gptel-babel-demo.org)

---

**Navigation**: [Home](Home) | [Architecture](Architecture) | [All Components](#)
