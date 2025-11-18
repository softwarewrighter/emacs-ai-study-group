# org-ai

**org-ai** integrates AI capabilities directly into org-mode, allowing you to use AI within your org documents for various tasks including chat, code generation, text manipulation, and image creation.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Features](#features)
- [Setup](#setup)
- [Usage Examples](#usage-examples)
- [Integration Patterns](#integration-patterns)
- [Resources](#resources)

## Overview

org-ai brings AI functionality into the heart of org-mode, enabling seamless AI interactions within your documents, notes, and workflows.

### Key Capabilities

- AI chat sessions within org documents
- Inline text generation and completion
- Code generation and refactoring
- Image generation with DALL-E
- Speech input and output
- Native org-mode block syntax

## Architecture

### Component Architecture

```mermaid
graph TB
    subgraph "Org-mode Layer"
        OrgDoc[Org Document]
        Block[AI Blocks]
        Babel[Babel Integration]
    end

    subgraph "org-ai Core"
        Parser[Block Parser]
        Handler[Request Handler]
        Renderer[Response Renderer]
        Context[Context Manager]
    end

    subgraph "OpenAI Integration"
        Chat[Chat API]
        Complete[Completion API]
        Image[DALL-E API]
        Speech[Whisper API]
    end

    subgraph "Output Processing"
        Format[Formatter]
        Insert[Inserter]
        Display[Display Handler]
    end

    OrgDoc --> Block
    Block --> Parser
    Parser --> Handler

    Handler --> Context
    Context --> Chat
    Context --> Complete
    Context --> Image
    Context --> Speech

    Chat --> Renderer
    Complete --> Renderer
    Image --> Renderer
    Speech --> Renderer

    Renderer --> Format
    Format --> Insert
    Insert --> Display
    Display --> OrgDoc

    style OrgDoc fill:#e1f5ff
    style Block fill:#e1f5ff
    style Parser fill:#d4edda
    style Handler fill:#d4edda
```

### AI Block Execution Flow

```mermaid
sequenceDiagram
    participant User
    participant Org as org-mode
    participant orgai as org-ai
    participant Parser
    participant API as OpenAI API

    User->>Org: Create #+begin_ai block
    User->>Org: Write prompt
    User->>Org: C-c C-c (execute block)

    Org->>orgai: Execute block
    orgai->>Parser: Parse block options
    Parser-->>orgai: Configuration

    orgai->>orgai: Build request
    Note over orgai: Model, temperature, tokens, etc.

    orgai->>API: Send request
    API-->>orgai: Stream response

    loop For each chunk
        orgai->>Org: Insert text after block
        Org-->>User: Display update
    end

    orgai-->>User: Execution complete
```

### Block Syntax Processing

```mermaid
graph LR
    subgraph "Input"
        Block[AI Block]
        Options[Block Options]
        Prompt[Prompt Text]
    end

    subgraph "Parsing"
        Parse[Parse Block]
        Extract[Extract Options]
        Validate[Validate Syntax]
    end

    subgraph "Configuration"
        Model[Model Selection]
        Params[Parameters]
        Context[Context Settings]
    end

    subgraph "Execution"
        Build[Build Request]
        Send[Send to API]
    end

    Block --> Parse
    Parse --> Extract
    Extract --> Options
    Extract --> Prompt

    Options --> Validate
    Validate --> Model
    Validate --> Params
    Validate --> Context

    Model --> Build
    Params --> Build
    Prompt --> Build
    Context --> Build

    Build --> Send
```

## Features

### AI Block Types

```mermaid
graph TB
    AIBlock[AI Blocks]

    AIBlock --> Chat[Chat Block]
    AIBlock --> Complete[Completion Block]
    AIBlock --> Image[Image Block]
    AIBlock --> Code[Code Block]

    Chat --> ChatOpt[Options: model, temperature]
    Complete --> CompOpt[Options: max-tokens, stop]
    Image --> ImgOpt[Options: size, n]
    Code --> CodeOpt[Options: language]

    style AIBlock fill:#e1f5ff
    style Chat fill:#d4edda
    style Complete fill:#d4edda
    style Image fill:#d4edda
    style Code fill:#d4edda
```

### Supported Operations

| Operation | Description | API Used |
|-----------|-------------|----------|
| Chat | Interactive conversation | Chat Completions |
| Completion | Text generation | Completions |
| Code Generation | Generate code | Chat/Completions |
| Refactoring | Improve code | Chat Completions |
| Image Generation | Create images | DALL-E |
| Speech-to-Text | Transcribe audio | Whisper |
| Text-to-Speech | Generate audio | TTS |

### Block Options

```elisp
;; Example block with options
#+begin_ai :model "gpt-4" :temperature 0.7 :max-tokens 500
What is the capital of France?
#+end_ai
```

Options include:
- `:model` - Model to use
- `:temperature` - Creativity level (0-2)
- `:max-tokens` - Maximum response length
- `:top-p` - Nucleus sampling
- `:frequency-penalty` - Reduce repetition
- `:presence-penalty` - Encourage new topics

## Setup

### Installation

```elisp
;; Install from MELPA
(use-package org-ai
  :ensure t
  :hook (org-mode . org-ai-mode)
  :config
  ;; Set your OpenAI API key
  (setq org-ai-openai-api-token "your-api-key-here")

  ;; Optional: Set default model
  (setq org-ai-default-chat-model "gpt-4"))
```

### Configuration Options

```elisp
;; Customize behavior
(setq org-ai-default-chat-model "gpt-4"
      org-ai-default-max-tokens 1000
      org-ai-default-temperature 0.7
      org-ai-talk-spoken-input t
      org-ai-talk-say-words-aloud t)

;; Key bindings
(use-package org-ai
  :bind (:map org-mode-map
              ("C-c M-a" . org-ai-mode)
              ("C-c M-c" . org-ai-chat)))
```

## Usage Examples

### Example 1: Simple Chat

```org
#+begin_ai
Explain how closures work in Emacs Lisp.
#+end_ai

;; Press C-c C-c to execute
;; Response appears below the block
```

### Example 2: Code Generation

```org
#+begin_ai :model "gpt-4"
Write an Emacs Lisp function that:
- Takes a list of numbers
- Returns only the even numbers
- Uses lexical binding
#+end_ai
```

### Example 3: Code Review

```org
#+begin_ai :model "gpt-4" :temperature 0.3
Review this code for bugs:

(defun my-function (items)
  (loop for item in items
        collect (process item)))
#+end_ai
```

### Example 4: Image Generation

```org
#+begin_ai :image :size "1024x1024"
A serene landscape with mountains and a lake at sunset
#+end_ai
```

### Workflow Diagrams

#### Chat Workflow

```mermaid
sequenceDiagram
    participant User
    participant Org
    participant orgai as org-ai
    participant OpenAI

    User->>Org: Type #+begin_ai block
    User->>Org: Add prompt
    User->>orgai: C-c C-c

    orgai->>Org: Read block content
    orgai->>orgai: Parse options
    orgai->>OpenAI: Send chat request

    loop Streaming
        OpenAI-->>orgai: Response chunk
        orgai->>Org: Insert text
        Org-->>User: Display
    end

    OpenAI-->>orgai: Complete
    orgai-->>User: Done
```

#### Multi-Turn Conversation

```mermaid
sequenceDiagram
    participant User
    participant Org
    participant orgai
    participant OpenAI

    Note over User,OpenAI: Conversation with context

    User->>orgai: First question
    orgai->>OpenAI: Send with system prompt
    OpenAI-->>orgai: Response 1
    orgai->>Org: Insert response

    User->>orgai: Follow-up question
    Note over orgai: Includes previous context
    orgai->>OpenAI: Send with history
    OpenAI-->>orgai: Response 2
    orgai->>Org: Insert response

    User->>orgai: Third question
    orgai->>OpenAI: Send with full context
    OpenAI-->>orgai: Response 3
    orgai->>Org: Insert response
```

## Integration Patterns

### Pattern 1: Document Enhancement

```mermaid
graph LR
    subgraph "Org Document"
        Draft[Draft Text]
        AIBlock[AI Block]
        Enhanced[Enhanced Text]
    end

    subgraph "Process"
        Request[Request Improvement]
        Generate[Generate Better Version]
    end

    Draft --> AIBlock
    AIBlock --> Request
    Request --> Generate
    Generate --> Enhanced
```

### Pattern 2: Research Workflow

```mermaid
graph TB
    Question[Research Question]

    Question --> Block1[AI Block: Research]
    Block1 --> Response1[Initial Research]

    Response1 --> Block2[AI Block: Elaborate]
    Block2 --> Response2[Detailed Info]

    Response2 --> Block3[AI Block: Summarize]
    Block3 --> Summary[Final Summary]

    Summary --> Note[Org Note]

    style Question fill:#e1f5ff
    style Note fill:#e1f5ff
```

### Pattern 3: Code Development

```mermaid
sequenceDiagram
    participant User
    participant Org
    participant orgai
    participant Test[Testing]

    User->>orgai: Generate function
    orgai-->>Org: Insert code

    User->>orgai: Add error handling
    orgai-->>Org: Update code

    User->>orgai: Write tests
    orgai-->>Org: Insert tests

    User->>Test: Run tests
    Test-->>User: Results

    alt Tests fail
        User->>orgai: Fix bugs
        orgai-->>Org: Corrected code
        User->>Test: Re-run tests
    end
```

## Data Flow

### Request Construction

```mermaid
graph LR
    subgraph "Input"
        Block[AI Block Content]
        Options[Block Options]
        History[Conversation History]
    end

    subgraph "Processing"
        Parse[Parse Block]
        Build[Build Messages]
        Apply[Apply Options]
    end

    subgraph "Request"
        Headers[HTTP Headers]
        Payload[JSON Payload]
    end

    Block --> Parse
    Options --> Parse
    History --> Build

    Parse --> Build
    Build --> Apply

    Apply --> Headers
    Apply --> Payload
```

### Response Integration

```mermaid
graph LR
    subgraph "Response"
        Stream[SSE Stream]
        Chunks[JSON Chunks]
    end

    subgraph "Processing"
        Parse[Parse SSE]
        Extract[Extract Content]
        Format[Format for Org]
    end

    subgraph "Output"
        Insert[Insert After Block]
        Update[Update Display]
        Save[Save to History]
    end

    Stream --> Parse
    Parse --> Chunks
    Chunks --> Extract

    Extract --> Format
    Format --> Insert
    Insert --> Update
    Insert --> Save
```

## Advanced Features

### Context Management

```mermaid
graph TB
    subgraph "Context Sources"
        A[Current Block]
        B[Previous Responses]
        C[Document Headers]
        D[Included Files]
    end

    subgraph "Context Builder"
        E[Collect Context]
        F[Apply Token Limit]
        G[Format Messages]
    end

    subgraph "API Request"
        H[System Message]
        I[User Messages]
        J[Assistant Responses]
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

### Speech Integration

```mermaid
sequenceDiagram
    participant User
    participant orgai
    participant Whisper as Whisper API
    participant Chat as Chat API
    participant TTS as TTS API

    User->>orgai: Start speech input
    orgai->>orgai: Record audio
    User->>orgai: Stop recording

    orgai->>Whisper: Upload audio
    Whisper-->>orgai: Transcribed text

    orgai->>Chat: Send text
    Chat-->>orgai: Response text

    orgai->>TTS: Convert to speech
    TTS-->>orgai: Audio file
    orgai->>User: Play audio
```

## Limitations

- **OpenAI Only**: Currently only supports OpenAI APIs
- **API Key Required**: Requires valid OpenAI API key
- **Network Dependent**: No offline mode
- **Rate Limits**: Subject to OpenAI rate limiting
- **Cost**: API usage incurs costs

## Resources

### Documentation

- [org-ai GitHub Repository](https://github.com/rksm/org-ai)
- [org-ai Documentation](https://github.com/rksm/org-ai#usage)
- [Demo Project](../../tree/master/demo-org-ai)

### Related Documentation

- [Org-mode Manual](https://orgmode.org/manual/)
- [OpenAI API Documentation](https://platform.openai.com/docs)
- [Elisp Development Guide](../../blob/master/docs/elisp-development.md)

### Examples

See the [demo-org-ai](../../tree/master/demo-org-ai) directory for example configurations and workflows.

---

**Navigation**: [Home](Home) | [Architecture](Architecture) | [gptel](Gptel) | [Ellama](Ellama)
