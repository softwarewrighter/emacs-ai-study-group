# elisp-dev-mcp

**elisp-dev-mcp** is a Model Context Protocol (MCP) server that enables AI assistants to interact with Emacs Lisp development environments.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Features](#features)
- [Setup](#setup)
- [Usage Examples](#usage-examples)
- [Protocol Details](#protocol-details)
- [Resources](#resources)

## Overview

elisp-dev-mcp implements the Model Context Protocol to expose Emacs Lisp development capabilities to AI assistants like Claude Desktop, enabling programmatic access to Emacs functionality.

### Key Capabilities

- Access and manipulate Emacs Lisp code
- Query Emacs documentation
- Interact with Emacs buffers and processes
- Execute Elisp code in Emacs environment
- Assist with Elisp development workflows
- Bridge between AI assistants and Emacs

## Architecture

### High-Level Architecture

```mermaid
graph TB
    subgraph "MCP Client"
        Client[AI Assistant]
        UI[User Interface]
    end

    subgraph "MCP Server"
        Server[elisp-dev-mcp]
        Handler[Request Handler]
        Protocol[MCP Protocol]
    end

    subgraph "Emacs"
        EmacsProc[Emacs Process]
        EvalEngine[Eval Engine]
        Buffers[Buffers]
        Docs[Documentation]
    end

    subgraph "Tools"
        ReadTool[Read File Tool]
        EvalTool[Eval Code Tool]
        DocTool[Get Docs Tool]
        BufferTool[Buffer Tool]
    end

    Client --> UI
    UI --> Protocol
    Protocol --> Server

    Server --> Handler
    Handler --> ReadTool
    Handler --> EvalTool
    Handler --> DocTool
    Handler --> BufferTool

    ReadTool -.-> EmacsProc
    EvalTool -.-> EvalEngine
    DocTool -.-> Docs
    BufferTool -.-> Buffers

    style Client fill:#e1f5ff
    style Server fill:#d4edda
    style Handler fill:#d4edda
```

### MCP Protocol Flow

```mermaid
sequenceDiagram
    participant Client as MCP Client
    participant Server as elisp-dev-mcp
    participant Emacs

    Note over Client,Emacs: Connection & Discovery

    Client->>Server: Connect (stdio/HTTP)
    Server-->>Client: Connection established

    Client->>Server: List tools
    Server-->>Client: Available tools

    Note over Client,Emacs: Tool Execution

    Client->>Server: Call tool (eval-code)
    Server->>Server: Validate request
    Server->>Emacs: Execute Elisp
    Emacs-->>Server: Result
    Server->>Server: Format response
    Server-->>Client: Return JSON

    Client->>Client: Display result
```

### Tool Architecture

```mermaid
graph TB
    subgraph "Tool Registry"
        Registry[Tool Registry]
    end

    subgraph "Available Tools"
        A[elisp-eval]
        B[elisp-doc]
        C[buffer-read]
        D[buffer-write]
        E[file-read]
        F[package-info]
    end

    subgraph "Tool Implementation"
        G[Input Validation]
        H[Emacs Communication]
        I[Response Formatting]
    end

    Registry --> A
    Registry --> B
    Registry --> C
    Registry --> D
    Registry --> E
    Registry --> F

    A --> G
    B --> G
    C --> G

    G --> H
    H --> I

    style Registry fill:#e1f5ff
    style A fill:#d4edda
    style B fill:#d4edda
    style C fill:#d4edda
```

## Features

### MCP Tools

| Tool | Description | Input | Output |
|------|-------------|-------|--------|
| `elisp-eval` | Evaluate Elisp code | Code string | Evaluation result |
| `elisp-doc` | Get documentation | Symbol name | Doc string |
| `buffer-read` | Read buffer contents | Buffer name | Buffer text |
| `buffer-write` | Write to buffer | Buffer name, text | Success status |
| `file-read` | Read Elisp file | File path | File contents |
| `package-info` | Get package info | Package name | Package metadata |

### Communication Modes

```mermaid
graph LR
    Server[elisp-dev-mcp]

    Server --> Stdio[stdio Mode]
    Server --> HTTP[HTTP Mode]
    Server --> Socket[Socket Mode]

    Stdio --> Process[Process Pipe]
    HTTP --> Rest[REST API]
    Socket --> TCP[TCP Connection]

    style Server fill:#e1f5ff
    style Stdio fill:#d4edda
    style HTTP fill:#d4edda
```

### Protocol Support

```mermaid
graph TB
    MCP[MCP Protocol]

    MCP --> Init[Initialize]
    MCP --> Tools[Tools API]
    MCP --> Resources[Resources API]
    MCP --> Prompts[Prompts API]

    Init --> Handshake[Protocol Handshake]
    Tools --> List[List Tools]
    Tools --> Call[Call Tool]
    Resources --> Read[Read Resource]
    Prompts --> Get[Get Prompt]

    style MCP fill:#e1f5ff
```

## Setup

### Prerequisites

1. Node.js (for MCP server)
2. Emacs (running instance)
3. MCP-compatible client (e.g., Claude Desktop)

### Installation

```bash
# Clone the repository
git clone https://github.com/ppbb/elisp-dev-mcp
cd elisp-dev-mcp

# Install dependencies
npm install

# Build the server
npm run build
```

### Emacs Configuration

```elisp
;; Optional: Configure Emacs to work with MCP server
;; The server communicates with Emacs via process

;; Enable server if needed
(server-start)

;; Configure any specific settings for MCP integration
```

### MCP Client Configuration

For Claude Desktop, add to `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "elisp-dev": {
      "command": "node",
      "args": ["/path/to/elisp-dev-mcp/build/index.js"],
      "env": {
        "EMACS_SERVER_NAME": "server"
      }
    }
  }
}
```

### Configuration Options

```json
{
  "mcpServers": {
    "elisp-dev": {
      "command": "node",
      "args": ["/path/to/elisp-dev-mcp/build/index.js"],
      "env": {
        "EMACS_SERVER_NAME": "server",
        "EMACS_SOCKET_PATH": "/path/to/socket",
        "LOG_LEVEL": "debug"
      }
    }
  }
}
```

## Usage Examples

### Example 1: Evaluate Code

```javascript
// Client request
{
  "method": "tools/call",
  "params": {
    "name": "elisp-eval",
    "arguments": {
      "code": "(+ 2 2)"
    }
  }
}

// Server response
{
  "result": "4"
}
```

### Example 2: Get Documentation

```javascript
// Request
{
  "method": "tools/call",
  "params": {
    "name": "elisp-doc",
    "arguments": {
      "symbol": "defun"
    }
  }
}

// Response
{
  "result": "Define a function with NAME, ARGLIST, and BODY..."
}
```

### Example 3: Read Buffer

```javascript
// Request
{
  "method": "tools/call",
  "params": {
    "name": "buffer-read",
    "arguments": {
      "buffer": "*scratch*"
    }
  }
}

// Response
{
  "result": ";; This buffer is for notes..."
}
```

## Protocol Details

### Request/Response Flow

```mermaid
sequenceDiagram
    participant Client
    participant MCP as MCP Server
    participant Validator
    participant Emacs

    Client->>MCP: JSON-RPC Request
    MCP->>Validator: Validate schema

    alt Valid request
        Validator-->>MCP: Valid
        MCP->>Emacs: Execute command
        Emacs-->>MCP: Result
        MCP->>MCP: Format response
        MCP-->>Client: JSON-RPC Response
    else Invalid request
        Validator-->>MCP: Invalid
        MCP-->>Client: Error response
    end
```

### Tool Call Lifecycle

```mermaid
graph TB
    subgraph "Request Phase"
        A[Receive Request]
        B[Parse JSON-RPC]
        C[Extract Tool Name]
    end

    subgraph "Validation Phase"
        D[Validate Tool Exists]
        E[Validate Arguments]
        F[Check Permissions]
    end

    subgraph "Execution Phase"
        G[Call Tool Handler]
        H[Execute in Emacs]
        I[Capture Result]
    end

    subgraph "Response Phase"
        J[Format Result]
        K[Build JSON Response]
        L[Send to Client]
    end

    A --> B
    B --> C
    C --> D

    D --> E
    E --> F
    F --> G

    G --> H
    H --> I
    I --> J

    J --> K
    K --> L
```

### Error Handling

```mermaid
graph LR
    subgraph "Error Types"
        A[Parse Error]
        B[Tool Not Found]
        C[Invalid Arguments]
        D[Execution Error]
    end

    subgraph "Error Response"
        E[Error Code]
        F[Error Message]
        G[Error Data]
    end

    A --> E
    B --> E
    C --> E
    D --> E

    E --> F
    F --> G
```

## Workflows

### Development Assistant Workflow

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant AI as AI Assistant
    participant MCP as elisp-dev-mcp
    participant Emacs

    Dev->>AI: Ask about Elisp function
    AI->>MCP: elisp-doc request
    MCP->>Emacs: Get documentation
    Emacs-->>MCP: Doc string
    MCP-->>AI: Return docs
    AI-->>Dev: Explain function

    Dev->>AI: Generate code
    AI->>AI: Generate Elisp code
    AI->>MCP: elisp-eval (test)
    MCP->>Emacs: Evaluate code
    Emacs-->>MCP: Result
    MCP-->>AI: Evaluation result
    AI-->>Dev: Here's working code
```

### Code Review Workflow

```mermaid
sequenceDiagram
    participant AI
    participant MCP
    participant Emacs

    Note over AI,Emacs: Review Code Workflow

    AI->>MCP: file-read "package.el"
    MCP->>Emacs: Read file
    Emacs-->>MCP: File contents
    MCP-->>AI: Return code

    AI->>AI: Analyze code
    AI->>MCP: elisp-eval (check syntax)
    MCP->>Emacs: Validate
    Emacs-->>MCP: Syntax OK

    AI->>MCP: elisp-doc (check functions)
    MCP->>Emacs: Get docs
    Emacs-->>MCP: Documentation

    Note over AI: Generate review
```

### Interactive Development

```mermaid
graph TB
    Start[Developer Query]

    Start --> Read[Read Code]
    Read --> Analyze[AI Analysis]
    Analyze --> Suggest[Suggestions]

    Suggest --> Try[Try Changes]
    Try --> Eval[Evaluate]
    Eval --> Check{Works?}

    Check -->|Yes| Save[Save Changes]
    Check -->|No| Refine[Refine Code]

    Refine --> Try
    Save --> Done[Complete]

    style Start fill:#e1f5ff
    style Done fill:#d4edda
```

## Data Flow

### Request Processing

```mermaid
graph LR
    subgraph "Input"
        A[JSON-RPC Request]
        B[Method Name]
        C[Parameters]
    end

    subgraph "Processing"
        D[Route to Handler]
        E[Extract Arguments]
        F[Build Emacs Command]
    end

    subgraph "Execution"
        G[Send to Emacs]
        H[Wait for Response]
        I[Parse Result]
    end

    subgraph "Output"
        J[Format JSON]
        K[Add Metadata]
        L[Return Response]
    end

    A --> B
    B --> C
    C --> D

    D --> E
    E --> F
    F --> G

    G --> H
    H --> I
    I --> J

    J --> K
    K --> L
```

### Emacs Communication

```mermaid
sequenceDiagram
    participant MCP
    participant Client as Emacs Client
    participant Server as Emacs Server

    MCP->>Client: Create connection
    Client->>Server: Connect to server

    loop For each request
        MCP->>Client: Send command
        Client->>Server: Evaluate
        Server->>Server: Execute
        Server-->>Client: Result
        Client-->>MCP: Format result
    end
```

## Advanced Features

### Resource Management

```mermaid
graph TB
    subgraph "Resources"
        A[File Resources]
        B[Buffer Resources]
        C[Documentation Resources]
    end

    subgraph "Resource API"
        D[List Resources]
        E[Read Resource]
        F[Subscribe to Updates]
    end

    A --> D
    B --> D
    C --> D

    D --> E
    E --> F

    style A fill:#d4edda
    style B fill:#d4edda
    style C fill:#d4edda
```

### Prompt Templates

```mermaid
graph LR
    subgraph "Prompts"
        A[Code Review Prompt]
        B[Debugging Prompt]
        C[Documentation Prompt]
    end

    subgraph "Customization"
        D[Parameters]
        E[Context]
        F[Examples]
    end

    A --> D
    B --> D
    C --> D

    D --> E
    E --> F
```

## Security Considerations

### Access Control

```mermaid
graph TB
    Request[Incoming Request]

    Request --> Auth[Authentication]
    Auth --> Perm[Permission Check]

    Perm -->|Allowed| Exec[Execute]
    Perm -->|Denied| Reject[Reject]

    Exec --> Safe[Safety Check]
    Safe -->|Safe| Run[Run Command]
    Safe -->|Unsafe| Block[Block]

    Run --> Result[Return Result]
    Reject --> Error[Return Error]
    Block --> Error

    style Request fill:#e1f5ff
    style Result fill:#d4edda
    style Error fill:#f8d7da
```

### Sandboxing

- Limit file system access
- Restrict dangerous functions
- Validate all inputs
- Monitor resource usage
- Log all operations

## Advantages

- **Standardized Protocol**: Uses MCP standard
- **AI Integration**: Easy AI assistant integration
- **Emacs Access**: Direct Emacs functionality access
- **Extensible**: Easy to add new tools
- **Language Agnostic**: Works with any MCP client

## Limitations

- **Requires Running Emacs**: Emacs must be active
- **Security Concerns**: Remote code execution risks
- **Setup Complexity**: Requires MCP client configuration
- **Network Overhead**: Communication latency
- **Limited to MCP Clients**: Not all AI tools support MCP

## Best Practices

1. **Security First**: Validate all inputs
2. **Error Handling**: Provide clear error messages
3. **Documentation**: Document all tools clearly
4. **Testing**: Test with various clients
5. **Monitoring**: Log operations for debugging

## Resources

### Documentation

- [elisp-dev-mcp GitHub Repository](https://github.com/ppbb/elisp-dev-mcp)
- [MCP Documentation](https://modelcontextprotocol.io/)
- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [Demo Project](../../tree/master/demo-elisp-dev-mcp)

### MCP Resources

- [MCP Getting Started](https://modelcontextprotocol.io/getting-started)
- [MCP Protocol Specification](https://spec.modelcontextprotocol.io/specification/)
- [MCP TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)

### Related Documentation

- [Elisp Development Guide](../../blob/master/docs/elisp-development.md)
- [Emacs Client/Server Documentation](https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html)

### Example Integrations

- [Claude Desktop MCP Setup](https://modelcontextprotocol.io/docs/tools/claude-desktop)
- [Other MCP Servers](https://github.com/modelcontextprotocol/servers)

---

**Navigation**: [Home](Home) | [Architecture](Architecture) | [aider.el](Aider-el) | [gptel](Gptel)
