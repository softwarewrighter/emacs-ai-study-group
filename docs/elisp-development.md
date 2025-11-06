# Emacs Lisp Development Guide for AI Coding Agents

This guide documents best practices for developing Emacs Lisp code, with special attention to considerations when working with AI coding agents and LLMs.

## Table of Contents

- [Quick Reference](#quick-reference)
- [Working with AI Coding Agents](#working-with-ai-coding-agents)
- [Avoiding Parenthesis Mismatches](#avoiding-parenthesis-mismatches)
- [Lexical Binding](#lexical-binding)
- [Using cl-lib](#using-cl-lib)
- [Naming Conventions](#naming-conventions)
- [Keybinding Conventions](#keybinding-conventions)
- [Code Layout and Formatting](#code-layout-and-formatting)
- [File Structure](#file-structure)
- [Functions and Macros](#functions-and-macros)
- [Documentation](#documentation)
- [Common Pitfalls](#common-pitfalls)

## Quick Reference

### Essential First Line
```elisp
;;; package-name.el --- Brief description -*- lexical-binding: t; -*-
```

### Package Prefix
All public symbols should use your package prefix:
```elisp
;; Good
(defun my-package-do-something ()
  ...)

;; Bad - no prefix
(defun do-something ()
  ...)
```

### Predicate Naming
```elisp
;; Single word: add 'p'
(defun evenp (n) ...)

;; Multiple words: add '-p'
(defun buffer-live-p (buffer) ...)
```

## Working with AI Coding Agents

### Why AI Agents Struggle with Elisp

AI coding agents (like Claude, ChatGPT, Copilot) face unique challenges with Emacs Lisp:

1. **Parenthesis Balancing**: Deep nesting and multiple closing parentheses on one line
2. **Dynamic Scope Legacy**: Older code without lexical binding
3. **Namespace Pollution**: No built-in module system; must manually prefix everything
4. **Special Forms**: Unique evaluation rules that differ from other Lisps

### Best Practices for AI-Assisted Development

#### 1. Use Modern Emacs Lisp Conventions

Always enable lexical binding in new files:
```elisp
;;; my-package.el --- Description -*- lexical-binding: t; -*-
```

This helps AI agents understand variable scoping correctly.

#### 2. Provide Clear Context

When asking an AI agent to modify code:
- Show the full function definition, not just snippets
- Include the package prefix in your instructions
- Specify whether you're working on a major mode, minor mode, or library
- Mention any required dependencies

Example prompt:
```
Please add error handling to the `my-package-fetch-data` function.
This is part of a library that requires `url` and `json`.
Use lexical binding and follow the existing naming convention.
```

#### 3. Request Incremental Changes

Break large refactorings into smaller steps:
```
1. First, extract the data validation into a helper function
2. Then, add the error handling
3. Finally, update the docstring
```

This reduces the chance of parenthesis mismatches and logic errors.

#### 4. Specify Testing Strategy

Always request validation:
```
After making changes, show me how to test this interactively with M-x eval-buffer
and provide a simple test case.
```

#### 5. Use AI-Friendly Code Structure

Write code that's easier for AI to parse and modify:

```elisp
;; Good: Clear structure with named let bindings
(defun my-package-process-data (data)
  "Process DATA and return result."
  (let* ((validated (my-package--validate data))
         (transformed (my-package--transform validated))
         (result (my-package--finalize transformed)))
    result))

;; Harder for AI: Deeply nested anonymous operations
(defun my-package-process-data (data)
  "Process DATA and return result."
  (my-package--finalize
   (my-package--transform
    (my-package--validate data))))
```

### Emacs AI Tools Integration

This repository demonstrates several AI tools for Emacs:
- **gptel**: General LLM client (see `demo-gptel/`)
- **ellama**: Ollama-specific integration (see `demo-ellama/`)
- **org-ai**: AI within org-mode (see `demo-org-ai/`)
- **aider.el**: AI pair programming (see `demo-aider-el/`)
- **elisp-dev-mcp**: Model Context Protocol server (see `demo-elisp-dev-mcp/`)

## Avoiding Parenthesis Mismatches

### Essential Tools

#### 1. Electric Pair Mode (Built-in)
```elisp
(electric-pair-mode 1)
```
Automatically inserts closing parentheses.

#### 2. Show Paren Mode (Built-in)
```elisp
(show-paren-mode 1)
(setq show-paren-delay 0)  ; Instant highlighting
```
Highlights matching parentheses.

#### 3. Smartparens (Recommended)
```elisp
(use-package smartparens
  :ensure t
  :hook (emacs-lisp-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))
```

**Why Smartparens?**
- Keeps parentheses balanced automatically
- Provides structural editing commands
- More forgiving than paredit for beginners
- Works well with AI-generated code

Key commands:
- `C-M-f` / `C-M-b`: Move forward/backward by s-expression
- `C-M-k`: Kill s-expression
- `sp-forward-slurp-sexp`: Pull next element into current list
- `sp-forward-barf-sexp`: Push last element out of current list

#### 4. Paredit (Alternative)
```elisp
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode))
```

More strict than smartparens but prevents invalid states completely.

#### 5. Lispy (Alternative)
```elisp
(use-package lispy
  :ensure t
  :hook (emacs-lisp-mode . lispy-mode))
```

Modal editing for Lisp code with powerful navigation.

### Verification Tools

#### Byte Compilation
Always byte-compile your code to catch errors:
```elisp
M-x byte-compile-file
```

Or check current buffer:
```elisp
M-x emacs-lisp-byte-compile
```

#### Check Parentheses
```elisp
M-x check-parens
```
Verifies all parentheses are balanced in the current buffer.

### Manual Techniques

1. **Count Closing Parens**: When ending a function, count the closing parens:
   ```elisp
   (defun my-func ()
     (let ((x 1))
       (+ x 2)))  ; Three opens = three closes
   ```

2. **Use Indentation**: Proper indentation reveals structure:
   ```elisp
   ;; Good indentation shows the structure
   (defun my-func (arg)
     (when arg
       (let ((x (process arg)))
         (do-something x))))

   ;; Bad indentation hides errors
   (defun my-func (arg)
   (when arg
   (let ((x (process arg)))
   (do-something x)))) ; Too many closing parens!
   ```

3. **Ask AI to Verify**: When uncertain, prompt:
   ```
   Please verify that all parentheses are balanced in this function
   and that the indentation is correct.
   ```

## Lexical Binding

### Always Enable It

**New code should always use lexical binding:**

```elisp
;;; my-package.el --- Description -*- lexical-binding: t; -*-
```

This must be on the **first line** of the file.

### Why Lexical Binding?

1. **Performance**: Faster variable lookups
2. **Correctness**: Predictable closures
3. **Debugging**: Easier to reason about scope
4. **Modern Standard**: Recommended by GNU Emacs team

### Migrating Old Code

To convert dynamic binding code:

1. Add lexical binding declaration
2. Byte-compile the file: `M-x byte-compile-file`
3. Fix any warnings about free variables
4. Declare special variables with `defvar`:

```elisp
(defvar my-package-dynamic-var nil
  "This needs dynamic binding for hooks/advices.")
```

### Lexical vs Dynamic

```elisp
;;; demo.el --- Example -*- lexical-binding: t; -*-

;; Lexical: Captures binding at definition time
(let ((x 10))
  (defun lexical-example ()
    x))  ; Always returns 10

;; Dynamic: Use defvar for intentionally dynamic variables
(defvar dynamic-var 10)
(defun dynamic-example ()
  dynamic-var)  ; Returns current value of dynamic-var
```

## Using cl-lib

### When to Use cl-lib

The `cl-lib` library provides Common Lisp compatibility functions. Use it when:

1. You need data structures beyond lists (e.g., `cl-defstruct`)
2. You want powerful iteration constructs (`cl-loop`)
3. You need set operations (`cl-union`, `cl-intersection`)
4. Built-in Elisp functions are insufficient

### Do NOT Use Old `cl` Package

```elisp
;; WRONG - cl is deprecated
(require 'cl)
(defun my-func ()
  (first my-list))

;; CORRECT - use cl-lib
(require 'cl-lib)
(defun my-func ()
  (cl-first my-list))  ; Or better: (car my-list) for idiom
```

The old `cl` package is obsolete as of Emacs 27.

### Compile-Time vs Runtime

If you only use **macros** from cl-lib, load it at compile-time only:

```elisp
(eval-when-compile
  (require 'cl-lib))

(defun my-func (items)
  (cl-loop for item in items
           collect (process item)))  ; cl-loop is a macro
```

If you use **functions** from cl-lib, require it normally:

```elisp
(require 'cl-lib)

(defun my-func (list1 list2)
  (cl-union list1 list2))  ; cl-union is a function
```

### Most Useful cl-lib Features

```elisp
;; Structures
(cl-defstruct person
  name age email)

(let ((p (make-person :name "Alice" :age 30)))
  (person-name p))  ; => "Alice"

;; Looping
(cl-loop for i from 1 to 10
         when (evenp i)
         collect i)  ; => (2 4 6 8 10)

;; Multiple values
(cl-defun parse-name (full-name)
  (let ((parts (split-string full-name)))
    (cl-values (car parts) (cadr parts))))

(cl-multiple-value-bind (first last)
    (parse-name "John Doe")
  (message "First: %s, Last: %s" first last))

;; Destructuring
(cl-destructuring-bind (a b &optional c &rest rest)
    '(1 2 3 4 5)
  (list a b c rest))  ; => (1 2 3 (4 5))
```

### Idiomatic Preferences

Some cl-lib functions have idiomatic Elisp equivalents:

```elisp
;; Prefer idiomatic Elisp
(car list)          ; Not (cl-first list)
(cdr list)          ; Not (cl-rest list)
(cadr list)         ; Not (cl-second list)

;; Use cl-lib when clearer
(cl-remove-if-not #'evenp numbers)  ; Clearer than filtering manually
(cl-every #'stringp items)          ; Clearer than (seq-every-p #'stringp items)
```

## Naming Conventions

### General Rules

1. **Use lisp-case** (lowercase with hyphens): `my-function-name`
2. **Prefix everything** with your package name: `my-package-function`
3. **Private functions** use double dash: `my-package--private-helper`
4. **Unused variables** start with underscore: `_unused`

### Package Prefixes

Choose a unique, short prefix for your package:

```elisp
;; Package: fancy-buffer
(defun fancy-buffer-create () ...)
(defun fancy-buffer-destroy () ...)
(defvar fancy-buffer-mode-map ...)

;; Private helpers
(defun fancy-buffer--internal-setup () ...)
```

Emacs has no namespaces, so prefixes prevent conflicts.

### Predicates

Functions that return true/false:

```elisp
;; Single word: add 'p'
(defun evenp (n) (zerop (mod n 2)))
(defun listp (obj) ...)
(defun bufferp (obj) ...)

;; Multiple words: add '-p'
(defun buffer-live-p (buffer) ...)
(defun my-package-valid-input-p (input) ...)
(defun remote-file-name-p (filename) ...)
```

### Commands vs Functions

```elisp
;; Interactive command: use verb or verb-noun
(defun my-package-insert-timestamp ()
  "Insert current timestamp at point."
  (interactive)
  ...)

;; Internal function: use noun or descriptive name
(defun my-package--format-timestamp (time)
  "Format TIME as string."
  ...)
```

### Variables

```elisp
;; User options (customizable)
(defcustom my-package-auto-save t
  "Whether to auto-save."
  :type 'boolean
  :group 'my-package)

;; Internal variables
(defvar my-package--cache nil
  "Internal cache.")

;; Constants
(defconst my-package-version "1.0"
  "Version string.")
```

### Avoid Name Collisions

Check for existing names:
```elisp
M-x apropos my-package
```

## Keybinding Conventions

### Reserved Keyspaces

#### For Users (Never Bind These in Packages)

- `C-c` followed by a **letter** (e.g., `C-c a`, `C-c x`)
- `F5` through `F9` without modifiers

These are **sacred** and reserved for end users.

#### For Major Modes

- `C-c` followed by **control character** or **digit**: `C-c C-c`, `C-c C-n`, `C-c 1`
- `C-c` followed by: `{`, `}`, `<`, `>`, `:`, `;`

```elisp
(define-key my-major-mode-map (kbd "C-c C-c") #'my-package-compile)
(define-key my-major-mode-map (kbd "C-c C-n") #'my-package-next)
```

#### For Minor Modes

- `C-c` followed by **punctuation/symbols**: `C-c .`, `C-c ,`, `C-c !`, etc.

```elisp
(define-key my-minor-mode-map (kbd "C-c !") #'my-package-toggle)
```

### Best Practices

1. **Don't override standard keys** unless you have very good reason
2. **Never bind `C-h` after a prefix** (it provides help)
3. **Use the package prefix key pattern**:
   ```elisp
   ;; Good: All commands under C-c p (for "projectile")
   (define-prefix-command 'projectile-command-map)
   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
   (define-key projectile-command-map (kbd "f") #'projectile-find-file)
   (define-key projectile-command-map (kbd "p") #'projectile-switch-project)
   ```

4. **Document keybindings** in the package commentary and docstrings

### Keybinding in use-package

```elisp
(use-package my-package
  :ensure t
  :bind (("C-c m t" . my-package-toggle)
         ("C-c m r" . my-package-reload)
         :map my-package-mode-map
         ("C-c C-c" . my-package-execute))
  :bind-keymap ("C-c m" . my-package-command-map))
```

## Code Layout and Formatting

### Indentation

- **Use spaces only**, never tabs
- Use 2-space indent for function bodies
- Use 4-space indent for special form arguments

```elisp
(defun my-func (arg)  ; 2 spaces for body
  (let ((x 1)
        (y 2))
    (when (and condition
               another)      ; Align continuation
      (do-something x y))))

(if condition
    (progn                   ; 4 spaces for special form args
      (action-1)
      (action-2))
  (alternative))
```

### Line Length

Keep lines under **80 characters** when feasible.

```elisp
;; Good
(defun my-package-long-function-name (arg1 arg2 arg3)
  "Documentation."
  (let ((result (some-operation arg1 arg2)))
    (process result)))

;; Good - break long calls
(my-package-do-something
 first-argument
 second-argument
 third-argument
 fourth-argument)
```

### Parentheses Placement

Place **all closing parens on the same line**:

```elisp
;; CORRECT
(defun my-func ()
  (let ((x 1))
    (when x
      (message "X is %s" x))))

;; WRONG - don't do this
(defun my-func ()
  (let ((x 1))
    (when x
      (message "X is %s" x)
    )
  )
)
```

### Spacing

```elisp
;; Good spacing
(defun my-func (arg)
  (let ((x (process arg))
        (y (other arg)))
    (combine x y)))

;; Bad - inconsistent spacing
(defun my-func(arg)
  (let((x(process arg))
       (y(other arg)))
    (combine x y)))
```

### Blank Lines

Separate top-level forms with **one blank line**:

```elisp
(defvar my-var nil
  "Documentation.")

(defun my-func-1 ()
  "Do something."
  ...)

(defun my-func-2 ()
  "Do another thing."
  ...)
```

## File Structure

### Standard File Layout

```elisp
;;; package-name.el --- Brief description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <email@example.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/user/package-name

;;; Commentary:

;; Longer description of what the package does.
;; Usage examples, etc.

;;; Code:

(require 'other-packages)

(defgroup package-name nil
  "Group documentation."
  :group 'applications)

(defcustom package-name-option t
  "Documentation."
  :type 'boolean
  :group 'package-name)

(defun package-name-function ()
  "Documentation."
  (interactive)
  ...)

(provide 'package-name)
;;; package-name.el ends here
```

### Essential Elements

1. **First line**: Must have file name, description, and lexical-binding
2. **Commentary section**: Explain what the package does
3. **Code section**: All implementation
4. **Provide statement**: Must match file name (without .el)
5. **End comment**: Must match file name

### Multiple Files

For larger packages:

```
my-package/
├── my-package.el          ; Main entry point
├── my-package-core.el     ; Core functionality
├── my-package-utils.el    ; Utilities
└── my-package-tests.el    ; Tests
```

Each file should have proper headers and `(provide 'filename)`.

## Functions and Macros

### Defining Functions

```elisp
(defun my-package-greet (name)
  "Greet NAME with a friendly message.
NAME should be a string."
  (interactive "sName: ")
  (message "Hello, %s!" name))
```

Always include:
1. Package prefix
2. Docstring (required for interactive commands)
3. `(interactive)` if it's a command

### Interactive Specifications

```elisp
(interactive)              ; No arguments
(interactive "sPrompt: ")  ; String
(interactive "nNumber: ")  ; Number
(interactive "p")          ; Prefix argument as number
(interactive "P")          ; Raw prefix argument
(interactive "r")          ; Region start and end
```

### Anonymous Functions (Lambdas)

```elisp
;; Good: Simple, inline use
(mapcar (lambda (x) (* x 2)) numbers)

;; Good: Sharp-quote for named functions
(mapcar #'1+ numbers)

;; Bad: Don't hard-quote lambdas
'(lambda (x) (* x 2))  ; WRONG

;; Bad: Unnecessarily wrapping
(lambda (x) (evenp x))  ; Use #'evenp instead
```

### Defining Macros

```elisp
(defmacro my-package-with-timing (form)
  "Execute FORM and report execution time."
  (declare (indent 0) (debug t))
  (let ((start-var (gensym "start-")))
    `(let ((,start-var (current-time)))
       ,form
       (message "Elapsed: %s" (float-time (time-since ,start-var))))))

;; Usage
(my-package-with-timing
  (sleep-for 1))
```

**Always** include:
- `declare` forms for indentation and debugging
- Proper gensym for internal variables
- Backquote (`) and unquote (,) syntax

### Macro Best Practices

1. Write function examples first
2. Only use macros when functions won't work (need to control evaluation)
3. Use `gensym` to avoid variable capture
4. Add `(debug t)` declaration for edebug support
5. Test with `macroexpand`

## Documentation

### Docstrings

**Required for:**
- All interactive commands
- All user-facing functions
- All customizable variables

**First line:**
- Must be complete sentence
- Should fit on one line if possible
- Ends with period

```elisp
(defun my-package-save-buffer ()
  "Save current buffer to its file.

If the buffer is not visiting a file, prompt for a filename.
With prefix argument, save all buffers without confirmation."
  (interactive)
  ...)
```

### Docstring Conventions

```elisp
;; Reference arguments in CAPITALS
(defun my-func (arg1 arg2)
  "Process ARG1 and ARG2 together."
  ...)

;; Use \\= to escape
(defun my-func ()
  "This uses \\[my-func] to show keybinding."
  ...)

;; Format return values
(defun my-func ()
  "Do something useful.
Returns a list of (NAME . VALUE) pairs."
  ...)
```

### File Comments

```elisp
;;; Three semicolons for file-level or section headers

;;; Commentary:

;;; Configuration:

;; Two semicolons for block comments or explanations
;; that span multiple lines or introduce code sections.

(defun my-func ()
  ;; One semicolon for margin comments
  (+ 1 2))  ; Or side comments
```

### TODOs and FIXMEs

```elisp
;; TODO(yourname): Add error handling here
;; FIXME(yourname): This doesn't work with remote files
;; OPTIMIZE(yourname): Could use hash table instead
;; HACK(yourname): Temporary workaround until upstream fixes bug
```

## Common Pitfalls

### 1. Forgetting Lexical Binding

```elisp
;; WRONG - no lexical binding
;;; my-package.el --- Description

;; CORRECT
;;; my-package.el --- Description -*- lexical-binding: t; -*-
```

### 2. Missing Package Prefix

```elisp
;; WRONG - will conflict with other packages
(defun open-file () ...)

;; CORRECT
(defun my-package-open-file () ...)
```

### 3. Using Deprecated `cl`

```elisp
;; WRONG
(require 'cl)
(loop for i from 1 to 10 collect i)

;; CORRECT
(require 'cl-lib)
(cl-loop for i from 1 to 10 collect i)
```

### 4. Binding User-Reserved Keys

```elisp
;; WRONG - C-c LETTER is reserved for users
(define-key my-mode-map (kbd "C-c a") #'my-action)

;; CORRECT - use control char or punctuation
(define-key my-mode-map (kbd "C-c C-a") #'my-action)
(define-key my-mode-map (kbd "C-c !") #'my-action)
```

### 5. Unbalanced Parentheses

**Prevention:**
- Use smartparens or paredit
- Run `M-x check-parens`
- Byte-compile regularly
- Use proper indentation

### 6. Not Testing Interactively

Always test your code:
```elisp
M-x eval-buffer          ; Load entire buffer
M-x eval-defun           ; Load function at point
M-x ielm                 ; Interactive Elisp REPL
```

### 7. Ignoring Byte-Compile Warnings

```elisp
M-x byte-compile-file
```

Fix all warnings - they usually indicate real problems.

### 8. Hard-Quoting Lambdas

```elisp
;; WRONG
'(lambda (x) (* x 2))

;; CORRECT
(lambda (x) (* x 2))

;; BETTER - use sharp-quote for named functions
#'my-function
```

### 9. Using progn in if/when

```elisp
;; WRONG - progn is implicit
(when condition
  (progn
    (do-this)
    (do-that)))

;; CORRECT
(when condition
  (do-this)
  (do-that))
```

### 10. Not Providing Package

```elisp
;; WRONG - file ends abruptly

;; CORRECT - always end with provide
(provide 'my-package)
;;; my-package.el ends here
```

## Additional Resources

### Official Documentation
- [GNU Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [Coding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html)
- [Key Binding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html)

### Style Guides
- [Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide) by Bozhidar Batsov

### Tools
- [package-lint](https://github.com/purcell/package-lint) - Check for common package mistakes
- [flycheck](https://www.flycheck.org/) - On-the-fly syntax checking
- [elisp-lint](https://github.com/gonewest818/elisp-lint) - Linting and style checking

### Learning Resources
- [An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/)
- [EmacsWiki](https://www.emacswiki.org/)
- [Emacs StackExchange](https://emacs.stackexchange.com/)

## Contributing to This Guide

This guide is part of the Emacs AI Study Group repository. If you discover additional best practices for working with AI coding agents, please contribute by:

1. Testing the recommendations in this guide
2. Documenting what works well with different AI tools
3. Adding examples of common AI-generated errors and how to fix them
4. Sharing prompts that produce better Elisp code

---

**Last Updated**: 2025-11-06
**Maintained by**: Emacs AI Study Group
