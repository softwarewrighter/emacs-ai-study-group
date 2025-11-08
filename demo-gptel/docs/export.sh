#!/bin/bash
# Export gptel-babel-demo.org to HTML with options for evaluation

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Parse command line arguments
EVAL_BLOCKS=false
if [ "$1" = "--eval" ] || [ "$1" = "-e" ]; then
    EVAL_BLOCKS=true
fi

if [ "$EVAL_BLOCKS" = true ]; then
    echo "Exporting with LIVE evaluation (requires Ollama running)..."
    echo "This will execute all babel blocks and capture real AI responses."
    echo ""

    # Check if Ollama is running
    if ! curl -s http://localhost:11434/api/tags > /dev/null 2>&1; then
        echo "⚠ WARNING: Ollama doesn't appear to be running!"
        echo "Start it with: ollama serve"
        echo ""
        read -p "Continue anyway? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi

    # Export with babel evaluation
    emacs --batch \
      --eval "(require 'package)" \
      --eval "(package-initialize)" \
      --eval "(require 'ox-html)" \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --visit=gptel-babel-demo.org \
      --eval "(org-babel-execute-buffer)" \
      --funcall org-html-export-to-html
else
    echo "Exporting with PRE-FILLED results (fast, no Ollama needed)..."
    echo "Use --eval flag to execute blocks and capture live results."
    echo ""

    # Export without evaluating (uses existing results)
    emacs --batch \
      --eval "(require 'ox-html)" \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --eval "(setq org-export-use-babel nil)" \
      --visit=gptel-babel-demo.org \
      --funcall org-html-export-to-html
fi

if [ -f "gptel-babel-demo.html" ]; then
    echo ""
    echo "✓ Export successful: gptel-babel-demo.html"
    echo ""
    echo "To view in browser:"
    echo "  open gptel-babel-demo.html      # macOS"
    echo "  xdg-open gptel-babel-demo.html  # Linux"
    echo "  start gptel-babel-demo.html     # Windows"
else
    echo "✗ Export failed"
    exit 1
fi
