#!/bin/bash
# Export gptel-babel-demo.org to HTML

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

echo "Exporting gptel-babel-demo.org to HTML..."
echo "Note: Using existing results (blocks are NOT executed during export)"
echo ""
echo "To capture LIVE results:"
echo "  1. Open gptel-babel-demo.org in Emacs"
echo "  2. Start Ollama: ollama serve"
echo "  3. Execute blocks with C-c C-c"
echo "  4. Then export with C-c C-e h h"
echo ""

# Export without evaluating (uses existing results)
emacs --batch \
  --eval "(require 'ox-html)" \
  --eval "(setq org-confirm-babel-evaluate nil)" \
  --eval "(setq org-export-use-babel nil)" \
  --visit=gptel-babel-demo.org \
  --funcall org-html-export-to-html

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
