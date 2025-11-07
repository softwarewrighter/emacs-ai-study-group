#!/bin/bash
# Export gptel-babel-demo.org to HTML

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

echo "Exporting gptel-babel-demo.org to HTML..."

emacs --batch \
  --eval "(require 'ox-html)" \
  --visit=gptel-babel-demo.org \
  --funcall org-html-export-to-html

if [ -f "gptel-babel-demo.html" ]; then
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
