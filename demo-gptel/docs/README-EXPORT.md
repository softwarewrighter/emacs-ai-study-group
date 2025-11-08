# Exporting gptel Babel Demo to HTML

This directory contains `gptel-babel-demo.org`, an interactive demonstration of using gptel with Org babel blocks that can be exported to HTML.

## Two Export Modes

### Mode 1: Fast Export with Pre-filled Results (No Ollama Needed)

This mode exports the document with example results already embedded. **No evaluation happens**, so it's fast and doesn't require Ollama to be running.

```bash
./export.sh
```

### Mode 2: Live Export with Real AI Results (Requires Ollama)

This mode executes all babel blocks and captures REAL responses from your Ollama instance.

```bash
# 1. Start Ollama
ollama serve

# 2. Export with evaluation
./export.sh --eval
```

This will:
- Execute all babel blocks
- Capture live AI responses
- Export to HTML with YOUR actual results
- Take several minutes (each AI call takes 30-60 seconds)

## Quick Export Methods

### Option 1: From Emacs (Interactive)

1. Open the org file:
   ```
   C-x C-f gptel-babel-demo.org RET
   ```

2. Export to HTML:
   ```
   C-c C-e h h
   ```
   This exports and opens in your default browser.

### Option 2: Using Export Script (Interactive)

```elisp
;; In Emacs
M-x load-file RET export-to-html.el RET
M-x gptel-demo-export-and-open RET
```

### Option 3: Batch Export (Command Line)

```bash
cd demo-gptel/docs
emacs --batch -l export-to-html.el
```

This creates `gptel-babel-demo.html` in the same directory.

### Option 4: Quick Bash Script (Pre-filled)

```bash
cd demo-gptel/docs
./export.sh
```

### Option 5: With Live Evaluation

```bash
cd demo-gptel/docs
./export.sh --eval  # Requires Ollama running
```

## Files

- **gptel-babel-demo.org** - The main org-mode document with babel blocks
- **style.css** - Custom CSS styling for HTML export
- **export-to-html.el** - Elisp export script
- **gptel-babel-demo.html** - Generated HTML (after export)

## Customizing the Export

### Change HTML Theme

Edit `style.css` to customize colors, fonts, layout, etc.

### Modify Export Settings

In `export-to-html.el`, you can configure:

```elisp
(setq org-html-htmlize-output-type 'css)        ; Use CSS for syntax highlighting
(setq org-html-html5-fancy t)                   ; Use HTML5 elements
(setq org-export-with-toc t)                    ; Include table of contents
(setq org-export-with-section-numbers t)        ; Number sections
```

### Add Custom Header/Footer

In the org file, add:

```org
#+HTML_HEAD: <link rel="stylesheet" href="custom.css" />
#+HTML_HEAD: <script src="custom.js"></script>
```

## Understanding the Two Workflows

### Workflow 1: Share Example Results (Fast)

Perfect for:
- Sharing documentation without requiring Ollama
- Quick previews
- Demonstrating what the tool can do

```bash
./export.sh  # Uses pre-filled example results
```

**Pros:** Fast (< 10 seconds), no dependencies, consistent results
**Cons:** Shows example data, not your actual AI responses

### Workflow 2: Capture Live Results (Slow)

Perfect for:
- Documenting YOUR specific AI interactions
- Testing different models
- Creating tutorials with real examples
- Research and analysis

```bash
ollama serve  # In one terminal
./export.sh --eval  # In another (takes 5-10 minutes)
```

**Pros:** Real AI responses, your actual results
**Cons:** Requires Ollama running, takes time, results vary

### Interactive Execution

You can also execute blocks one at a time in Emacs:

```elisp
;; In Emacs, with gptel-babel-demo.org open:
C-c C-c           ; Execute current block
M-x org-babel-execute-buffer RET  ; Execute all blocks
```

This lets you:
- Test individual interactions
- Modify prompts and re-run
- Debug issues
- Build up results incrementally

## Publishing to a Website

### Option 1: Single File Export

Just copy the generated HTML and CSS files to your web server:

```bash
cp gptel-babel-demo.html style.css /path/to/webserver/
```

### Option 2: Org Publish Project

Create an org publish configuration in your Emacs init file:

```elisp
(require 'ox-publish)

(setq org-publish-project-alist
      '(("gptel-demo"
         :base-directory "~/emacs-ai-study-group/demo-gptel/docs/"
         :publishing-directory "~/public_html/gptel-demo/"
         :publishing-function org-html-publish-to-html
         :recursive t
         :html-head-include-default-style nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "gptel Demos")))
```

Then publish:

```elisp
M-x org-publish RET gptel-demo RET
```

### Option 3: GitHub Pages

1. Export to HTML
2. Commit the HTML and CSS files
3. Enable GitHub Pages in repository settings
4. Access at: `https://username.github.io/repository/demo-gptel/docs/gptel-babel-demo.html`

## Viewing Results

The exported HTML includes:

- ✓ Table of contents
- ✓ Syntax-highlighted code blocks
- ✓ Formatted results sections
- ✓ Responsive design (mobile-friendly)
- ✓ Clean, professional styling
- ✓ Working internal links

## Troubleshooting

### "org-html-export-to-html not found"

Install the Org export backend:

```elisp
M-x package-install RET org RET
```

### Code blocks not syntax highlighted

The export uses CSS for highlighting. Make sure `style.css` is in the same directory as the HTML file.

### Results not showing

The `:eval no` tags in the org file prevent automatic execution. To include real results:

1. Remove `:eval no` from babel blocks
2. Start Ollama: `ollama serve`
3. Execute blocks: `M-x org-babel-execute-buffer`
4. Export: `C-c C-e h h`

Or use the mock framework for demonstration purposes.

### Browser not opening

Check the `browse-url` configuration:

```elisp
;; Use default browser
(setq browse-url-browser-function 'browse-url-default-browser)

;; Or specify browser
(setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-browser-function 'browse-url-chrome)
```

## Advanced: CI/CD Export

You can automate HTML generation with GitHub Actions:

```yaml
name: Export Org to HTML

on:
  push:
    paths:
      - 'demo-gptel/docs/*.org'

jobs:
  export:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Install Emacs
        run: sudo apt-get install -y emacs

      - name: Export to HTML
        run: |
          cd demo-gptel/docs
          emacs --batch -l export-to-html.el

      - name: Commit HTML
        run: |
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add demo-gptel/docs/*.html
          git commit -m "Auto-export org to HTML" || true
          git push
```

## Resources

- [Org Export Manual](https://orgmode.org/manual/Exporting.html)
- [Org Babel Manual](https://orgmode.org/manual/Working-with-Source-Code.html)
- [Org HTML Export](https://orgmode.org/manual/HTML-Export.html)
- [gptel Documentation](https://github.com/karthink/gptel)

---

**Last Updated:** 2025-11-07
**Part of the [Emacs AI Study Group](../../README.md)**
