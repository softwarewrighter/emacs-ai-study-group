# GitHub Wiki Pages

This directory contains the GitHub Wiki pages for the Emacs AI Study Group project.

## Contents

- **Home.md** - Main wiki landing page with overview and navigation
- **Architecture.md** - Comprehensive architecture documentation with Mermaid diagrams
- **Gptel.md** - gptel component documentation
- **Org-AI.md** - org-ai component documentation
- **Ellama.md** - Ellama component documentation
- **Aider-el.md** - aider.el component documentation
- **Elisp-Dev-MCP.md** - elisp-dev-mcp component documentation
- **_Sidebar.md** - Wiki sidebar navigation

## Features

All wiki pages include:

- **Mermaid Diagrams**: Architecture diagrams, sequence diagrams, data flow diagrams
- **Component Details**: Features, setup instructions, usage examples
- **Cross-References**: Links between pages and to repository files
- **Navigation**: Sidebar for easy access to all content

## How to Set Up the GitHub Wiki

GitHub wikis are stored in a separate git repository. To upload these pages to the GitHub wiki:

### Option 1: Manual Upload (Web Interface)

1. Go to your repository on GitHub: `https://github.com/softwarewrighter/emacs-ai-study-group`
2. Click on the "Wiki" tab
3. Click "Create the first page" to initialize the wiki
4. For each markdown file in this directory:
   - Click "New Page"
   - Copy the filename (without .md extension) as the page title
   - Paste the file contents
   - Click "Save Page"

### Option 2: Git Clone and Push

1. First, create the wiki through the web interface (see Option 1, step 3)

2. Clone the wiki repository:
   ```bash
   git clone https://github.com/softwarewrighter/emacs-ai-study-group.wiki.git
   cd emacs-ai-study-group.wiki
   ```

3. Copy all the markdown files from this directory:
   ```bash
   cp ../wiki-pages/*.md .
   ```

4. Commit and push:
   ```bash
   git add .
   git commit -m "Add comprehensive wiki documentation with architecture diagrams"
   git push
   ```

### Option 3: Direct Push (If Wiki Exists)

If the wiki repository already exists:

```bash
cd wiki-pages
git init
git add .
git commit -m "Add comprehensive wiki documentation"
git remote add origin https://github.com/softwarewrighter/emacs-ai-study-group.wiki.git
git push -u origin master
```

## Viewing the Wiki

Once uploaded, view the wiki at:
`https://github.com/softwarewrighter/emacs-ai-study-group/wiki`

## Maintaining the Wiki

To update the wiki:

1. Edit the files in this directory
2. Commit changes to the main repository
3. Copy updated files to the wiki repository
4. Push to the wiki

Alternatively, you can edit directly in the wiki repository if you've cloned it.

## Link Format Notes

The wiki pages use GitHub-specific link formats:

- **Wiki page to wiki page**: `[Link Text](Page-Name)`
- **Wiki page to repository file**: `[Link Text](../../blob/master/path/to/file)`
- **Sidebar links**: Same format as page-to-page links

## Mermaid Diagrams

All diagrams are created using Mermaid syntax and will render automatically in GitHub's wiki. The diagrams include:

- System architecture diagrams
- Component interaction flows
- Sequence diagrams for workflows
- Data flow diagrams
- Decision trees
- State diagrams

Note: HTML elements (like `<br>`) are not used in Mermaid diagrams per GitHub requirements.

## Contributing

To improve the wiki documentation:

1. Edit the markdown files in this directory
2. Test Mermaid diagrams at [Mermaid Live Editor](https://mermaid.live/)
3. Ensure all internal links work correctly
4. Update this README if adding new pages
5. Commit and push changes
