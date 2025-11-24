# LaTeX vs Markdown+Jinja2 Comparison

## Side-by-Side Example

### LaTeX (Current System)

**steps.tex:**
```latex
\documentclass[letterpaper]{article}
\usepackage{common/ohpc-doc}

% Define variables
\newcommand{\baseOS}{Rocky 10}
\newcommand{\OSTree}{EL\_10}
\newcommand{\arch}{x86\_64}
\newcommand{\install}{dnf -y install}
\newcommand{\provisioner}{Warewulf4}
\newcommand{\rms}{SLURM}

% Boolean toggles
\toggletrue{isCentOS}
\toggletrue{isWarewulf4}

\begin{document}

\section{Introduction}
\input{common/intro}

\subsection{Enable \OHPC{} repository}
\input{common/enable_ohpc_repo}

\subsection{Install \SLURM{}}
\input{common/install_slurm}

\end{document}
```

**common/enable_ohpc_repo.tex:**
```latex
To begin, enable use of the \OHPC{} repository...

\iftoggleverb{isCentOS}
\begin{lstlisting}[language=bash]
[sms](*\#*) dnf install http://repos.openhpc.community/...
\end{lstlisting}
\else
\begin{lstlisting}[language=bash]
[sms](*\#*) rpm -ivh http://repos.openhpc.community/...
\end{lstlisting}
\fi
```

### Markdown+Jinja2 (Proposed System)

**config.yaml:**
```yaml
# Variables
baseOS: "Rocky 10"
OSTree: "EL_10"
arch: "x86_64"
install: "dnf -y install"
provisioner: "Warewulf4"
rms: "SLURM"

# Toggles
isCentOS: true
isWarewulf4: true

# Global names
OHPC: "OpenHPC"
SLURM: "Slurm"
```

**steps.md.j2:**
```markdown
# {{ OHPC }} Installation Guide

## Introduction

{% include 'common/intro.md.j2' %}

### Enable {{ OHPC }} repository

{% include 'common/enable_ohpc_repo.md.j2' %}

### Install {{ SLURM }}

{% include 'common/install_slurm.md.j2' %}
```

**common/enable_ohpc_repo.md.j2:**
```markdown
To begin, enable use of the {{ OHPC }} repository...

{% if isCentOS %}
\`\`\`bash
[sms]# dnf install http://repos.openhpc.community/...
\`\`\`
{% else %}
\`\`\`bash
[sms]# rpm -ivh http://repos.openhpc.community/...
\`\`\`
{% endif %}
```

**Makefile:**
```makefile
all: steps.md

steps.md: steps.md.j2 config.yaml
	jinja2 --strict --format=yaml steps.md.j2 config.yaml -o steps.md
```

## Complexity Comparison

| Aspect | LaTeX | Markdown+Jinja2 |
|--------|-------|-----------------|
| Variable definition | `\newcommand{\var}{value}` | `var: "value"` in YAML |
| Variable use | `\var{}` | `{{ var }}` |
| Conditionals | `\iftoggleverb{flag}...\fi` | `{% if flag %}...{% endif %}` |
| Includes | `\input{file}` | `{% include 'file' %}` |
| Code blocks | `\begin{lstlisting}...\end{lstlisting}` | ` ```bash...``` ` |
| Bold text | `\textbf{text}` | `**text**` |
| Italic text | `\emph{text}` or `{\em text}` | `*text*` |
| Inline code | `\texttt{code}` | `` `code` `` |
| Comments | `% comment` | `{# comment #}` |
| Lists | `\begin{itemize}\item...\end{itemize}` | `* item` |
| Links | `\href{url}{text}` | `[text](url)` |

## Build Process Comparison

### LaTeX
```bash
# From recipe directory
make                    # Runs latexmk -pdf steps.tex
# Output: steps.pdf
```

### Markdown+Jinja2
```bash
# From recipe directory
make                    # Runs jinja2 to generate steps.md
make pdf               # (Optional) Runs pandoc to convert to PDF
# Output: steps.md (and optionally steps.pdf)
```

## File Size Comparison

**This POC example:**

| File | LaTeX | Markdown+Jinja2 |
|------|-------|-----------------|
| Main template | steps.tex (285 lines) | steps.md.j2 (97 lines) |
| enable_ohpc_repo | 37 lines | 17 lines |
| install_slurm | ~60 lines | ~37 lines |
| Configuration | Embedded in steps.tex | config.yaml (69 lines) |

**Advantages:**
- Markdown templates are ~40-50% shorter
- Configuration is separate and more readable
- No escaping needed for shell commands
- Simpler conditional syntax

## Readability Example

### LaTeX escape complexity:
```latex
\newcommand{\arch}{x86\_64}
\newcommand{\chrootinstall}{dnf -y --installroot=\$CHROOT install}
```

### Markdown simplicity:
```yaml
arch: "x86_64"
chrootinstall: "dnf -y --installroot=$CHROOT install"
```

## Migration Path

1. **Phase 1**: Create parallel Markdown system (POC - DONE)
2. **Phase 2**: Convert all common/*.tex to *.md.j2
3. **Phase 3**: Create config.yaml for each recipe variant
4. **Phase 4**: Validate all recipes build correctly
5. **Phase 5**: Update documentation and contributor guides
6. **Phase 6**: Deprecate LaTeX system

## Potential Challenges

1. **PDF formatting**: LaTeX excels at PDF generation
   - *Solution*: Use pandoc with custom templates

2. **Complex tables**: LaTeX handles these better
   - *Solution*: Use HTML tables in Markdown or keep simpler

3. **Cross-references**: LaTeX `\ref{}` is powerful
   - *Solution*: Use Markdown anchors or doc framework

4. **Learning curve**: Team knows LaTeX
   - *Solution*: Jinja2 is simpler, similar to many template engines

5. **Script generation**: Custom LaTeX processing exists
   - *Solution*: Post-process Jinja2 comments similarly

## Conclusion

The Markdown+Jinja2 approach offers:
- **Simpler syntax** that's easier to read and write
- **Better version control** with clearer diffs
- **More flexibility** for multiple output formats
- **Standard tooling** that's widely known
- **Reduced maintenance** with less complex templates

The POC demonstrates this is technically feasible and can be done incrementally
without disrupting current documentation workflows.
