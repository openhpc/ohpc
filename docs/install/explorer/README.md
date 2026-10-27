# Recipe explorer

A static, client-side page that renders the OpenHPC install guides in the
browser straight from the recipe sources in `docs/install/`: pick a base OS,
architecture, provisioner and scheduler (the four axes a `recipes/*.conf` is
named by), choose the optional features you plan to enable, and read the guide
for exactly that path — with the `recipe.sh` and an `input.local` to download.

The published site is <https://openhpc.github.io/ohpc/>. The PDF/HTML that
`make pdf`/`make html` produce remain the reference rendering; this page is a
different *view* of the same templates, not a second source.

## Running it

```bash
cd docs/install
make explorer                      # writes explorer/manifest.json
python3 -m http.server 8000        # any static server rooted at docs/install
open http://localhost:8000/explorer/
```

Deploying is copying `docs/install/` (minus `build/` and `.venv/`) to a static
host. The page needs only the files it fetches: `explorer/`, `templates/`,
`manifests/`, `config/`, `recipes/` and `input.local.template`. It must be
served over HTTP — `file://` blocks the fetches and the ES module.

A path is linkable: `#rocky10-x86_64-warewulf-slurm?enable_ib=1&skipped=1`. Each
heading's Copy link button adds `chapter=<heading-id>` to deep-link into the
guide; the parameter is applied once when the guide renders, so later option
changes do not jump the reader back.

Trust boundary: the page injects template-derived HTML (the same first-party
content that becomes the PDF) without sanitising; nothing from the URL reaches
`innerHTML`. Host it on its own origin rather than one shared with unrelated
pages, as a template change could then script those too.

## How it works

```text
recipes/<name>.conf ──▶ config/*.yaml (+ recipes/<name>.yaml) ──▶ merged config
                                                                      │
templates/chapters/main.md.j2 ──▶ nunjucks (Jinja2 for JS) ◀──────────┘
                                        │
                                        ▼
                               guide Markdown ──▶ extractScript() ──▶ recipe.sh
                                        │
                                        ▼
                     render-html.js: pandoc syntax → marked → DOM structure
```

| File | Role |
| --- | --- |
| `engine.js` | Port of `../Makefile` + `../mkdoc.py`: config merge (yq `*` semantics), the `<!-- +section -->` include markers, macro loading, `tojson`, script extraction, line-length warnings. Environment-agnostic (fetch in the page, `fs` under node). |
| `render-html.js` | Guide Markdown → DOM: rewrites the pandoc-only syntax (`::: {.tip}`, `[x]{.class}`, image/heading attributes, stray LaTeX), parses with marked, then turns the HTML-comment markers into structure — provenance wrappers per template, optional-step blocks per `ohpc_if_set` flag, `recipe.sh` regions, script-only lines — and numbers the headings. |
| `app.js` | The page: chooser, URL state, sidebar (TOC, flags, view switches, downloads). |
| `mksite.py` | Assembles a **multi-version site**: `git archive` of each ref's `docs/install` into `<out>/<label>/`, its `manifest.json`, and one `<out>/versions.json` for the page's version switcher. Exports committed trees only (not the working copy). See "Versions" below. |
| `manifest.json` | Written by `../mkdoc.py --manifest` (`make explorer`): recipe list, loader file list, `enable_*` flags and pick-one settings (below) from `input.local.template`, git revision. A static host has no directory listing, so this is the one index the page needs. Git-ignored. |
| `vendor/` | nunjucks 3.2.4 (unminified, see below), js-yaml 4.1.0, marked 15.0.7, highlight.js 11.11.1 + two themes. No build step, no CDN at runtime. |
| `test/parity.mjs` | Renders every recipe under node and diffs the Markdown and script byte-for-byte against `../build/`. **Run it after any change to engine.js or mkdoc.py.** It stamps each render with the revision in `build/<name>.yaml`, so a `build/` older than HEAD still compares. |
| `test/stack.mjs` | Renders every recipe under node and fails if the JS stack at any template load exceeds 200 frames (see "Includes as calls" below). |
| `test/e2e.py` | Playwright check of the page: chooser, a path, flag toggles, downloads, provenance hover, dark mode, phone width, every recipe. Serves the tree from disk through request interception, so no web server is needed. `--browser firefox` or `webkit` runs it in the other engines, `--channel moz-firefox` in the installed Firefox. |

```bash
make && make explorer && node explorer/test/parity.mjs       # engine parity
node explorer/test/stack.mjs                                 # stack depth
python3 explorer/test/e2e.py [--quick] [--url http://host/]  # page (needs playwright + chromium)
```

### Jinja2 ↔ nunjucks

nunjucks is close to Jinja2 but not identical. The engine covers the
differences the templates actually hit; keep this list in sync with
`mkdoc.py`:

- **Include markers.** `mkdoc.py`'s `SectionCommentExtension` wraps every
  `{% include %}` in `<!-- +section: X -->`/`<!-- -section: X -->`;
  `preprocessIncludes()` performs the same text rewrite before compilation.
- **Macros as globals.** `macros.j2` is rendered once with the merged config and
  its exports are installed as globals, as `env.globals.update(module.__dict__)`
  does in Python. The last-definition-wins override pattern works unchanged.
- **`tojson`** is added, with Python's `, ` separators and HTML-safe escapes.
- **String escapes.** nunjucks drops the backslash from unknown escapes
  (`"\*"` → `*`); Jinja keeps it. `patchNunjucksEscapes()` restores Jinja's
  rule — this is why `vendor/nunjucks.js` is the unminified bundle (the method
  name survives).
- **Synchronous loader.** nunjucks' async mode loses the output of an include
  nested in an `{% if %}/{% elif %}` chain (seen with 3.2.4), which the chapter
  aggregators use. Templates are therefore rendered from an in-memory cache; a
  miss raises `NeedsFetch`, the engine fetches and retries. `manifest.json`'s
  file list lets the page warm the cache up front.
- **Includes as calls.** nunjucks runs the rest of a template inside the
  callback of each `{% include %}`, so the stack grows with every include
  rendered: about 2700 frames for a whole guide, which Firefox refuses with
  "too much recursion". `preprocessIncludes()` therefore turns each include
  into a call to a global that renders the child with the caller's variables
  and returns its text, which keeps the stack to the include depth.
  `test/stack.mjs` holds it there.
- `autoescape` off, undefined variables throw, trailing newlines kept — the
  `mkdoc.py` settings.

**Where the browser is more permissive than `make`** (nunjucks semantics the
engine cannot change — the parity test is what catches a template that
depends on them, so keep it in CI):

- `{% if nope %}` with an undefined name: Jinja's `StrictUndefined` fails the
  build; nunjucks only throws on `{{ }}` output, so the branch is silently
  false. A typo in a condition renders cleanly here and fails `make`.
- Empty lists/maps are truthy in nunjucks (`{% if [] %}`), falsy in Jinja. No
  list-valued config exists today.
- `10/5` renders `2` not `2.0`; `.replace()` on a string replaces the first
  match only, not all.
- String literals: `\xHH`, `\uHHHH`, `\UHHHHHHHH` and Python's simple escapes
  are decoded; octal and `\N{NAME}` are not (kept verbatim).
- `tojson` sorts keys and `\u`-escapes non-ASCII like `json.dumps`, but a float
  `1.0` renders `1`.
- YAML: js-yaml speaks YAML 1.2, PyYAML 1.1 — `yes`/`no` are strings here and
  booleans in Python, `4.0` loads as `4`, bare dates become `Date` objects.
  Quote anything in `config/` that is not a plain integer or string.

### What the page adds on top of the Markdown

- **Optional steps.** `<!-- ohpc_if_set enable_x -->` blocks become cards that
  fold when the flag is off; the sidebar toggles are the `enable_*` flags from
  `input.local.template`, and the downloaded `input.local` carries the choice.
  Clicking a folded card's label enables the feature; "preview optional
  features that are off" unfolds their steps, dimmed, without enabling them.
- **Pick-one settings.** A setting with a fixed set of values declares them
  as a JSON list on the line before it in `input.local.template`:
  `modules_package_choices='["lmod-ohpc","environment-modules-ohpc"]'`, then
  `modules_package="${modules_package:-lmod-ohpc}"`. The default is the one in
  `${name:-default}`, which must be in the list; the list's order does not
  matter. `mkdoc.py --manifest` fails on a list that is not JSON, not
  followed by its setting, or without the default. The sidebar shows a
  selector, the hash carries the pick (`?modules_package=…`; a value not in
  the list is ignored), and the downloaded `input.local` gets it as the
  default. The guide's commands show the pick as the default too
  (`${modules_package:-environment-modules-ohpc}`), so a pasted command uses
  it and an `input.local` that sets the variable still wins; the recipe.sh
  and Markdown downloads stay the build output, and recipe.sh gets the pick
  from the downloaded `input.local`. A pick edits the affected code blocks in
  place rather than re-rendering the guide, so the reader does not move.
- **Developer switches** (a collapsed "Developer" section, off by default):
  "show lines only in recipe.sh" shows the `ohpc_command`/`ohpc_comment` lines
  that exist only in the script; "show template source files" adds a hover
  badge naming the template that produced a paragraph, linked to the file;
  "show recipe conditions" labels the general `ohpc_if CONDITION` blocks
  ("recipe condition — if …"), which are shown but not evaluated. Code blocks
  that go into recipe.sh are tagged "in recipe.sh" either way.

**Labels say what they control, in the same words.** A switch and the thing it
shows share their wording, so a reader can tell which switch made what
appear: "show lines only in recipe.sh" shows blocks tagged "only in
recipe.sh", "show recipe conditions" the "recipe condition" labels,
"preview optional features that are off" the folded cards under "Optional
features"; flags whose steps exist only in recipe.sh sit in a collapsed group
labelled "only in recipe.sh", and the script download is
labelled recipe.sh like the tags. Keep a new switch or label to that rule. (The
feature checkboxes drop the `enable_` prefix the cards show, to keep the list
short.)

The marker comments are read as **top-level** nodes of the rendered guide:
keep them at column 0, outside fenced `:::` divs and list items, and properly
nested (`ohpc_begin … ohpc_if_set … ohpc_fi … ohpc_end`), as the current
templates do. A marker inside a tip or a list is left as an invisible comment.

## Versions

Adrian's question — telling a release's guide from the development one — is a
build-side concern, and the page stays a single tree. `mksite.py` lays out one
`docs/install` per git ref side by side, and the page probes for a
`versions.json` two levels above itself; when it finds one with two or more
entries it shows a version switcher in the masthead, otherwise nothing changes.
A bare checkout served as today never has one.

```bash
python3 explorer/mksite.py --out ../../_site 4.x v4.1.GA=4.1
```

A ref is `REF` or `REF=LABEL`. The label is what the switcher shows and the
directory the ref is published under, so the tag `v4.1.GA` above lands in
`4.1/explorer/`; without one the ref names both (`4.x/explorer/`). Two refs
that would share a directory are refused.

- **The explorer that renders a ref is the one committed in that ref**, so its
  `engine.js` matches that ref's `mkdoc.py`. `--allow-overlay` copies the
  current explorer onto a ref that predates it (flagged `explorer_overlaid` in
  `versions.json`); the guide text renders, but script section markers can
  differ from that release's `recipe.sh` where its `mkdoc.py` was older.
- **The site build is managed from the tip.** `.github/workflows/explorer-pages.yml`
  runs `mksite.py` from the branch that triggered it, which becomes the first,
  default entry; released refs are listed in its `RELEASE_REFS`. A docs-only
  tag (say `v4.1.GA.docs`) is just another ref there.
- Switching keeps the hash (recipe and flags); a recipe that does not exist in
  the other version falls back to its chooser.
- `e2e.py --root <site> --prefix <label>/` exercises the switcher against a
  `mksite.py` output.

## Known limits

- Section numbering is computed here, not by pandoc; it matches
  `--number-sections` for the current templates (unnumbered Legal Notice,
  numbered everything else) but is not cross-checked by the tests.
- `ohpc_if` with an arbitrary shell condition is displayed, not evaluated.
- The page reads the working tree it is served from; the revision stamp comes
  from `manifest.json`, so regenerate it when deploying a new commit.
