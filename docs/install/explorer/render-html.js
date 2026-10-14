/**
 * Markdown guide → DOM, for the recipe explorer.
 *
 * The guide produced by engine.js is the same pandoc-flavoured Markdown that
 * `make pdf`/`make html` consume. This module:
 *
 *   1. rewrites the pandoc-only syntax (fenced `::: {.tip}` divs,
 *      `[text]{.class}` spans, image attributes, heading attributes, stray
 *      LaTeX lines) into HTML that a CommonMark parser passes through;
 *   2. parses with marked;
 *   3. walks the result and turns the HTML-comment markers into structure:
 *        <!-- +section: X --> … <!-- -section: X -->   → provenance wrappers
 *        <!-- ohpc_if_set VAR --> … <!-- ohpc_fi -->   → optional-step blocks
 *        <!-- ohpc_begin --> … <!-- ohpc_end -->       → recipe.sh regions
 *        <!-- ohpc_command/ohpc_comment … -->           → script-only lines
 *   4. numbers headings (pandoc --number-sections), assigns ids, builds a TOC.
 *
 * Everything here is DOM-only; it does not depend on the engine.
 */

/* ------------------------------------------------------------------------ */
/* 1. pandoc → CommonMark-friendly text                                      */
/* ------------------------------------------------------------------------ */

/** Parse a pandoc attribute string: `.cls #id key=val` */
function parseAttrs(text) {
  const out = { id: null, classes: [], kv: {} };
  for (const tok of text.trim().split(/\s+/).filter(Boolean)) {
    if (tok.startsWith(".")) out.classes.push(tok.slice(1));
    else if (tok.startsWith("#")) out.id = tok.slice(1);
    else if (tok.includes("=")) {
      const [k, v] = tok.split("=", 2);
      out.kv[k] = v.replace(/^"|"$/g, "");
    }
  }
  return out;
}

/** True when `text` is a pandoc attribute list (`.cls #id key=val`), as
 *  opposed to braces that happen to follow a bracket in ordinary prose. */
function isAttrList(text) {
  const toks = text.trim().split(/\s+/).filter(Boolean);
  return toks.length > 0 && toks.every((t) => /^(\.[\w-]+|#[\w-]+|[\w-]+=.+)$/.test(t));
}

/** Pandoc-style slug for heading ids. */
export function slugify(text) {
  return (
    text
      .toLowerCase()
      .replace(/[^\p{L}\p{N}\s_.-]/gu, "")
      .trim()
      .replace(/\s+/g, "-")
      .replace(/^[^\p{L}]+/u, "") || "section"
  );
}

/**
 * @param {string} md
 * @param {object} opts
 * @param {(src: string) => string} opts.resolveImage  map an image src like
 *        `figures/ohpc_logo_blue` to a URL (adding the right extension)
 */
export function pandocToCommonMark(md, { resolveImage }) {
  const out = [];
  let inFence = false;
  for (const raw of md.split("\n")) {
    let line = raw;
    if (/^\s*```/.test(line)) {
      inFence = !inFence;
      out.push(line);
      continue;
    }
    if (inFence) {
      out.push(line);
      continue;
    }
    let m;
    // fenced divs
    if ((m = /^:::+\s*\{([^}]*)\}\s*$/.exec(line))) {
      const a = parseAttrs(m[1]);
      out.push(`<div class="${["fenced", ...a.classes].join(" ")}">`, "");
      continue;
    }
    if (/^:::+\s*$/.test(line)) {
      out.push("", "</div>");
      continue;
    }
    // stray LaTeX (\newpage, \tableofcontents, \vspace*{…})
    if (/^\\[a-zA-Z]+\*?(\{.*\})?\s*$/.test(line)) continue;
    // heading attributes: `# Title {.unnumbered}` / `{#id}`
    if ((m = /^(#{1,6}\s+.*?)\s*\{([^}]*)\}\s*$/.exec(line)) && isAttrList(m[2])) {
      line = `${m[1]} <!--hattr ${m[2].trim()}-->`;
    }
    // images with attributes, and plain images (prefix the path)
    line = line.replace(/!\[([^\]]*)\]\(([^)\s]+)\)(\{([^}]*)\})?/g, (all, alt, src, _b, attrs) => {
      const a = parseAttrs(attrs || "");
      const url = resolveImage(src);
      const style = a.kv.width ? ` style="width:${a.kv.width}"` : "";
      const cls = a.classes.filter((c) => c !== "format-variant");
      const img = `<img src="${url}" alt="${alt.replace(/"/g, "&quot;")}" class="${cls.join(" ")}"${style}>`;
      if (cls.includes("right-align")) return `<div class="right-align">${img}</div>`;
      return alt ? `<figure>${img}<figcaption>${alt}</figcaption></figure>` : img;
    });
    // bracketed spans: [text]{.class}
    line = line.replace(/\[([^\]]+)\]\{([^}]*)\}/g, (all, text, attrs) => {
      if (!isAttrList(attrs)) return all; // e.g. a regex quantifier like [0-9]{3}
      const a = parseAttrs(attrs);
      return `<span class="${a.classes.join(" ")}">${text}</span>`;
    });
    out.push(line);
  }
  return out.join("\n");
}

/* ------------------------------------------------------------------------ */
/* 3. marker comments → structure                                            */
/* ------------------------------------------------------------------------ */

const MARKER = /^\s*(\+section|-section|ohpc_begin|ohpc_end|ohpc_if_set|ohpc_if|ohpc_else|ohpc_fi|ohpc_command|ohpc_comment)\b:?\s*([\s\S]*?)\s*$/;

function el(doc, tag, cls, text) {
  const e = doc.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

/**
 * Wrap the top-level nodes of `root` according to the marker comments.
 * Mutates `root` in place. Returns statistics about the flags encountered.
 */
export function structureMarkers(root, { sourceLink } = {}) {
  const doc = root.ownerDocument;
  const nodes = Array.from(root.childNodes);
  root.textContent = "";
  const stack = []; // { kind, el, name }
  const flags = new Map(); // flag → count of blocks
  let scriptOnly = null; // current <pre class="script-only"> accumulating lines

  const top = () => (stack.length ? stack[stack.length - 1].el : root);
  const push = (kind, element, name) => {
    top().appendChild(element);
    stack.push({ kind, el: element, name });
  };
  const popKind = (kind, name) => {
    const idx = stack.map((s) => s.kind + (name ? ":" + s.name : "")).lastIndexOf(kind + (name ? ":" + name : ""));
    if (idx >= 0) stack.length = idx;
  };
  const flushScriptOnly = () => {
    scriptOnly = null;
  };

  for (const node of nodes) {
    if (node.nodeType === 8 /* comment */) {
      const m = MARKER.exec(node.data);
      if (!m) continue; // other comments: drop
      const [, kind, arg] = m;
      if (kind !== "ohpc_command" && kind !== "ohpc_comment") flushScriptOnly();
      switch (kind) {
        case "+section": {
          const wrap = el(doc, "div", "src");
          wrap.dataset.src = arg;
          const badge = el(doc, "div", "src-badge");
          const link = sourceLink ? sourceLink(arg) : null;
          if (link) {
            const a = el(doc, "a", null, arg);
            a.href = link.href;
            a.target = "_blank";
            a.rel = "noopener";
            a.title = link.title || "View template source";
            badge.appendChild(a);
          } else {
            badge.textContent = arg;
          }
          wrap.appendChild(badge);
          push("section", wrap, arg);
          break;
        }
        case "-section":
          popKind("section", arg);
          break;
        case "ohpc_begin":
          push("recipe", el(doc, "div", "recipe"));
          break;
        case "ohpc_end":
          popKind("recipe");
          break;
        case "ohpc_if_set": {
          const wrap = el(doc, "div", "cond");
          wrap.dataset.flag = arg;
          wrap.dataset.want = "1";
          const label = el(doc, "div", "cond-label");
          label.innerHTML = `optional &mdash; when <code>${escapeHtml(arg)}=1</code>`;
          wrap.appendChild(label);
          flags.set(arg, (flags.get(arg) || 0) + 1);
          push("cond", wrap, arg);
          break;
        }
        case "ohpc_if": {
          const wrap = el(doc, "div", "cond cond-expr");
          wrap.dataset.cond = arg;
          const label = el(doc, "div", "cond-label");
          label.innerHTML = `conditional &mdash; <code>if ${escapeHtml(arg)}</code>`;
          wrap.appendChild(label);
          push("cond", wrap, "if " + arg);
          break;
        }
        case "ohpc_else": {
          const current = [...stack].reverse().find((s) => s.kind === "cond");
          popKind("cond");
          const wrap = el(doc, "div", "cond");
          const label = el(doc, "div", "cond-label");
          if (current?.el.dataset.flag) {
            wrap.dataset.flag = current.el.dataset.flag;
            wrap.dataset.want = "0";
            label.innerHTML = `otherwise &mdash; when <code>${escapeHtml(current.el.dataset.flag)}=0</code>`;
          } else {
            wrap.classList.add("cond-expr");
            wrap.dataset.cond = "else";
            label.innerHTML = `otherwise &mdash; <code>else</code>`;
          }
          wrap.appendChild(label);
          push("cond", wrap, "else");
          break;
        }
        case "ohpc_fi":
          popKind("cond");
          break;
        case "ohpc_command":
        case "ohpc_comment": {
          if (!scriptOnly) {
            scriptOnly = el(doc, "pre", "script-only");
            scriptOnly.title = "Present only in the generated recipe.sh";
            const code = el(doc, "code", "language-bash");
            scriptOnly.appendChild(code);
            top().appendChild(scriptOnly);
          }
          const code = scriptOnly.firstChild;
          code.textContent += (kind === "ohpc_comment" ? "# " + arg : arg) + "\n";
          break;
        }
      }
      continue;
    }
    if (node.nodeType === 3 && !node.data.trim()) continue; // whitespace
    flushScriptOnly();
    top().appendChild(node);
  }
  return { flags };
}

function escapeHtml(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

/* ------------------------------------------------------------------------ */
/* 4. headings: attributes, ids, numbering, TOC                              */
/* ------------------------------------------------------------------------ */

export function processHeadings(root) {
  const counters = [0, 0, 0, 0, 0, 0];
  const used = new Set();
  const toc = [];
  let unnumberedLevel = 0; // headings deeper than an .unnumbered one stay unnumbered
  for (const h of root.querySelectorAll("h1, h2, h3, h4, h5, h6")) {
    const level = Number(h.tagName[1]);
    // pull `<!--hattr …-->` out of the heading
    let attrs = { id: null, classes: [] };
    for (const c of Array.from(h.childNodes)) {
      if (c.nodeType === 8 && c.data.startsWith("hattr ")) {
        attrs = parseAttrs(c.data.slice(6));
        c.remove();
      }
    }
    const text = h.textContent.trim();
    let id = attrs.id || slugify(text);
    let n = 1;
    while (used.has(id)) id = `${attrs.id || slugify(text)}-${n++}`;
    used.add(id);
    h.id = id;
    for (const cls of attrs.classes) h.classList.add(cls);

    let number = null;
    if (attrs.classes.includes("unnumbered")) {
      unnumberedLevel = level;
    } else if (unnumberedLevel && level > unnumberedLevel) {
      /* inside an unnumbered section (pandoc leaves these unnumbered too) */
    } else {
      unnumberedLevel = 0;
      counters[level - 1] += 1;
      for (let i = level; i < counters.length; i++) counters[i] = 0;
      number = counters.slice(0, level).join(".");
      const span = el(root.ownerDocument, "span", "secno", number);
      h.prepend(span);
    }
    toc.push({ level, id, text, number });
  }
  return toc;
}

/* ------------------------------------------------------------------------ */
/* Entry point                                                               */
/* ------------------------------------------------------------------------ */

/**
 * Render the guide Markdown into a detached <article>.
 * @returns {{article: HTMLElement, toc: Array, flags: Map}}
 */
export function renderGuide(markdown, { marked, hljs, resolveImage, sourceLink, document: doc = document }) {
  const text = pandocToCommonMark(markdown, { resolveImage });
  const html = marked.parse(text, { gfm: true, breaks: false });
  const article = doc.createElement("article");
  article.className = "guide";
  article.innerHTML = html;
  const { flags } = structureMarkers(article, { sourceLink });
  const toc = processHeadings(article);
  // conditional blocks whose only content is script-only lines (e.g. the
  // input.local check in the front matter) are themselves script-only
  for (const cond of Array.from(article.querySelectorAll(".cond")).reverse()) {
    const body = Array.from(cond.children).filter((c) => !c.classList.contains("cond-label"));
    if (body.every((c) => c.classList.contains("script-only"))) cond.classList.add("script-only");
  }

  // code blocks: language class, highlight, copy button
  for (const pre of article.querySelectorAll("pre")) {
    const code = pre.querySelector("code");
    if (!code) continue;
    if (!/language-/.test(code.className)) code.classList.add("language-plaintext");
    if (hljs) {
      try {
        hljs.highlightElement(code);
      } catch {
        /* leave plain */
      }
    }
    const btn = el(doc, "button", "copy", "copy");
    btn.type = "button";
    btn.setAttribute("aria-label", "Copy to clipboard");
    pre.appendChild(btn);
  }
  // tables: wrap for horizontal scrolling
  for (const table of article.querySelectorAll("table")) {
    const wrap = el(doc, "div", "table-wrap");
    table.replaceWith(wrap);
    wrap.appendChild(table);
  }
  // external links open in a new tab
  for (const a of article.querySelectorAll("a[href^='http']")) {
    a.target = "_blank";
    a.rel = "noopener";
  }
  return { article, toc, flags };
}

/**
 * Apply runtime flag values to the rendered article: each `.cond[data-flag]`
 * becomes `.on` or `.off` according to the chosen value.
 */
export function applyFlags(article, values) {
  for (const block of article.querySelectorAll(".cond[data-flag]")) {
    const v = values[block.dataset.flag];
    const want = block.dataset.want;
    const known = v !== undefined && v !== null && v !== "";
    block.classList.toggle("on", known && String(v) === want);
    block.classList.toggle("off", known && String(v) !== want);
    block.classList.toggle("unknown", !known);
  }
}
