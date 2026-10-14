/**
 * OpenHPC recipe explorer — rendering engine.
 *
 * A browser-side port of the build pipeline in ../Makefile + ../mkdoc.py:
 *
 *   recipes/<name>.conf  →  merge config/*.yaml (+ recipes/<name>.yaml)
 *                        →  render templates/chapters/main.md.j2 (Jinja2 → nunjucks)
 *                        →  Markdown guide  +  extracted recipe.sh
 *
 * The module is environment-agnostic: the caller injects `nunjucks`, `jsyaml`
 * and a `readText(relPath)` function (fetch in the browser, fs under node), so
 * the same code runs in the page and in the node-side parity test
 * (test/parity.mjs), which checks the output byte-for-byte against the Python
 * build in ../build/.
 *
 * Jinja2 ↔ nunjucks notes (keep in sync with mkdoc.py):
 *   - mkdoc.py's SectionCommentExtension wraps every `{% include %}` in
 *     `<!-- +section: X -->` / `<!-- -section: X -->`; `preprocessIncludes()`
 *     does the same text rewrite before nunjucks compiles a template.
 *   - macros.j2 is rendered once with the merged config and its exported macros
 *     are installed as globals (mkdoc.py: `env.globals.update(module.__dict__)`).
 *   - Jinja's `tojson` filter is added (`pyJson`, with Python's `, ` separators).
 *   - autoescape is off and undefined variables throw, as in mkdoc.py.
 */

const LOADER_ROOTS = ["templates/", "manifests/", ""];

/* ------------------------------------------------------------------------ */
/* Small helpers                                                             */
/* ------------------------------------------------------------------------ */

const isPlainObject = (v) =>
  v !== null && typeof v === "object" && !Array.isArray(v);

/** yq `*` style deep merge: maps merge recursively, everything else replaces. */
export function deepMerge(base, over) {
  if (!isPlainObject(base) || !isPlainObject(over)) return over;
  const out = { ...base };
  for (const [k, v] of Object.entries(over)) {
    out[k] = k in out ? deepMerge(out[k], v) : v;
  }
  return out;
}

/** Python `json.dumps` default formatting (`, ` and `: ` separators) with
 *  Jinja's `tojson` HTML-safe escapes. */
export function pyJson(value) {
  const esc = (s) =>
    JSON.stringify(s)
      .replace(/[\u007f-\uffff]/g, (ch) => "\\u" + ch.charCodeAt(0).toString(16).padStart(4, "0"))
      .replace(/</g, "\\u003c")
      .replace(/>/g, "\\u003e")
      .replace(/&/g, "\\u0026")
      .replace(/'/g, "\\u0027");
  const walk = (v) => {
    if (v === null || v === undefined) return "null";
    if (typeof v === "string") return esc(v);
    if (typeof v === "number" || typeof v === "boolean") return String(v);
    if (Array.isArray(v)) return "[" + v.map(walk).join(", ") + "]";
    return (
      "{" +
      Object.entries(v)
        .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)) // json.dumps(sort_keys=True)
        .map(([k, x]) => esc(String(k)) + ": " + walk(x))
        .join(", ") +
      "}"
    );
  };
  return walk(value);
}

/** mkdoc.py SectionCommentExtension.preprocess, verbatim semantics. */
export function preprocessIncludes(source) {
  return source.replace(/{%-?\s*include\s+(.+?)\s*-?%}/g, (m, expr) => {
    const path = "{{ " + expr + " }}";
    return `<!-- +section: ${path} -->\n${m}\n<!-- -section: ${path} -->\n`;
  });
}

/** Parse a recipes/*.conf: one config path per non-blank, non-comment line. */
export function parseConf(text) {
  return text
    .split("\n")
    .map((l) => l.trim())
    .filter((l) => l && !l.startsWith("#"));
}

/** Split a recipe name into its axes: {distro}{version}-{arch}-{provisioner}-{scheduler}. */
export function parseRecipeName(name) {
  // provisioner and scheduler may carry a digit (3.x: warewulf3, openpbs)
  const m = /^([a-z]+)([\d.]+)-([a-z0-9_]+)-([a-z][a-z0-9_]*)-([a-z][a-z0-9]*)$/.exec(name);
  if (!m) return null;
  return {
    name,
    distro: m[1],
    distro_version: m[2],
    arch: m[3],
    provisioner: m[4],
    scheduler: m[5],
  };
}

/* ------------------------------------------------------------------------ */
/* Script extraction — port of mkdoc.extract_recipe_script                   */
/* ------------------------------------------------------------------------ */

export function extractScript(markdown) {
  const lines = [];
  let inRun = false;
  let inCode = false;
  let currentSection = null;
  const errors = [];

  for (const line of markdown.split("\n")) {
    let m = /<!-- ([+-])section: (.+) -->/.exec(line);
    if (m) {
      lines.push(`# --- ${m[1]}section: ${m[2]} ---`);
      if (m[1] === "+") currentSection = m[2];
      continue;
    }
    if (line.includes("<!-- ohpc_begin -->")) {
      inRun = true;
      if (currentSection) lines.push(`echo '=== section: ${currentSection} ==='`);
      continue;
    }
    if (line.includes("<!-- ohpc_end -->")) {
      inRun = false;
      continue;
    }
    if (!inRun) continue;

    if (line.startsWith("```")) {
      inCode = !inCode;
      continue;
    }
    if (inCode) {
      lines.push(line);
    } else if (line.trim() === "") {
      lines.push("");
    } else {
      if (line.startsWith("<!-- ohpc_") && !line.endsWith("-->")) {
        errors.push(`ohpc directive missing closing -->: ${line}`);
      }
      if ((m = /<!-- ohpc_if_set (\S+) -->/.exec(line))) {
        lines.push(`if [[ "\${${m[1]}}" -eq 1 ]];then`);
        continue;
      }
      if ((m = /<!-- ohpc_if (.+) -->/.exec(line))) {
        lines.push(`if ${m[1]};then`);
        continue;
      }
      if (line.includes("<!-- ohpc_else -->")) {
        lines.push("else");
        continue;
      }
      if (line.includes("<!-- ohpc_fi -->")) {
        lines.push("fi");
        continue;
      }
      if ((m = /<!-- ohpc_command (.+) -->/.exec(line))) {
        lines.push(m[1].replace(/\s+$/, ""));
        continue;
      }
      if ((m = /<!-- ohpc_comment (.+) -->/.exec(line))) {
        const comment = m[1].replace(/'/g, "'\\''");
        lines.push(`echo '--- ${comment}'`);
      }
    }
  }

  const header = [
    "#!/bin/bash",
    "# OpenHPC Installation Recipe",
    "# Auto-generated from markdown documentation",
    "",
    "set -e",
    "",
  ];
  return { script: header.concat(lines).join("\n"), errors };
}

/** mkdoc.check_line_lengths: warn on long lines in bash/console blocks. */
export function checkLineLengths(markdown, max = 87) {
  const warnings = [];
  let section = "<unknown>";
  let sectionStart = 1;
  let inCode = false;
  markdown.split("\n").forEach((raw, i) => {
    const lineno = i + 1;
    const line = raw.replace(/\s+$/, "");
    const m = /^<!-- [+-]section: (.+?) -->/.exec(line);
    if (m) {
      section = m[1];
      sectionStart = lineno;
      return;
    }
    if (line.startsWith("```")) {
      const lang = line.replace(/^`+/, "");
      inCode = !inCode && (lang === "bash" || lang === "console");
      return;
    }
    if (inCode && line.length > max) {
      warnings.push({ section, line: lineno - sectionStart, length: line.length, text: line });
    }
  });
  return warnings;
}

/* ------------------------------------------------------------------------ */
/* Engine                                                                    */
/* ------------------------------------------------------------------------ */

/**
 * @param {object} opts
 * @param {object} opts.nunjucks   the nunjucks module/global
 * @param {object} opts.jsyaml     the js-yaml module/global
 * @param {(path: string) => Promise<string>} opts.readText  read a file
 *        relative to docs/install (throws / rejects on 404)
 * @param {object} [opts.manifest] explorer/manifest.json contents (optional;
 *        without it the loader probes each root in turn)
 */
/**
 * Make nunjucks string literals follow Jinja/Python escape rules: `\n`, `\t`,
 * `\r`, `\\`, `\'`, `\"` are escapes; any other `\x` is kept verbatim (nunjucks
 * drops the backslash, which breaks sed regexes such as "s/^\*\.info/.../").
 * Idempotent; applied once per nunjucks instance.
 */
export function patchNunjucksEscapes(nunjucks) {
  const lexer = nunjucks?.lexer;
  if (!lexer || lexer._jinjaEscapes) return;
  // Python's simple escapes; \xHH, \uHHHH and \UHHHHHHHH are decoded below.
  // Not covered: octal escapes and \N{NAME} (kept verbatim; none in the templates).
  const KNOWN = { n: "\n", t: "\t", r: "\r", a: "\x07", b: "\b", f: "\f", v: "\v", "0": "\0", "\\": "\\", "'": "'", '"': '"' };
  const HEX = { x: 2, u: 4, U: 8 };
  function parseString(delimiter) {
    this.forward();
    let str = "";
    while (!this.isFinished() && this.current() !== delimiter) {
      const cur = this.current();
      if (cur === "\\") {
        this.forward();
        const c = this.current();
        if (c in HEX) {
          const n = HEX[c];
          const hex = this.str.slice(this.index + 1, this.index + 1 + n);
          if (hex.length === n && /^[0-9a-fA-F]+$/.test(hex)) {
            str += String.fromCodePoint(parseInt(hex, 16));
            this.forwardN(n);
          } else {
            str += "\\" + c;
          }
        } else {
          str += c in KNOWN ? KNOWN[c] : "\\" + c;
        }
      } else {
        str += cur;
      }
      this.forward();
    }
    this.forward();
    return str;
  }
  // Only `lex()` is exported (not the Tokenizer class), so patch the prototype
  // of the first tokenizer it hands out; the parser calls `lexer.lex` through
  // this same exports object.
  const origLex = lexer.lex;
  lexer.lex = function (...args) {
    const tok = origLex.apply(this, args);
    const proto = Object.getPrototypeOf(tok);
    if (proto._parseString !== parseString) proto._parseString = parseString;
    return tok;
  };
  lexer._jinjaEscapes = true;
}

export function createEngine({ nunjucks, jsyaml, readText, manifest = null }) {
  patchNunjucksEscapes(nunjucks);
  /** path → text, for everything fetched so far (templates render synchronously
   *  from this cache; see `render()` for why). */
  const texts = new Map();
  const inflight = new Map();
  const known = manifest?.files ? new Set(manifest.files) : null;

  async function read(path) {
    if (texts.has(path)) return texts.get(path);
    if (!inflight.has(path)) {
      inflight.set(
        path,
        readText(path)
          .then((t) => {
            texts.set(path, t);
            return t;
          })
          .finally(() => inflight.delete(path))
      );
    }
    return inflight.get(path);
  }

  /** Fetch many files at once (ignoring failures); used to warm the cache. */
  async function prefetch(paths) {
    await Promise.all(paths.map((p) => read(p).catch(() => null)));
  }

  /** Candidate paths for a template name, in loader-root order. */
  const candidates = (name) => LOADER_ROOTS.map((r) => r + name);

  /** Fetch the first existing candidate for a template name. */
  async function fetchTemplate(name) {
    const list = candidates(name);
    const order = known ? list.filter((c) => known.has(c)).concat(list) : list;
    let lastErr = null;
    for (const c of order) {
      try {
        return { path: c, src: await read(c) };
      } catch (e) {
        lastErr = e;
      }
    }
    throw new Error(`template not found: ${name} (${lastErr?.message || "no roots"})`);
  }

  /** Makefile `build/%.yaml`: merge the conf's configs + override, inject vc_*. */
  async function loadConfig(recipe) {
    const entry = typeof recipe === "string" ? await recipeEntry(recipe) : recipe;
    let config = {};
    for (const file of entry.configs) {
      config = deepMerge(config, jsyaml.load(await read(file)) || {});
    }
    if (entry.override) {
      config = deepMerge(config, jsyaml.load(await read(entry.override)) || {});
    }
    config.vc_revision = manifest?.vc_revision ?? "non-git-checkout";
    config.vc_date = manifest?.vc_date ?? new Date().toISOString().slice(0, 10);
    return config;
  }

  async function recipeEntry(name) {
    const fromManifest = manifest?.recipes?.find((r) => r.name === name);
    if (fromManifest) return fromManifest;
    const conf = `recipes/${name}.conf`;
    const configs = parseConf(await read(conf));
    let override = null;
    try {
      await read(`recipes/${name}.yaml`);
      override = `recipes/${name}.yaml`;
    } catch {
      /* no override */
    }
    return { name, conf, configs, override };
  }

  /** Thrown by the synchronous loader when a template is not cached yet. */
  class NeedsFetch extends Error {
    constructor(name) {
      super(`template not loaded: ${name}`);
      this.templateName = name;
    }
  }

  function makeEnvironment(config) {
    // The loader is synchronous on purpose: nunjucks' async mode drops the
    // output of an `{% include %}` nested in an `{% if %}/{% elif %}` chain
    // (observed with 3.2.4), and the chapter aggregators use exactly that.
    // Templates are served from the text cache; a miss raises NeedsFetch and
    // render() fetches it and retries.
    const Loader = nunjucks.Loader.extend({
      getSource(name) {
        for (const c of candidates(name)) {
          if (texts.has(c)) {
            return { src: preprocessIncludes(texts.get(c)), path: c, noCache: false };
          }
        }
        throw new NeedsFetch(name);
      },
    });
    const env = new nunjucks.Environment(new Loader(), {
      autoescape: false,
      throwOnUndefined: true,
      trimBlocks: false,
      lstripBlocks: false,
    });
    env.addFilter("tojson", pyJson);
    for (const [k, v] of Object.entries(config)) env.addGlobal(k, v);
    return env;
  }

  /** Call a nunjucks callback-style API that completes synchronously. */
  function sync(fn) {
    let result;
    let error;
    let done = false;
    fn((e, r) => {
      done = true;
      error = e;
      result = r;
    });
    if (!done) throw new Error("nunjucks did not complete synchronously");
    if (error) throw error;
    return result;
  }

  /** Unwrap nunjucks' error wrapping to find a NeedsFetch, if any. */
  function needsFetch(err) {
    for (let e = err; e; e = e.cause || e.original) {
      if (e instanceof NeedsFetch) return e.templateName;
      const m = /template not loaded: (\S+)/.exec(e.message || "");
      if (m) return m[1];
    }
    return null;
  }

  function renderSync(config) {
    const env = makeEnvironment(config);
    const macros = env.getTemplate("macros.j2");
    const exported = sync((cb) => macros.getExported(config, cb));
    for (const [k, v] of Object.entries(exported)) env.addGlobal(k, v);
    return env.render("chapters/main.md.j2", config);
  }

  /**
   * Render one recipe.
   * @returns {{config, markdown, script, scriptErrors, warnings}}
   */
  async function render(recipe, { onProgress } = {}) {
    const progress = (msg) => onProgress && onProgress(msg);
    progress("merging configuration");
    const config = await loadConfig(recipe);

    progress("rendering templates");
    let markdown;
    for (let attempt = 0; ; attempt++) {
      try {
        markdown = renderSync(config);
        break;
      } catch (e) {
        const name = needsFetch(e);
        if (!name || attempt > 500) throw e;
        progress(`loading ${name}`);
        await fetchTemplate(name);
      }
    }

    progress("extracting recipe script");
    const { script, errors: scriptErrors } = extractScript(markdown);
    const warnings = checkLineLengths(markdown);
    return { config, markdown, script, scriptErrors, warnings };
  }

  /** Warm the cache with every loader file listed in the manifest. */
  async function prefetchAll() {
    if (!manifest?.files) return;
    await prefetch(manifest.files.filter((f) => /\.(j2|md)$/.test(f)));
  }

  return { read, prefetch, prefetchAll, fetchTemplate, loadConfig, recipeEntry, render, manifest };
}
