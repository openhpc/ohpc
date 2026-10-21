/**
 * OpenHPC recipe explorer — the page.
 *
 * Choose a base OS, architecture, provisioner and scheduler (the four axes a
 * recipes/*.conf is named by), pick the optional features you plan to enable,
 * and read the install guide for exactly that path — rendered in the browser
 * from the .md.j2 templates by engine.js, with the recipe.sh and an
 * input.local to download.
 *
 * State lives in the URL hash so a path is linkable:
 *   #rocky10-x86_64-warewulf-slurm?enable_ib=1&skipped=1
 */
import { createEngine, parseRecipeName } from "./engine.js";
import { renderGuide, applyFlags } from "./render-html.js";

const BASE = "../"; // docs/install, relative to explorer/
// A multi-version site (explorer/mksite.py) places each ref's docs/install at
// <site>/<label>/ and a versions.json at <site>/. From explorer/ that is two
// levels up; a bare docs/install checkout has none there, and the switcher
// stays hidden.
const VERSIONS_URL = "../../versions.json";
const SOURCE_REPO = "https://github.com/openhpc/ohpc/blob";
// Upstream branch named in provenance titles: the release branch this tree was
// stamped from when it looks like one (4.x, 3.x), else 4.x.
const sourceBranch = () => (/^\d+\.x$/.test(manifest?.vc_branch || "") ? manifest.vc_branch : "4.x");
const FLAG_RE = /^(enable_|has_|update_)\w+$/; // runtime flags accepted from the URL

const AXES = [
  {
    key: "distro",
    title: "Base operating system",
    prompt: "You have a head node and a rack of bare metal. Which base OS goes on the head node?",
    configDir: "config/distro",
    label: (cfg, v) => cfg.distro_full_name || v,
  },
  {
    key: "arch",
    title: "Architecture",
    prompt: "What silicon fills the racks?",
    configDir: "config/arch",
    label: (cfg, v) => cfg.arch || v,
  },
  {
    key: "provisioner",
    title: "Provisioner",
    prompt: "How will the compute nodes receive their operating system?",
    configDir: "config/provisioner",
    label: (cfg, v) => {
      // distinguish variants that share a display name: xcat_stateless and
      // xcat_stateful are both "xCAT"; warewulf3 and warewulf both "Warewulf"
      const name = cfg.provisioner_name || v;
      const extra = v.replace(name.toLowerCase().replace(/\s+/g, ""), "").replace(/^_/, "");
      if (!extra || extra === v) return name;
      return /^\d/.test(extra) ? `${name} ${extra}` : `${name} (${extra})`;
    },
  },
  {
    key: "scheduler",
    title: "Scheduler",
    prompt: "And who decides which job runs where?",
    configDir: "config/scheduler",
    label: (cfg, v) => cfg.scheduler_name || v,
  },
];

const $ = (sel, root = document) => root.querySelector(sel);
const h = (tag, attrs = {}, ...children) => {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === "class") e.className = v;
    else if (k === "dataset") Object.assign(e.dataset, v);
    else if (k.startsWith("on")) e.addEventListener(k.slice(2), v);
    else if (v !== null && v !== undefined && v !== false) e.setAttribute(k, v === true ? "" : v);
  }
  for (const c of children.flat(Infinity)) {
    if (c === null || c === undefined || c === false) continue;
    e.append(c.nodeType ? c : document.createTextNode(String(c)));
  }
  return e;
};

/* ------------------------------------------------------------------------ */
/* State ↔ URL                                                               */
/* ------------------------------------------------------------------------ */

const state = {
  axes: { distro: null, arch: null, provisioner: null, scheduler: null },
  flags: {}, // name → 0/1 (only when the user changed it from the default)
  chapter: null, // chapter heading id to restore when a deep link is opened
  skipped: false, // show optional steps that are switched off
  scriptOnly: false, // show script-only lines
  sources: true, // show template provenance badges
};

function readHash() {
  const raw = location.hash.replace(/^#\/?/, "");
  state.flags = {};
  state.chapter = null;
  state.skipped = false;
  state.scriptOnly = false;
  state.sources = true;
  const clearAxes = () => { for (const k of Object.keys(state.axes)) state.axes[k] = null; };
  if (!raw) {
    clearAxes();
    return;
  }
  const [name, query = ""] = raw.split("?");
  const parsed = parseRecipeName(name);
  if (parsed && recipes.some((r) => r.name === name)) {
    state.axes = {
      distro: parsed.distro + parsed.distro_version,
      arch: parsed.arch,
      provisioner: parsed.provisioner,
      scheduler: parsed.scheduler,
    };
  } else {
    clearAxes(); // unknown or renamed recipe: back to the chooser
  }
  const params = new URLSearchParams(query);
  for (const [k, v] of params) {
    if (k === "skipped") state.skipped = v === "1";
    else if (k === "script") state.scriptOnly = v === "1";
    else if (k === "sources") state.sources = v !== "0";
    else if (k === "chapter") state.chapter = v;
    else if (FLAG_RE.test(k)) state.flags[k] = Number(v) ? 1 : 0;
  }
}

function hashFor(chapter = state.chapter) {
  const name = currentRecipeName();
  if (!name) return "";
  const params = new URLSearchParams();
  for (const [k, v] of Object.entries(state.flags)) params.set(k, String(v));
  if (state.skipped) params.set("skipped", "1");
  if (state.scriptOnly) params.set("script", "1");
  if (!state.sources) params.set("sources", "0");
  if (chapter) params.set("chapter", chapter);
  const q = params.toString();
  return name + (q ? "?" + q : "");
}

function writeHash() {
  const next = hashFor();
  if (location.hash.replace(/^#\/?/, "") !== next) {
    history.replaceState(null, "", next ? "#" + next : location.pathname + location.search);
  }
}

/* ------------------------------------------------------------------------ */
/* Recipes and axis metadata                                                 */
/* ------------------------------------------------------------------------ */

let manifest = null;
let engine = null;
let recipes = []; // parsed recipe names, with axis values
const axisInfo = {}; // key → { value → { label, description } }

function recipeAxes(r) {
  return { distro: r.distro + r.distro_version, arch: r.arch, provisioner: r.provisioner, scheduler: r.scheduler };
}

function currentRecipeName() {
  const a = state.axes;
  if (!a.distro || !a.arch || !a.provisioner || !a.scheduler) return null;
  const hit = recipes.find((r) => {
    const ra = recipeAxes(r);
    return AXES.every((ax) => ra[ax.key] === a[ax.key]);
  });
  return hit ? hit.name : null;
}

/** Recipes compatible with the current selection on every axis except `except`. */
function compatible(except) {
  return recipes.filter((r) => {
    const ra = recipeAxes(r);
    return AXES.every((ax) => ax.key === except || !state.axes[ax.key] || ra[ax.key] === state.axes[ax.key]);
  });
}

/** Leading `#` comment block of a YAML file, as a description. */
function yamlDescription(text) {
  const lines = [];
  for (const line of text.split("\n")) {
    if (!line.startsWith("#")) {
      if (lines.length) break;
      continue;
    }
    lines.push(line.replace(/^#\s?/, ""));
  }
  // drop the title line and keep the first paragraph after it
  const body = lines.slice(1).join("\n").trim().split(/\n\s*\n/)[0] || "";
  return { title: lines[0] || "", body: body.replace(/\s+/g, " ") };
}

async function loadAxisInfo() {
  for (const ax of AXES) {
    axisInfo[ax.key] = {};
    const values = [...new Set(recipes.map((r) => recipeAxes(r)[ax.key]))];
    await Promise.all(
      values.map(async (v) => {
        // the distro config file is named with the version on 3.x (rocky9.yaml)
        // and without on 4.x (rocky.yaml); try both
        let files = ax.key === "distro" ? [v, v.replace(/[\d.]+$/, "")] : [v];
        const listed = files.filter((f) => manifest.files.includes(`${ax.configDir}/${f}.yaml`));
        if (listed.length) files = listed; // avoid probing (and 404s) when the manifest knows
        let cfg = {};
        let desc = { title: "", body: "" };
        for (const file of files) {
          try {
            const text = await engine.read(`${ax.configDir}/${file}.yaml`);
            cfg = jsyaml.load(text) || {};
            desc = yamlDescription(text);
            break;
          } catch {
            /* try the next spelling; the label falls back to the raw value */
          }
        }
        axisInfo[ax.key][v] = { label: ax.label(cfg, v), description: desc.body || desc.title, cfg };
      })
    );
  }
}

/* ------------------------------------------------------------------------ */
/* Chooser                                                                   */
/* ------------------------------------------------------------------------ */

function renderChooser() {
  const root = $("#chooser");
  root.textContent = "";
  const complete = Boolean(currentRecipeName());
  root.classList.toggle("complete", complete);

  for (const ax of AXES) {
    const options = Object.keys(axisInfo[ax.key]).sort();
    const ok = new Set(compatible(ax.key).map((r) => recipeAxes(r)[ax.key]));
    const section = h("section", { class: "axis", dataset: { axis: ax.key } },
      h("h2", {}, h("span", { class: "axis-step" }, String(AXES.indexOf(ax) + 1)), ax.title),
      h("p", { class: "prompt" }, ax.prompt),
      h("div", { class: "chips", role: "radiogroup", "aria-label": ax.title },
        options.map((v) => {
          const info = axisInfo[ax.key][v];
          const selected = state.axes[ax.key] === v;
          const available = ok.has(v);
          return h("button", {
            type: "button",
            class: "chip" + (selected ? " selected" : "") + (available ? "" : " unavailable"),
            role: "radio",
            "aria-checked": selected ? "true" : "false",
            dataset: { value: v },
            title: available ? info.description : "No recipe combines this with your other choices",
            onclick: () => {
              if (!available) {
                // clear the axes that conflict, keep this one
                for (const other of AXES) if (other.key !== ax.key) state.axes[other.key] = null;
              }
              state.chapter = null;
              state.axes[ax.key] = selected ? null : v;
              update();
            },
          }, h("span", { class: "chip-label" }, info.label), h("span", { class: "chip-sub" }, v));
        })
      )
    );
    root.appendChild(section);
  }
  const n = compatible(null).length;
  root.appendChild(
    h("p", { class: "paths" },
      complete
        ? h("span", {}, "Your path: ", h("code", {}, currentRecipeName()))
        : `${n} of ${recipes.length} paths still open`,
      " ",
      h("button", { type: "button", class: "link", onclick: () => { for (const ax of AXES) state.axes[ax.key] = null; state.flags = {}; state.chapter = null; update(); } }, "start over")
    )
  );
}

function renderPathBar() {
  const bar = $("#pathbar");
  bar.textContent = "";
  const name = currentRecipeName();
  if (!name) {
    bar.hidden = true;
    return;
  }
  bar.hidden = false;
  bar.append(
    h("span", { class: "crumbs" },
      AXES.map((ax, i) => [i ? h("span", { class: "sep" }, "›") : null, h("span", { class: "crumb" }, axisInfo[ax.key][state.axes[ax.key]].label)])
    ),
    h("button", { type: "button", class: "link", onclick: () => { document.body.classList.remove("reading"); window.scrollTo(0, 0); } }, "change path")
  );
}

/* ------------------------------------------------------------------------ */
/* Guide                                                                     */
/* ------------------------------------------------------------------------ */

let rendered = null; // { name, result, article, toc, flags }
let renderSeq = 0; // guards against two overlapping renders

function resolveImage(src) {
  const clean = src.replace(/^\.\//, "");
  const files = new Set(manifest.files);
  for (const ext of ["", ".svg", ".png"]) {
    if (files.has(`templates/${clean}${ext}`)) return `${BASE}templates/${clean}${ext}`;
  }
  return `${BASE}templates/${clean}`;
}

function sourceLink(name) {
  const files = new Set(manifest.files);
  const rel = ["templates/", "manifests/", ""].map((r) => r + name).find((p) => files.has(p)) || `templates/${name}`;
  return { href: `${BASE}${rel}`, title: `${rel} — template source (upstream: ${SOURCE_REPO}/${sourceBranch()}/docs/install/${rel})` };
}

async function renderCurrent() {
  const name = currentRecipeName();
  const main = $("#guide");
  const status = $("#status");
  if (!name) {
    document.body.classList.remove("reading");
    main.textContent = "";
    delete main.dataset.recipe;
    rendered = null;
    return;
  }
  document.body.classList.add("reading");
  if (rendered && rendered.name === name) {
    applyView();
    await restoreChapter();
    return;
  }
  const seq = ++renderSeq;
  status.hidden = false;
  status.textContent = "Rendering your guide…";
  main.textContent = "";
  delete main.dataset.recipe;
  $("#toc").textContent = "";
  try {
    const t0 = performance.now();
    const result = await engine.render(name, { onProgress: (m) => (status.textContent = `Rendering your guide… ${m}`) });
    if (seq !== renderSeq) return; // a newer path was chosen meanwhile
    const { article, toc, flags } = renderGuide(result.markdown, { marked, hljs, resolveImage, sourceLink });
    rendered = { name, result, article, toc, flags };
    addChapterNav(article, toc);
    main.appendChild(article);
    main.dataset.recipe = name;
    renderToc(toc);
    renderOptions(flags);
    renderDownloads();
    status.hidden = true;
    const ms = Math.round(performance.now() - t0);
    $("#stamp").textContent = `rendered in ${ms} ms from ${manifest.vc_revision.slice(0, 9)} (${manifest.vc_date})`;
    if (result.warnings.length) console.warn("mkdoc warnings", result.warnings);
    if (result.scriptErrors.length) {
      status.hidden = false;
      status.textContent = "";
      status.append(h("strong", {}, "The recipe script is incomplete (mkdoc.py would fail this build): "),
        h("pre", { class: "error" }, result.scriptErrors.join("\n")));
    }
    applyView();
    await restoreChapter();
  } catch (err) {
    console.error(err);
    status.hidden = false;
    status.textContent = "";
    status.append(
      h("strong", {}, "Could not render this path. "),
      h("pre", { class: "error" }, String(err.message || err))
    );
  }
}

function addChapterNav(article, toc) {
  const chapters = toc.filter((t) => t.level === 1);
  const headings = new Map(
    Array.from(article.querySelectorAll("h1[id]"), (heading) => [heading.id, heading])
  );
  const wrappers = Array.from(article.querySelectorAll(':scope > .src[data-src^="chapters/"]'));
  // Use the processed TOC as the source of truth so every chapter heading,
  // including the headings nested in the appendices aggregator, gets a link.
  for (const chapter of chapters) {
    const first = headings.get(chapter.id);
    if (!first || first.querySelector(".chapter-link")) continue;
    const copy = h(
      "button",
      {
        type: "button",
        class: "chapter-link",
        "aria-label": `Copy a link to ${first.textContent.trim()}`,
        title: "Copy a link to this chapter",
        onclick: () => copyChapterLink(first.id, copy),
      },
      "Copy link"
    );
    first.append(copy);
  }
  for (const wrap of wrappers) {
    const first = wrap.querySelector("h1");
    if (!first) continue;
    const idx = chapters.findIndex((c) => c.id === first.id);
    const next = idx >= 0 ? chapters[idx + 1] : null;
    if (!next) continue;
    wrap.appendChild(
      h("nav", { class: "chapter-next" },
        h("a", { href: "#" + next.id, onclick: (e) => { e.preventDefault(); jumpTo(next.id); } },
          "Continue to ", h("span", { class: "secno" }, next.number), next.text, " →")
      )
    );
  }
}

function jumpTo(id, behavior = "smooth") {
  const target = document.getElementById(id);
  if (!target) return;
  target.scrollIntoView({ behavior, block: "start" });
}

function waitForImages(article) {
  return Promise.all(Array.from(article.querySelectorAll("img"), (img) => {
    const decoded = () => img.decode().catch(() => {});
    if (img.complete) return decoded();
    return new Promise((resolve) => {
      const done = () => {
        img.removeEventListener("load", done);
        img.removeEventListener("error", done);
        resolve(decoded());
      };
      img.addEventListener("load", done, { once: true });
      img.addEventListener("error", done, { once: true });
    });
  }));
}

async function restoreChapter() {
  const id = state.chapter;
  const view = rendered;
  if (!id || !view) return;
  // Consume the one-shot URL instruction before waiting. Otherwise an option
  // update during image loading writes the old chapter back into the hash and
  // starts another restore, moving the reader unexpectedly.
  state.chapter = null;
  writeHash();
  await waitForImages(view.article);
  if (rendered !== view) return;
  jumpTo(id, "instant");
}

async function copyChapterLink(id, button) {
  const url = new URL(location.href);
  url.hash = hashFor(id);
  if (!navigator.clipboard) {
    button.textContent = "Copy unavailable";
    setTimeout(() => { button.textContent = "Copy link"; }, 1500);
    return;
  }
  try {
    await navigator.clipboard.writeText(url.href);
    button.textContent = "Copied";
  } catch {
    button.textContent = "Copy failed";
  }
  setTimeout(() => { button.textContent = "Copy link"; }, 1500);
}

function renderToc(toc) {
  const nav = $("#toc");
  nav.textContent = "";
  const list = h("ol", { class: "toc" });
  let current = null;
  for (const t of toc) {
    if (t.level > 2) continue;
    const link = h("a", { href: "#" + t.id, onclick: (e) => { e.preventDefault(); jumpTo(t.id); } },
      t.number ? h("span", { class: "secno" }, t.number) : null, t.text);
    if (t.level === 1) {
      current = h("li", { class: "toc-chapter" }, link, h("ol", {}));
      list.appendChild(current);
    } else if (current) {
      current.lastChild.appendChild(h("li", {}, link));
    }
  }
  nav.appendChild(list);
}

/* ------------------------------------------------------------------------ */
/* Options (runtime flags) and view switches                                 */
/* ------------------------------------------------------------------------ */

function flagValue(name) {
  if (name in state.flags) return state.flags[name];
  const f = manifest.flags.find((x) => x.name === name);
  return f ? Number(f.default) : undefined;
}

function allFlagValues() {
  const out = {};
  for (const f of manifest.flags) out[f.name] = flagValue(f.name);
  for (const k of Object.keys(state.flags)) out[k] = state.flags[k];
  return out;
}

function renderOptions(usedFlags) {
  const root = $("#options");
  root.textContent = "";
  const names = new Set([...manifest.flags.map((f) => f.name), ...usedFlags.keys()]);
  const used = [...names].filter((n) => usedFlags.has(n));
  const unused = [...names].filter((n) => !usedFlags.has(n));

  const row = (name) => {
    const f = manifest.flags.find((x) => x.name === name);
    const v = flagValue(name);
    const count = usedFlags.get(name) || 0;
    const id = "flag-" + name;
    return h("label", { class: "flag" + (v === undefined ? " unknown" : ""), for: id, title: f?.description || "" },
      h("input", {
        type: "checkbox", id, checked: v === 1, dataset: { flag: name },
        onchange: (e) => {
          const def = f ? Number(f.default) : undefined;
          const val = e.target.checked ? 1 : 0;
          if (val === def) delete state.flags[name];
          else state.flags[name] = val;
          update();
        },
      }),
      h("span", { class: "flag-name" }, name.replace(/^enable_/, "")),
      count ? h("span", { class: "flag-count", title: `${count} optional step${count > 1 ? "s" : ""} in this guide` }, String(count)) : null
    );
  };

  root.append(
    h("h3", {}, "Optional features"),
    h("p", { class: "hint" }, "The ", h("code", {}, "enable_*"), " switches from ", h("code", {}, "input.local"), ". Steps you switch off are folded away; the recipe.sh keeps them behind ", h("code", {}, "if"), "."),
    h("div", { class: "flags" }, used.map(row)),
    unused.length
      ? h("details", { class: "more-flags" }, h("summary", {}, `${unused.length} more (not referenced in this guide)`), h("div", { class: "flags" }, unused.map(row)))
      : null
  );
}

function applyView() {
  if (!rendered) return;
  applyFlags(rendered.article, allFlagValues());
  document.body.classList.toggle("show-skipped", state.skipped);
  document.body.classList.toggle("show-script-only", state.scriptOnly);
  document.body.classList.toggle("show-sources", state.sources);
  $("#sw-skipped").checked = state.skipped;
  $("#sw-script").checked = state.scriptOnly;
  $("#sw-sources").checked = state.sources;
  for (const input of $("#options").querySelectorAll("input[data-flag]")) {
    input.checked = flagValue(input.dataset.flag) === 1;
  }
  renderDownloads();
}

/* ------------------------------------------------------------------------ */
/* Downloads                                                                 */
/* ------------------------------------------------------------------------ */

let inputLocalTemplate = null;

function buildInputLocal() {
  let text = inputLocalTemplate || "";
  for (const [name, v] of Object.entries(allFlagValues())) {
    if (v === undefined) continue;
    const re = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    text = text.replace(new RegExp(`^(${re}="?)\\$\\{${re}:-[^}]*\\}`, "m"), `$1\${${name}:-${v}}`);
  }
  const rec = rendered?.result.config;
  if (rec) {
    text = `# input.local generated by the OpenHPC recipe explorer for ${rendered.name}\n# (${rec.distro_full_name}, ${rec.arch}, ${rec.provisioner_name}, ${rec.scheduler_name}; revision ${rec.vc_revision.slice(0, 9)})\n` + text;
  }
  return text;
}

const blobUrls = new Map();
function blobLink(key, content, type) {
  if (blobUrls.has(key)) URL.revokeObjectURL(blobUrls.get(key));
  const url = URL.createObjectURL(new Blob([content], { type }));
  blobUrls.set(key, url);
  return url;
}

function renderDownloads() {
  const root = $("#downloads");
  root.textContent = "";
  if (!rendered) return;
  const { result, name } = rendered;
  const link = (label, file, content, type, title) =>
    h("a", { class: "dl", download: file, href: blobLink(file, content, type), title }, label, h("small", {}, file));
  root.append(
    link("Install guide", `Install_guide-${name}.md`, result.markdown, "text/markdown", "The guide as Markdown — the same file `make` writes to build/"),
    link("Recipe script", `recipe-${name}.sh`, result.script, "text/x-shellscript", "Every command in the guide, extracted into one script (the recipe.sh shipped in docs-ohpc)"),
    link("input.local", `input.local-${name}`, buildInputLocal(), "text/plain", "The input.local template with your optional-feature choices applied"),
  );
}

/* ------------------------------------------------------------------------ */
/* Versions (multi-ref site)                                                 */
/* ------------------------------------------------------------------------ */

async function loadVersions() {
  const res = await fetch(VERSIONS_URL, { cache: "no-cache" });
  if (!res.ok) return;
  const data = await res.json();
  const versions = Array.isArray(data.versions) ? data.versions : [];
  if (versions.length < 2) return;
  // which entry is this page? match the revision the manifest was stamped with
  const here = versions.find((v) => v.revision === manifest.vc_revision) || versions.find((v) => v.ref === manifest.vc_branch);
  const select = h("select", {
    id: "version", class: "version", "aria-label": "Documentation version",
    title: "Switch to another published version of the templates",
    onchange: (e) => {
      const v = versions.find((x) => x.path === e.target.value);
      if (!v) return;
      // same hash (recipe + flags) in the other version; it falls back to the
      // chooser there if that recipe does not exist
      location.href = new URL(`../../${v.path}explorer/${location.hash}`, location.href).href;
    },
  }, versions.map((v) => h("option", { value: v.path, selected: v === here }, v.label || v.ref)));
  $("#rev").before(select);
}

/* ------------------------------------------------------------------------ */
/* Wiring                                                                    */
/* ------------------------------------------------------------------------ */

function update() {
  writeHash();
  renderChooser();
  renderPathBar();
  renderCurrent();
}

async function init() {
  const res = await fetch(`${BASE}explorer/manifest.json`, { cache: "no-cache" });
  if (!res.ok) throw new Error(`manifest.json: HTTP ${res.status} — run \`make explorer\` in docs/install`);
  manifest = await res.json();
  engine = createEngine({
    nunjucks,
    jsyaml,
    manifest,
    readText: async (rel) => {
      const r = await fetch(BASE + rel, { cache: "no-cache" });
      if (!r.ok) throw new Error(`HTTP ${r.status} for ${rel}`);
      return r.text();
    },
  });
  recipes = manifest.recipes.map((r) => parseRecipeName(r.name)).filter(Boolean);
  hljs.registerAliases("console", { languageName: "shell" });
  const warm = engine.prefetchAll(); // every template, in parallel
  engine.read("input.local.template").then((t) => (inputLocalTemplate = t)).catch(() => {});
  await loadAxisInfo();
  await warm;

  $("#rev").textContent = `${manifest.vc_branch || ""} ${manifest.vc_revision.slice(0, 9)} · ${manifest.vc_date} · ${recipes.length} recipes`;
  loadVersions().catch((e) => console.warn("versions.json:", e.message)); // optional; absent on a bare checkout
  $("#sw-skipped").addEventListener("change", (e) => { state.skipped = e.target.checked; update(); });
  $("#sw-script").addEventListener("change", (e) => { state.scriptOnly = e.target.checked; update(); });
  $("#sw-sources").addEventListener("change", (e) => { state.sources = e.target.checked; update(); });
  window.addEventListener("hashchange", () => { readHash(); update(); });
  document.addEventListener("click", (e) => {
    // clicking an optional block's label flips its flag
    const label = e.target.closest(".guide .cond[data-flag] > .cond-label");
    if (label) {
      const block = label.parentElement;
      const name = block.dataset.flag;
      const f = manifest.flags.find((x) => x.name === name);
      const def = f ? Number(f.default) : undefined;
      const val = flagValue(name) === 1 ? 0 : 1;
      if (val === def) delete state.flags[name];
      else state.flags[name] = val;
      update();
      return;
    }
    const btn = e.target.closest("button.copy");
    if (!btn) return;
    const code = btn.parentElement.querySelector("code");
    navigator.clipboard?.writeText(code.textContent).then(() => {
      btn.textContent = "copied";
      setTimeout(() => (btn.textContent = "copy"), 1200);
    });
  });
  // provenance: highlight only the innermost template section under the pointer
  let hovered = null;
  document.addEventListener("mouseover", (e) => {
    const src = e.target.closest(".guide .src");
    if (src === hovered) return;
    hovered?.classList.remove("hovered");
    hovered = src;
    hovered?.classList.add("hovered");
  });
  readHash();
  update();
  document.body.classList.add("ready");
}

init().catch((err) => {
  console.error(err);
  const status = $("#status");
  status.hidden = false;
  status.append(h("strong", {}, "The explorer could not start. "), h("pre", { class: "error" }, String(err.message || err)));
});
