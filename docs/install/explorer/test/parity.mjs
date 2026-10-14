#!/usr/bin/env node
/**
 * Parity test: render every recipe with the JS engine (node, fs loader) and
 * compare byte-for-byte with the Python build output in ../build/.
 *
 *   cd docs/install && make && make explorer
 *   node explorer/test/parity.mjs            # all recipes
 *   node explorer/test/parity.mjs rocky10-x86_64-warewulf-slurm
 *
 * Exit status is non-zero on any mismatch; the first differing line of each
 * mismatch is printed with context.
 */
import { createRequire } from "node:module";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { createEngine } from "../engine.js";

const require = createRequire(import.meta.url);
const nunjucks = require("../vendor/nunjucks.js");
const jsyaml = require("../vendor/js-yaml.min.js");

const HERE = path.dirname(fileURLToPath(import.meta.url));
const BASE = path.resolve(HERE, "..", "..");

const manifest = JSON.parse(await readFile(path.join(BASE, "explorer/manifest.json"), "utf8"));
const engine = createEngine({
  nunjucks,
  jsyaml,
  manifest,
  readText: (rel) => readFile(path.join(BASE, rel), "utf8"),
});

function firstDiff(a, b) {
  const al = a.split("\n");
  const bl = b.split("\n");
  for (let i = 0; i < Math.max(al.length, bl.length); i++) {
    if (al[i] !== bl[i]) {
      return { line: i + 1, expected: al.slice(Math.max(0, i - 2), i + 3), actual: bl.slice(Math.max(0, i - 2), i + 3) };
    }
  }
  return null;
}

const wanted = process.argv.slice(2);
const recipes = manifest.recipes.filter((r) => !wanted.length || wanted.includes(r.name));
let failures = 0;
for (const r of recipes) {
  // Stamp with the revision the Makefile merged into build/<name>.yaml, so a
  // build/ older than HEAD still compares (the title page carries vc_*).
  try {
    const merged = jsyaml.load(await readFile(path.join(BASE, `build/${r.name}.yaml`), "utf8")) || {};
    if (merged.vc_revision) manifest.vc_revision = merged.vc_revision;
    if (merged.vc_date) manifest.vc_date = String(merged.vc_date);
  } catch {
    /* no merged config: fall back to manifest.json's stamp */
  }
  let result;
  try {
    result = await engine.render(r);
  } catch (e) {
    failures++;
    console.log(`FAIL ${r.name}: render error: ${e.message}`);
    continue;
  }
  const checks = [
    ["md", result.markdown, await readFile(path.join(BASE, `build/${r.name}.md`), "utf8")],
    ["sh", result.script, await readFile(path.join(BASE, `build/${r.name}.sh`), "utf8")],
  ];
  let ok = true;
  for (const [kind, actual, expected] of checks) {
    if (actual === expected) continue;
    ok = false;
    failures++;
    const d = firstDiff(expected, actual);
    console.log(`FAIL ${r.name}.${kind}: first difference at line ${d.line}`);
    console.log("  expected: " + JSON.stringify(d.expected));
    console.log("  actual:   " + JSON.stringify(d.actual));
  }
  if (ok) console.log(`ok   ${r.name} (${result.markdown.length} bytes md, ${result.script.split("\n").length} script lines)`);
}
console.log(failures ? `${failures} failure(s)` : `all ${recipes.length} recipes match`);
process.exit(failures ? 1 : 0);
