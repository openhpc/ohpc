#!/usr/bin/env node
/**
 * Stack-depth test: render every recipe and record the JS stack depth each
 * time nunjucks loads a template. nunjucks' include runs the rest of the
 * template inside its callback, so before the engine rendered includes as
 * calls the stack grew with every include (about 2700 frames for a whole
 * guide) and Firefox failed with "too much recursion". The depth must now
 * follow the include nesting, not the number of includes.
 *
 *   cd docs/install && make explorer
 *   node explorer/test/stack.mjs            # all recipes
 *   node explorer/test/stack.mjs rocky10-x86_64-warewulf-slurm
 */

import { createRequire } from "node:module";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { createEngine } from "../engine.js";

const MAX_FRAMES = 200;

const require = createRequire(import.meta.url);
const nunjucks = require("../vendor/nunjucks.js");
const jsyaml = require("../vendor/js-yaml.min.js");

const HERE = path.dirname(fileURLToPath(import.meta.url));
const BASE = path.resolve(HERE, "..", "..");

Error.stackTraceLimit = Infinity;
let deepest = { frames: 0, name: "" };
const getTemplate = nunjucks.Environment.prototype.getTemplate;
nunjucks.Environment.prototype.getTemplate = function (name, ...rest) {
  const frames = new Error().stack.split("\n").length;
  if (frames > deepest.frames) deepest = { frames, name };
  return getTemplate.call(this, name, ...rest);
};

const manifest = JSON.parse(await readFile(path.join(BASE, "explorer/manifest.json"), "utf8"));
const engine = createEngine({
  nunjucks,
  jsyaml,
  manifest,
  readText: (rel) => readFile(path.join(BASE, rel), "utf8"),
});
await engine.prefetchAll();

const wanted = process.argv.slice(2);
const recipes = manifest.recipes.filter((r) => !wanted.length || wanted.includes(r.name));
let failures = 0;
for (const r of recipes) {
  deepest = { frames: 0, name: "" };
  try {
    await engine.render(r);
  } catch (e) {
    failures++;
    console.log(`FAIL ${r.name}: render error: ${e.message.split("\n").pop()}`);
    continue;
  }
  const ok = deepest.frames <= MAX_FRAMES;
  if (!ok) failures++;
  console.log(`${ok ? "ok  " : "FAIL"} ${r.name}: ${deepest.frames} frames at ${deepest.name} (max ${MAX_FRAMES})`);
}
console.log(failures ? `${failures} failure(s)` : `all ${recipes.length} recipes within ${MAX_FRAMES} frames`);
process.exit(failures ? 1 : 0);
