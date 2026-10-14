#!/usr/bin/env node
/**
 * Marker-grammar contract: engine.js's extractScript() must turn
 * ../../tests/markers.md into exactly ../../tests/markers.sh — the same
 * fixture tests/test_markers.py holds mkdoc.py to. Change the grammar in both
 * implementations and the fixture together.
 *
 *   node explorer/test/markers.mjs
 */
import { readFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { extractScript } from "../engine.js";

const BASE = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");
const markdown = await readFile(path.join(BASE, "tests/markers.md"), "utf8");
const expected = await readFile(path.join(BASE, "tests/markers.sh"), "utf8");

let failures = 0;
const { script, errors } = extractScript(markdown);
if (errors.length) {
  failures++;
  console.log("FAIL unexpected extraction errors:", errors);
}
if (script + "\n" !== expected) {
  failures++;
  const a = expected.split("\n");
  const b = (script + "\n").split("\n");
  const i = a.findIndex((line, n) => line !== b[n]);
  console.log(`FAIL markers.sh differs at line ${i + 1}:`);
  console.log("  expected:", JSON.stringify(a[i]));
  console.log("  actual:  ", JSON.stringify(b[i]));
} else {
  console.log("ok   tests/markers.md -> tests/markers.sh");
}

// a directive missing its closing --> is reported (mkdoc.py exits 1 on it)
const broken = extractScript("<!-- ohpc_begin -->\n<!-- ohpc_if_set enable_x\n<!-- ohpc_end -->\n");
if (broken.errors.length === 1 && /missing closing/.test(broken.errors[0])) {
  console.log("ok   unterminated directive is reported");
} else {
  failures++;
  console.log("FAIL unterminated directive not reported:", broken.errors);
}

console.log(failures ? `${failures} failure(s)` : "all marker checks passed");
process.exit(failures ? 1 : 0);
