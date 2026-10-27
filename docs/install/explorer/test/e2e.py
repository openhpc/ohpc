#!/usr/bin/env python3
"""
End-to-end check of the recipe explorer with Playwright (Chromium by default;
--browser firefox or webkit for the others).

By default no web server is needed: requests to http://ohpc.test/ are routed
to files under docs/install, so the page runs exactly as it would on a static
host. Pass --url http://host:port/ to test a real server instead.

    python3 explorer/test/e2e.py                 # all recipes, screenshots to build/explorer-shots/
    python3 explorer/test/e2e.py --quick         # one recipe only
    python3 explorer/test/e2e.py --url http://127.0.0.1:8000/
    python3 explorer/test/e2e.py --url https://openhpc.github.io/ohpc/ --prefix 4.x/
    python3 explorer/test/e2e.py --browser firefox
    python3 explorer/test/e2e.py --browser firefox --channel moz-firefox   # the installed Firefox

Requires: pip install playwright && playwright install chromium (firefox, webkit)
"""

import argparse
import json
import mimetypes
import sys
import time
from pathlib import Path
from urllib.parse import urljoin

from playwright.sync_api import sync_playwright

BASE = Path(__file__).resolve().parent.parent.parent  # docs/install
ORIGIN = "http://ohpc.test/"


# The sample recipe for the interactive checks: a Rocky + Warewulf + Slurm
# path if the tree has one (4.x: rocky10-…, 3.x: rocky9-…), else the first.
def pick_sample(names):
    return next(
        (n for n in names if n.startswith("rocky") and n.endswith("-warewulf-slurm")),
        names[0],
    )


def recipe_axes(name):
    """rocky10-x86_64-warewulf-slurm -> ["rocky10", "x86_64", "warewulf", "slurm"]"""
    distro, arch, provisioner, scheduler = name.split("-", 3)
    return [distro, arch, provisioner, scheduler]


SERVE_ROOT = BASE


def serve_from_disk(route, request):
    rel = request.url[len(ORIGIN) :].split("?")[0].split("#")[0]
    path = SERVE_ROOT / rel
    if path.is_dir():
        path = path / "index.html"
    if not path.is_file():
        route.fulfill(status=404, body=f"not found: {rel}")
        return
    ctype = mimetypes.guess_type(str(path))[0] or "application/octet-stream"
    if path.suffix in (".j2", ".conf", ".yaml", ".md"):
        ctype = "text/plain; charset=utf-8"
    route.fulfill(status=200, body=path.read_bytes(), content_type=ctype)


def click_in_sidebar(page, selector):
    """Click a sidebar control as a reader would: scroll the sidebar to it,
    then click with the mouse. locator.click() scrolls it into view itself,
    which in Firefox moves the page too."""
    page.evaluate(
        """(sel) => {
            const sb = document.querySelector('.sidebar');
            const top = document.querySelector(sel).getBoundingClientRect().top;
            sb.scrollTop += top - sb.getBoundingClientRect().top - 200;
        }""",
        selector,
    )
    box = page.locator(selector).bounding_box()
    page.mouse.click(box["x"] + 5, box["y"] + box["height"] / 2)


def load_versions(page, page_url, args):
    """The versions.json the page probes (two levels above explorer/), or None."""
    if not args.url:
        versions_file = (SERVE_ROOT / "versions.json") if args.root else None
        if versions_file and versions_file.exists():
            return json.loads(versions_file.read_text())
        return None
    resp = page.request.get(urljoin(page_url, "../../versions.json"))
    return resp.json() if resp.ok else None


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--url", help="serve from this base URL instead of disk")
    ap.add_argument(
        "--root",
        help="serve this directory from disk instead of docs/install (e.g. a mksite.py output)",
    )
    ap.add_argument(
        "--prefix",
        default="",
        help="path of docs/install below the served root, e.g. 4.x/ for a mksite.py site",
    )
    ap.add_argument("--quick", action="store_true", help="only render one recipe")
    ap.add_argument("--shots", default=str(BASE / "build" / "explorer-shots"))
    ap.add_argument("--headed", action="store_true")
    ap.add_argument(
        "--browser", choices=["chromium", "firefox", "webkit"], default="chromium"
    )
    ap.add_argument(
        "--channel",
        help="an installed browser instead of Playwright's, e.g. moz-firefox or chrome",
    )
    args = ap.parse_args()

    shots = Path(args.shots)
    shots.mkdir(parents=True, exist_ok=True)
    manifest = json.loads((BASE / "explorer" / "manifest.json").read_text())
    recipes = [r["name"] for r in manifest["recipes"]]
    sample = pick_sample(recipes)
    if args.quick:
        recipes = [sample]
    global SERVE_ROOT
    if args.root:
        SERVE_ROOT = Path(args.root).resolve()
    base_url = (args.url or ORIGIN).rstrip("/") + "/" + args.prefix
    page_url = base_url.rstrip("/") + "/explorer/index.html"

    failures = []
    console_errors = []
    with sync_playwright() as p:
        # the Chromium flags let it start inside a macOS sandbox
        launch_args = (
            ["--single-process", "--no-zygote"] if args.browser == "chromium" else []
        )
        browser = getattr(p, args.browser).launch(
            headless=not args.headed, args=launch_args, channel=args.channel
        )
        print(f"browser: {args.browser} {browser.version}")
        ctx = browser.new_context(viewport={"width": 1400, "height": 900})
        if not args.url:
            ctx.route(ORIGIN + "**", serve_from_disk)
        page = ctx.new_page()
        page.add_init_script(
            "Object.defineProperty(navigator, 'clipboard', {value: {writeText: "
            "text => { window.__copiedChapterLink = text; return Promise.resolve(); }}})"
        )
        page.on(
            "console",
            lambda m: (
                console_errors.append(f"{m.text} [{(m.location or {}).get('url', '')}]")
                if m.type == "error"
                else None
            ),
        )
        page.on("pageerror", lambda e: console_errors.append(f"pageerror: {e}"))

        # 1. chooser
        page.goto(page_url)
        page.wait_for_selector("body.ready", timeout=30000)
        chips = page.locator(".chip")
        n_chips = chips.count()
        print(f"chooser: {n_chips} chips across {page.locator('.axis').count()} axes")
        if page.locator(".axis").count() != 4:
            failures.append("expected 4 axes")
        page.screenshot(path=str(shots / "01-chooser.png"), full_page=True)

        # 2. click a path together
        for value in recipe_axes(sample):
            page.locator(f".chip[data-value='{value}']").click()
        page.wait_for_selector(".guide h1", timeout=60000)
        page.wait_for_function("document.querySelector('#status').hidden")
        if f"#{sample}" not in page.url:
            failures.append(f"hash not updated: {page.url}")
        h1s = page.locator(".guide h1").count()
        toc = page.locator("#toc .toc-chapter").count()
        conds = page.locator(".guide .cond[data-flag]").count()
        srcs = page.locator(".guide .src").count()
        print(
            f"guide: {h1s} chapters, {toc} toc chapters, {conds} optional blocks, {srcs} template sections"
        )
        if h1s < 10:
            failures.append(f"too few chapters rendered: {h1s}")
        if toc != h1s:
            failures.append(f"toc chapters {toc} != h1 count {h1s}")
        chapter_links = page.locator(".guide .chapter-link")
        if chapter_links.count() != h1s:
            failures.append(f"chapter links {chapter_links.count()} != h1 count {h1s}")
        deep_chapter = page.locator(".guide h1").nth(6).get_attribute("id")
        chapter_links.nth(6).click()
        page.wait_for_function("Boolean(window.__copiedChapterLink)")
        copied = page.evaluate("window.__copiedChapterLink")
        if f"chapter={deep_chapter}" not in copied:
            failures.append(f"copied chapter link missing chapter query: {copied}")
        page.goto("about:blank")
        page.goto(copied)
        page.wait_for_function(f"document.querySelector('#guide').dataset.recipe === '{sample}'")
        page.wait_for_function(
            "(id) => document.getElementById(id).getBoundingClientRect().top < 100",
            arg=deep_chapter,
        )
        if "chapter=" in page.url:
            failures.append("chapter query was not consumed after restoring the chapter")
        if conds == 0:
            failures.append("no optional-step blocks found")
        page.screenshot(path=str(shots / "02-guide-top.png"))

        # optional flags: enable_ib off by default → blocks folded; switch on
        off_before = page.locator(".guide .cond.off[data-flag='enable_ib']").count()
        page.locator("#options input[data-flag='enable_ib']").check()
        page.wait_for_function("location.hash.includes('enable_ib=1')")
        on_after = page.locator(
            ".guide .cond.on[data-flag='enable_ib'][data-want='1']"
        ).count()
        print(f"enable_ib: {off_before} folded before, {on_after} active after")
        if off_before == 0 or on_after == 0:
            failures.append("enable_ib toggle did not change blocks")

        # downloads
        for a in page.locator("#downloads a.dl").all():
            href = a.get_attribute("href")
            if not href or not href.startswith("blob:"):
                failures.append(f"download link not a blob: {href}")
        href = page.locator("#downloads a.dl").nth(1).get_attribute("href")
        script_text = page.evaluate("(u) => fetch(u).then(r => r.text())", href)
        if not script_text.startswith("#!/bin/bash"):
            failures.append("recipe.sh download does not start with shebang")
        expected = BASE / "build" / f"{sample}.sh"
        if expected.exists() and script_text != expected.read_text():
            failures.append("downloaded recipe.sh differs from build/*.sh")
        else:
            print(
                "recipe.sh download matches build output"
                if expected.exists()
                else "recipe.sh downloaded (no build/ to compare)"
            )

        # flags whose steps exist only in recipe.sh sit in their own collapsed group
        misplaced = page.evaluate(
            """() => [...new Set([...document.querySelectorAll('.guide .cond[data-flag]')]
                .map((c) => c.dataset.flag))]
              .filter((f) => !document.querySelector(
                  `.guide .cond[data-flag="${f}"]:not(.script-only)`))
              .filter((f) => !document.querySelector(
                  `#options .script-only-flags input[data-flag="${f}"]`))"""
        )
        if misplaced:
            failures.append(f"recipe.sh-only flags outside the collapsed group: {misplaced}")
        grouped = page.locator("#options .script-only-flags input[data-flag]").count()
        print(f"flags in the 'only in recipe.sh' group: {grouped}")

        # pick-one settings: the selector writes the hash and the input.local
        choices = json.loads((BASE / "explorer" / "manifest.json").read_text())[
            "choices"
        ]
        for c in choices:
            other = next(v for v in c["choices"] if v != c["default"])
            # instant: the page scrolls smoothly, and WebKit would still be
            # moving when scrollY is read
            page.evaluate(
                "window.scrollTo({ top: document.body.scrollHeight / 2, behavior: 'instant' })"
            )
            page.wait_for_timeout(200)
            y_before = page.evaluate("window.scrollY")
            # set it from script: select_option would scroll the selector into view
            page.locator(f"#options select[data-choice='{c['name']}']").evaluate(
                "(e, v) => { e.value = v; e.dispatchEvent(new Event('change')); }", other
            )
            page.wait_for_function(f"location.hash.includes('{c['name']}={other}')")
            page.wait_for_timeout(300)
            y_after = page.evaluate("window.scrollY")
            if abs(y_after - y_before) > 50:
                failures.append(f"picking {c['name']} moved the reader: {y_before} -> {y_after}")
            href = page.locator("#downloads a.dl").nth(2).get_attribute("href")
            local = page.evaluate("(u) => fetch(u).then(r => r.text())", href)
            line = f'{c["name"]}="${{{c["name"]}:-{other}}}"'
            if line not in local:
                failures.append(f"input.local download lacks {line}")
            # the guide's commands show the pick; the recipe.sh download does not
            shown_default = f"${{{c['name']}:-{other}}}"
            guide_code = page.locator(".guide pre").all_text_contents()
            if not any(shown_default in t for t in guide_code):
                failures.append(f"guide commands do not show {shown_default}")
            href = page.locator("#downloads a.dl").nth(1).get_attribute("href")
            script = page.evaluate("(u) => fetch(u).then(r => r.text())", href)
            if shown_default in script:
                failures.append("recipe.sh download changed with the pick")
            page.goto(f"{page_url}#{sample}?{c['name']}=not-a-choice")
            page.wait_for_function(
                f"document.querySelector('#guide').dataset.recipe === '{sample}'"
            )
            shown = page.locator(
                f"#options select[data-choice='{c['name']}']"
            ).input_value()
            if shown != c["default"]:
                failures.append(f"{c['name']}: a value not in the list was taken")
            print(f"{c['name']}: {other} in hash and input.local; bad value ignored")
        if not choices:
            print("no pick-one settings in this tree: selector checks skipped")

        # a hostile/garbled flag name in the hash must not break the page
        page.goto(f"{page_url}#{sample}?enable_(=1&enable_ib=1")
        page.wait_for_function(
            f"document.querySelector('#guide').dataset.recipe === '{sample}'"
        )
        if page.locator("#status .error").count():
            failures.append("garbled flag in hash broke the render")
        # "start over" (bare hash) returns to the chooser
        page.goto(f"{page_url}#")
        page.wait_for_function("!document.body.classList.contains('reading')")
        if page.locator(".chip.selected").count():
            failures.append("start over left chips selected")
        # an unknown recipe name falls back to the chooser too
        page.goto(f"{page_url}#rocky99-x86_64-warewulf-slurm")
        page.wait_for_timeout(200)
        if page.locator(".chip.selected").count() or page.locator(".guide h1").count():
            failures.append("unknown recipe did not fall back to the chooser")
        # back to the guide for the remaining checks
        page.goto(f"{page_url}#{sample}?enable_ib=1")
        page.wait_for_function(
            f"document.querySelector('#guide').dataset.recipe === '{sample}'"
        )

        # jump via TOC, screenshot mid-document
        page.locator("#toc .toc-chapter a").nth(4).click()
        page.wait_for_timeout(600)
        page.screenshot(path=str(shots / "03-guide-chapter.png"))

        # a TOC jump is not a deep link: it must not write chapter= into the
        # URL, and a later view-switch re-render must leave the reader in
        # place (toggles used to jump back to the last jump target)
        if "chapter=" in page.url:
            failures.append(f"TOC jump wrote a chapter query into the URL: {page.url}")
        page.evaluate(
            "window.scrollTo({ top: document.body.scrollHeight - 900, behavior: 'instant' })"
        )
        page.wait_for_timeout(200)
        y_before = page.evaluate("window.scrollY")
        click_in_sidebar(page, "#developer > summary")  # the developer switches
        click_in_sidebar(page, "#sw-sources")
        page.wait_for_timeout(300)
        y_after = page.evaluate("window.scrollY")
        if abs(y_after - y_before) > 5:
            failures.append(f"toggling a view switch moved the reader: {y_before} -> {y_after}")
        page.locator("#sw-sources").click()  # back to the default
        page.wait_for_timeout(200)

        # provenance badge appears on hover, with template sources switched on
        page.locator("#sw-sources").check()
        first_src = page.locator(".guide .src").nth(6)
        first_src.hover()
        badge_visible = first_src.locator("> .src-badge").is_visible()
        page.locator("#sw-sources").uncheck()

        # shell conditions (ohpc_if) are labelled only on request
        visible_labels = (
            "Array.from(document.querySelectorAll('.guide .cond.cond-expr > .cond-label'))"
            ".filter((e) => e.getClientRects().length).length"
        )
        hidden = page.evaluate(visible_labels) == 0
        page.locator("#sw-conditions").check()
        shown = page.evaluate(visible_labels) > 0
        page.locator("#sw-conditions").uncheck()
        print(f"condition labels: hidden by default {hidden}, shown on request {shown}")
        if not (hidden and shown):
            failures.append("show recipe conditions does not toggle the labels")
        print(f"source badge visible on hover: {badge_visible}")
        if not badge_visible:
            failures.append("provenance badge not visible on hover")

        # version switcher: present only on a multi-version site
        versions = load_versions(page, page_url, args)
        if versions:
            n_versions = len(versions["versions"])
            page.wait_for_selector("#version", timeout=10000)
            options = page.locator("#version option").count()
            selected = page.locator("#version").input_value()
            print(f"versions: {options} in switcher, current {selected}")
            if options != n_versions:
                failures.append(
                    f"version switcher has {options} entries, versions.json {n_versions}"
                )
            if selected.rstrip("/") != args.prefix.rstrip("/"):
                failures.append(
                    f"switcher selected {selected!r}, expected {args.prefix!r}"
                )
            other = page.locator("#version option:not([selected])").first.get_attribute(
                "value"
            )
            page.select_option("#version", other)
            page.wait_for_selector("body.ready", timeout=30000)
            # the other version keeps the recipe if it has one of that name,
            # otherwise it shows its chooser (and drops the recipe from the hash)
            kept = f"#{sample}" in page.url
            chooser = not page.evaluate("document.body.classList.contains('reading')")
            if f"/{other}explorer/" not in page.url or not (kept or chooser):
                failures.append(
                    f"switching version did not navigate as expected: {page.url}"
                )
            page.goto(f"{page_url}#{sample}?enable_ib=1")
            page.wait_for_function(
                f"document.querySelector('#guide').dataset.recipe === '{sample}'"
            )
        elif page.locator("#version").count():
            failures.append("version switcher shown without a versions.json")

        # dark mode + mobile
        page.emulate_media(color_scheme="dark")
        page.screenshot(path=str(shots / "04-dark.png"))
        page.emulate_media(color_scheme="light")
        # phone width: a fresh load, as a phone would do it
        page.set_viewport_size({"width": 390, "height": 844})
        page.goto(f"{page_url}#{sample}?enable_ib=1")
        page.wait_for_function(
            f"document.querySelector('#guide').dataset.recipe === '{sample}'"
        )
        page.screenshot(path=str(shots / "05-mobile.png"))
        scroll_w = page.evaluate("document.documentElement.scrollWidth")
        if scroll_w > 390:
            wide = page.evaluate(
                """() => Array.from(document.querySelectorAll('body *'))
                    .map(e => [e, e.getBoundingClientRect()])
                    .filter(([e, r]) => r.right > 392 && r.width > 40)
                    .slice(0, 8)
                    .map(([e, r]) => `${e.tagName}.${String(e.className).slice(0, 40)} right=${Math.round(r.right)}`)"""
            )
            failures.append(
                f"horizontal overflow on phone width: {scroll_w}px; widest: {wide}"
            )
        page.set_viewport_size({"width": 1400, "height": 900})

        # 3. every recipe via the hash
        timings = []
        for name in recipes:
            t0 = time.time()
            page.goto(f"{page_url}#{name}")
            page.wait_for_function(
                "(n) => document.querySelector('#guide').dataset.recipe === n || document.querySelector('#status .error')",
                arg=name,
                timeout=90000,
            )
            err = page.locator("#status .error")
            if err.count():
                failures.append(f"{name}: {err.first.inner_text()[:300]}")
                print(f"FAIL {name}")
                continue
            h1 = page.locator(".guide h1").count()
            dt = time.time() - t0
            timings.append(dt)
            print(f"ok   {name}: {h1} chapters in {dt:.2f}s")
        if timings:
            print(
                f"render time: avg {sum(timings) / len(timings):.2f}s max {max(timings):.2f}s"
            )

        browser.close()

    # versions.json is probed and expected to 404 on a bare docs/install tree
    real_errors = [
        e for e in console_errors if "favicon" not in e and "versions.json" not in e
    ]
    if real_errors:
        print("console errors:")
        for e in real_errors[:20]:
            print("  " + e[:300])
        failures.append(f"{len(real_errors)} console error(s)")
    print("screenshots:", shots)
    if failures:
        print("FAILURES:")
        for f in failures:
            print("  - " + f)
        return 1
    print("all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
