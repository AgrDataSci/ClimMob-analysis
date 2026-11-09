"""Tiny Mustache renderer (variables, sections, inverted sections, {{{raw}}}).
Enough for the feedback templates, so the Python and R (whisker) renderers agree."""
import re, html

TAG = re.compile(r"\{\{\{\s*([\w.]+)\s*\}\}\}|\{\{\s*([#^/!]?)\s*([\w.]+)\s*\}\}")


def lookup(stack, key):
    if key == ".":
        top = stack[-1]
        return top["."] if isinstance(top, dict) and "." in top else top
    for ctx in reversed(stack):
        cur = ctx
        ok = True
        for part in key.split("."):
            if isinstance(cur, dict) and part in cur:
                cur = cur[part]
            else:
                ok = False; break
        if ok:
            return cur
    return None


def render(tpl, data):
    return _render(tpl, [data])


def _render(tpl, stack):
    out, pos = [], 0
    while True:
        m = TAG.search(tpl, pos)
        if not m:
            out.append(tpl[pos:]); break
        out.append(tpl[pos:m.start()])
        if m.group(1):
            v = lookup(stack, m.group(1)); out.append("" if v is None else str(v)); pos = m.end(); continue
        kind, key = m.group(2), m.group(3)
        if kind == "!":
            pos = m.end(); continue
        if kind in ("#", "^"):
            end = re.compile(r"\{\{\s*/\s*" + re.escape(key) + r"\s*\}\}")
            depth, scan = 1, m.end()
            while depth:
                nxt = TAG.search(tpl, scan)
                if not nxt: raise ValueError("unclosed section " + key)
                if nxt.group(3) == key and nxt.group(2) in ("#", "^"): depth += 1
                elif nxt.group(3) == key and nxt.group(2) == "/": depth -= 1
                scan = nxt.end()
            inner = tpl[m.end():nxt.start()]
            v = lookup(stack, key)
            truthy = bool(v) and v != [] and v != 0
            if kind == "#" and truthy:
                items = v if isinstance(v, list) else [v]
                for it in items:
                    out.append(_render(inner, stack + [it if isinstance(it, dict) else {".": it}]))
            if kind == "^" and not truthy:
                out.append(_render(inner, stack))
            pos = scan; continue
        v = lookup(stack, key)
        out.append("" if v is None else html.escape(str(v), quote=False)); pos = m.end()
    return "".join(out)
