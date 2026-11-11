#!/usr/bin/env python3
"""sk-v27 LW2.TOPO-4d: crates.io as it would read after the dry-run publish (read-only).

Every name is read through from index.crates.io; the versions a dry-run store holds
(`<store>/index.jsonl`, written by LW2.GATE/s16-topo4/registry-server.py) are appended to
their name's index body, and their tarballs are served from the store. Every other download
reads through from static.crates.io. So a fresh resolve against this registry is the fresh
resolve a published consumer would get once the publish set is on crates.io. Never committed;
configured on the command line only. Usage: union-registry.py <store-dir> <port>
"""
import json, os, sys, urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

STORE, PORT = sys.argv[1], int(sys.argv[2])
UA = {"User-Agent": "lockstep-dry-run (generic client)"}

def store_lines(name):
    p = f"{STORE}/index.jsonl"
    if not os.path.exists(p): return []
    return [l.strip() for l in open(p) if json.loads(l)["name"].lower() == name.lower()]

def fetch(url):
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=60) as r:
            return r.status, r.read()
    except urllib.error.HTTPError as e:
        return e.code, b""

class H(BaseHTTPRequestHandler):
    def log_message(self, fmt, *a):
        sys.stderr.write("%s %s\n" % (self.command, self.path))

    def send(self, code, body, ctype="text/plain"):
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        p = self.path.split("?")[0]
        if p == "/index/config.json":
            cfg = {"dl": f"http://127.0.0.1:{PORT}/dl/{{crate}}/{{version}}/download"}
            return self.send(200, json.dumps(cfg).encode(), "application/json")
        if p.startswith("/index/"):
            rel = p[len("/index/"):]
            mine = store_lines(rel.rsplit("/", 1)[-1])
            code, body = fetch(f"https://index.crates.io/{rel}")
            lines = [l for l in body.decode().splitlines() if l.strip()] if code == 200 else []
            held = {json.loads(l)["vers"] for l in lines}
            lines += [l for l in mine if json.loads(l)["vers"] not in held]
            return self.send(200, ("\n".join(lines) + "\n").encode()) if lines else self.send(404, b"")
        if p.startswith("/dl/"):
            _, _, name, vers, _ = p.split("/")
            f = f"{STORE}/crates/{name}-{vers}.crate"
            if os.path.exists(f):
                return self.send(200, open(f, "rb").read(), "application/octet-stream")
            code, body = fetch(f"https://static.crates.io/crates/{name}/{name}-{vers}.crate")
            return self.send(code if code == 200 else 404, body, "application/octet-stream")
        self.send(404, b"")

ThreadingHTTPServer(("127.0.0.1", PORT), H).serve_forever()
