#!/usr/bin/env python3
"""sk-v27 LW2.TOPO-4: a throwaway local alternative registry (sparse index + publish API).

Crates published here shadow crates.io by name; every other name is read through from
index.crates.io and static.crates.io, so `source.crates-io.replace-with` can point at this
registry and the published crates resolve each other at their crates.io versions. Never
committed; configured on the command line only. Usage: server.py <store-dir> <port> [<names-file>]
"""
import hashlib, json, os, struct, sys, threading, urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

STORE, PORT = sys.argv[1], int(sys.argv[2])
# The crate set under dry-run: these names are served from this store only (empty until published),
# never read through, so a version already on crates.io does not mask the dry-run.
OURS = {n.strip().lower() for n in open(sys.argv[3]) if n.strip()} if len(sys.argv) > 3 else set()
os.makedirs(f"{STORE}/crates", exist_ok=True)
INDEX = f"{STORE}/index.jsonl"   # one published version per line
LOCK = threading.Lock()
UA = {"User-Agent": "lockstep-dry-run (generic client)"}

def local_lines(name):
    if not os.path.exists(INDEX): return []
    out = []
    for line in open(INDEX):
        e = json.loads(line)
        if e["name"].lower() == name.lower(): out.append(line.strip())
    return out

def fetch(url):
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=60) as r:
            return r.status, r.read()
    except urllib.error.HTTPError as e:
        return e.code, b""

class H(BaseHTTPRequestHandler):
    def log_message(self, fmt, *a):
        sys.stderr.write("%s %s\n" % (self.command, self.path))

    def send(self, code, body, ctype="application/json"):
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        p = self.path.split("?")[0]
        if p == "/index/config.json":
            cfg = {"dl": f"http://127.0.0.1:{PORT}/dl/{{crate}}/{{version}}/download", "api": f"http://127.0.0.1:{PORT}"}
            return self.send(200, json.dumps(cfg).encode())
        if p.startswith("/index/"):
            rel = p[len("/index/"):]
            name = rel.rsplit("/", 1)[-1]
            with LOCK:
                mine = local_lines(name)
            if mine or name.lower() in OURS:
                if not mine:
                    return self.send(404, b"", "text/plain")
                return self.send(200, ("\n".join(mine) + "\n").encode(), "text/plain")
            code, body = fetch(f"https://index.crates.io/{rel}")
            return self.send(code if code == 200 else 404, body, "text/plain")
        if p.startswith("/dl/"):
            _, _, name, vers, _ = p.split("/")
            f = f"{STORE}/crates/{name}-{vers}.crate"
            if os.path.exists(f):
                return self.send(200, open(f, "rb").read(), "application/octet-stream")
            if name.lower() in OURS:
                return self.send(404, b"", "application/octet-stream")
            code, body = fetch(f"https://static.crates.io/crates/{name}/{name}-{vers}.crate")
            return self.send(code if code == 200 else 404, body, "application/octet-stream")
        self.send(404, b"{}")

    def do_PUT(self):
        if self.path != "/api/v1/crates/new":
            return self.send(404, b"{}")
        body = self.rfile.read(int(self.headers["Content-Length"]))
        (jl,) = struct.unpack("<I", body[:4]); meta = json.loads(body[4:4 + jl])
        (cl,) = struct.unpack("<I", body[4 + jl:8 + jl]); crate = body[8 + jl:8 + jl + cl]
        deps = []
        for d in meta["deps"]:
            e = {"name": d.get("explicit_name_in_toml") or d["name"], "req": d["version_req"], "features": d["features"],
                 "optional": d["optional"], "default_features": d["default_features"], "target": d.get("target"),
                 "kind": d.get("kind", "normal"), "registry": None}
            if d.get("explicit_name_in_toml"): e["package"] = d["name"]
            deps.append(e)
        line = {"name": meta["name"], "vers": meta["vers"], "deps": deps, "cksum": hashlib.sha256(crate).hexdigest(),
                "features": {}, "features2": meta.get("features", {}), "yanked": False, "links": meta.get("links"), "v": 2}
        with LOCK:
            if any(json.loads(l)["vers"] == meta["vers"] for l in local_lines(meta["name"])):
                return self.send(400, json.dumps({"errors": [{"detail": f"{meta['name']}@{meta['vers']} already exists"}]}).encode())
            open(f"{STORE}/crates/{meta['name']}-{meta['vers']}.crate", "wb").write(crate)
            with open(INDEX, "a") as f: f.write(json.dumps(line) + "\n")
        sys.stderr.write(f"PUBLISHED {meta['name']} {meta['vers']} {line['cksum']}\n")
        self.send(200, json.dumps({"warnings": {"invalid_categories": [], "invalid_badges": [], "other": []}}).encode())

ThreadingHTTPServer(("127.0.0.1", PORT), H).serve_forever()
