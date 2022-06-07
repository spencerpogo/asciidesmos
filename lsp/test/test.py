import subprocess as sp
import json
import sys


def enc_msg(msg):
    msg_enc = json.dumps(msg)
    return f"Content-Length: {len(msg_enc)}\r\n\r\n{msg_enc}".encode()


def send(p, msg, dbg=False):
    data = enc_msg(msg)
    if dbg:
        print(data)
    p.stdin.write(data)
    p.stdin.flush()


def read(p, dbg=False):
    log = lambda *a, **kw: None
    if dbg:
        log = print
    content_length = None
    while True:
        log("recv header line...")
        header = p.stdout.readline()
        log(header)
        if not header.endswith(b"\r\n"):
            raise ValueError("Improper line ending")
        header = header[:-2]
        if not header:
            break
        k, v = header.split(b": ")
        if k.lower() == b"content-length":
            content_length = int(v)
        else:
            raise ValueError(f"Invalid header name: {k}")
    if content_length is None:
        raise ValueError("Missing Content-Length header")
    log(f"recv {content_length} byte body...")
    body = p.stdout.read(content_length)
    log(body)
    return json.loads(body)


def main():
    p = sp.Popen(["./target/debug/desmosc", "--lsp"],
                 stdin=sp.PIPE, stdout=sp.PIPE, stderr=sys.stderr)
    send(p, {"jsonrpc": "2.0", "method": "initialize",
         "id": 1, "params": {"capabilities": {}}})
    print(read(p))
    send(p, {"jsonrpc": "2.0", "method": "initialized", "params": {}})
    send(p, {"jsonrpc": "2.0", "method": "textDocument/definition", "id": 2, "params": {
         "textDocument": {"uri": "file://temp"}, "position": {"line": 1, "character": 1}}})
    print("resp:", read(p))


if __name__ == "__main__":
    main()
