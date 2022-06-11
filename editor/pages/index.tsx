import init, { js_closure_test, lsp_request } from "desmosc-wasm";
import { useEffect, useState } from "react";
import { basicSetup } from "codemirror";
import CodeMirror from "@uiw/react-codemirror";
import { Transport } from "@open-rpc/client-js/build/transports/Transport";
import {
  getNotifications,
  JSONRPCRequestData,
} from "@open-rpc/client-js/build/Request";
import { languageServerWithTransport } from "../util/langServer";
import { keymap } from "@codemirror/view";
import { indentWithTab } from "@codemirror/commands";
import { acceptCompletion, startCompletion } from "@codemirror/autocomplete";

enum State {
  Loading,
  Error,
  Ready,
}

export class MyTransport extends Transport {
  public log: (msg: string) => void;
  constructor(log) {
    super();
    this.log = log;
    console.log = (...args) =>
      args.length == 1 &&
      this.log(typeof args[0] === "string" ? args[0] : JSON.stringify(args));
  }
  public async connect(): Promise<void> {
    this.log("connect");
    this.transportRequestManager.transportEventChannel.addListener(
      "error",
      (...args) => this.log("emit error " + JSON.stringify(args))
    );
  }
  public close() {
    this.log("close");
  }
  public async sendData(
    data: JSONRPCRequestData,
    timeout?: number | null
  ): Promise<void> {
    const prom = this.transportRequestManager.addRequest(data, timeout);
    const notifications = getNotifications(data);
    console.log(data);
    const r = lsp_request(JSON.stringify(this.parseData(data)), () => {});
    this.transportRequestManager.settlePendingRequest(notifications);
    if (!r) {
      console.log("r empty");
      return;
    }
    console.log("r", JSON.parse(r));
    try {
      this.transportRequestManager.resolveResponse(r);
    } catch (e) {
      this.log("err " + JSON.stringify(e));
    }
    return prom;
  }
}

const Main = () => {
  const [state, setState] = useState(State.Loading);
  const [value, setValue] = useState("test");
  const [ls, setLs] = useState(null);
  const [logs, setLogs] = useState([]);

  useEffect(() => {
    if (state == State.Ready && ls === null) {
      setLs(
        languageServerWithTransport({
          transport: new MyTransport((msg) => setLogs((v) => v.concat([msg]))),
          rootUri: "file:///",
          documentUri: "file:///a.desmos",
          languageId: "desmos",
          workspaceFolders: [],
        })
      );
    }
  }, [state, ls]);

  useEffect(() => {
    init()
      .then(() => {
        setState(State.Ready);
        js_closure_test(setValue);
      })
      .catch((e) => {
        alert(e);
        setState(State.Error);
      });
  }, []);

  if (state == State.Error)
    return <p>Error loading WASM. Check console for details.</p>;
  if (state == State.Loading) return <p>Loading...</p>;
  if (ls === null) return <p>Starting LSP...</p>;
  return (
    <>
      <CodeMirror
        value={value}
        height="200px"
        extensions={[
          basicSetup,
          ls,
          keymap.of([
            indentWithTab,
            { key: "Alt-Space", run: startCompletion },
            { key: "tab", run: acceptCompletion },
          ]),
        ]}
      />
      <pre>{logs.join("\n")}</pre>
    </>
  );
};

const IndexPage = () => {
  return (
    <>
      <h1>Test App</h1>
      <Main />
    </>
  );
};

export default IndexPage;
