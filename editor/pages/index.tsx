import { acceptCompletion, startCompletion } from "@codemirror/autocomplete";
import { indentWithTab } from "@codemirror/commands";
import { keymap } from "@codemirror/view";
import {
  getNotifications,
  JSONRPCRequestData,
} from "@open-rpc/client-js/build/Request";
import { Transport } from "@open-rpc/client-js/build/transports/Transport";
import CodeMirror from "@uiw/react-codemirror";
import { basicSetup } from "codemirror";
import init, {
  js_closure_test,
  LspState,
  lsp_request,
  lsp_state_new,
} from "desmosc-wasm";
import { useEffect, useState } from "react";
import { languageServerWithTransport } from "../util/langServer";

enum State {
  Loading,
  Error,
  Ready,
}

export class MyTransport extends Transport {
  public log: (msg: string) => void;
  public state: LspState;

  constructor(log) {
    super();
    this.log = log;
    this.state = lsp_state_new();
    console.log = (...args) =>
      this.log(
        args.length == 1 && typeof args[0] === "string"
          ? args[0]
          : args
              .map((i) => {
                if (typeof i === "string" && /^[a-zA-Z0-9]*$/.test(i)) return i;
                try {
                  return JSON.stringify(i);
                } catch (e) {
                  return i.toString();
                }
              })
              .join(" ")
      );
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
    console.log("req", data);
    const r = lsp_request(
      this.state,
      JSON.stringify(this.parseData(data)),
      this.log
    );
    this.transportRequestManager.settlePendingRequest(notifications);
    if (!r) {
      console.log("resp empty");
      return;
    }
    console.log("resp", JSON.parse(r));
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
