import init, { js_closure_test } from "desmosc-wasm";
import { useEffect, useState } from "react";
import { basicSetup } from "codemirror";
import CodeMirror from "@uiw/react-codemirror";
import { Transport } from "@open-rpc/client-js/build/transports/Transport";
import { JSONRPCRequestData } from "@open-rpc/client-js/build/Request";

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
  }
  public async connect(): Promise<void> {
    this.log("connect");
  }
  public close() {
    this.log("close");
  }
  public async sendData(
    data: JSONRPCRequestData,
    timeout?: number | null
  ): Promise<void> {
    this.log(JSON.stringify(data));
  }
}

const Main = () => {
  const [state, setState] = useState(State.Loading);
  const [value, setValue] = useState("test");
  const [logs, setLogs] = useState([]);
  
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
  return (
    <>
      <CodeMirror value={value} height="200px" extensions={[basicSetup]} />
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
