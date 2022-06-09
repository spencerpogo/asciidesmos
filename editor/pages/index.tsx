import init, { js_closure_test } from "desmosc-wasm";
import { useEffect, useState } from "react";
import { basicSetup } from "codemirror";
import CodeMirror from "@uiw/react-codemirror";

enum State {
  Loading,
  Error,
  Ready,
}

const Main = () => {
  const [state, setState] = useState(State.Loading);
  const [value, setValue] = useState("test");
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
  return <CodeMirror value={value} height="200px" extensions={[basicSetup]} />;
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
