import init, { greet, try_eval } from "desmosc-wasm";
import { useEffect, useState } from "react";

enum State {
  Loading,
  Error,
  Ready,
}

const Main = () => {
  const [state, setState] = useState(State.Loading);
  useEffect(() => {
    init()
      .then(() => setState(State.Ready))
      .catch((e) => {
        console.error(e);
        setState(State.Error);
      });
  }, []);
  if (state == State.Error)
    return <p>Error loading WASM. Check console for details.</p>;
  if (state == State.Loading) return <p>Loading...</p>;
  const res = try_eval("1+1;");
  return <pre>{res.output}</pre>;
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
