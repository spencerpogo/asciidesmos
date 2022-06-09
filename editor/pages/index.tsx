import init, { greet } from "desmosc-wasm";
import { useEffect, useState } from "react";

const IndexPage = () => {
  const [ready, setReady] = useState(false);
  useEffect(() => {
    init().then(() => setReady(true));
  }, []);
  return (
    <>
      <h1>Test App</h1>
      <pre>{ready ? greet("rust + next.js") : "Loading..."}</pre>
    </>
  );
};

export default IndexPage;
