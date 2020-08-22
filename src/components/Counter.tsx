import { useState } from "react";
import React from "react";

const Counter = () => {
  const [counter, setCounter] = useState(0);

  return (
    <div>
      <p>You clicked {counter} times</p>
      <button onClick={() => setCounter((x) => x + 1)}>Click</button>
    </div>
  );
};

export default Counter