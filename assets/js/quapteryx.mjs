// Preserve a canceler, set as `promise.cancel()`
function thenWithCancel(promise, callback, onError=undefined) {
  if (typeof promise !== 'object' || typeof promise.then !== 'function') {
    try {
      return callback(promise);
    } catch(err) {
      return Promise.reject(err);
    }
  }
  let canceled = false;
  let canceler = () => {
    canceled = true;
  };
  let result = promise.then((value) => {
    let newPromise = callback(value);
    if (newPromise.cancel) canceler = (...arg) => {
      canceled = true;
      return newPromise.cancel(...arg);
    };
    if (canceled) canceler();
    return newPromise;
  }, onError);
  result.cancel = (...arg) => canceler(...arg);
  return result;
}

export function quapteryx(input) {
  const u64 = (value) => ((value + (1n<<64n)) % (1n<<64n));
  const u32 = (value) => ((value + (2**32)) % (2**32));

  // Round `l` up to the next multiple of `amt`
  const roundUp = (l, amt=64) => l + (amt - l%amt)%amt;

  // Convert quaternary to binary
  const q2b = i => {
    const digit = x => {const v = parseInt(x, 4); return isFinite(v) ? v : '`IKS'.indexOf(x)};
    return Array.from(i, x => digit(x).toString(2).padStart(2, 0)).join("");
  };
  // Binary to quaternary
  const b2q = i => {
    return i.padStart(roundUp(i.length, 2), 0).replace(/[01][01]/g, q => parseInt(q, 2).toString(4));
  };

  if (!quapteryx.compiled) {
    quapteryx.compiled = fetch('../assets/wasm/quapteryx.wasm')
      .then(response => WebAssembly.compileStreaming(response))
      .then(compiled => {
        quapteryx.compiled = compiled; // set it to a non-Promise
      });
  }
  if (quapteryx.compiled instanceof Promise) {
    // Already being fetched but not finished yet
    return thenWithCancel(quapteryx.compiled, () => quapteryx(input));
  }
  const instantiating = quapteryx.recycle ? quapteryx.recycle : WebAssembly.instantiate(quapteryx.compiled);
  return thenWithCancel(instantiating, (instance) => {
    // console.log(instance.exports);

    /** @type {WebAssembly.Memory} */
    const memory = instance.exports.memory;

    const littleEndian = true; // for `DataView`

    // Routine to set the input, given a quaternary string
    const setInput = (value) => {
      const inputView = new DataView(memory.buffer, Number(instance.exports.input_words.value));
      let bits = q2b(value);
      bits = bits.padEnd(roundUp(bits.length, 64), 0);
      for (let i=0; i<bits.length/64; i++) {
        const word = BigInt("0b" + bits.substring(64*i, 64*i+64));
        inputView.setBigUint64(8*i, word, littleEndian);
      }
    };
    // Routine to read the output as a quaternary string
    const getOutput = (len) => {
      const outputView = new DataView(memory.buffer, Number(instance.exports.output_words.value));
      len = Number(typeof len === 'bigint' ? u64(len) : u32(len));
      let output = new Array(roundUp(len, 32)/32);
      for (let i=0; i<output.length; i++) {
        output[i] = u64(outputView.getBigUint64(8*i, littleEndian));
      }
      return b2q(Array.from(output, bigint => bigint.toString(2).padStart(64, 0)).join("")).substring(0, len);
    };

    // Copy it into the input buffer
    setInput(input);
    // return getOutput(instance.exports.eval(BigInt(input.length))); // synchronous
    // Asynchronous loop until it is done
    instance.exports.setup(BigInt(input.length)); // set up the stack
    var calculating = quapteryx.manager(240, (fuel) => {
      return !!instance.exports.resume(BigInt(fuel)); // evaluate a little bit
    }, (done) => {
      if (false && !done) {
        instance.exports.cancel(); // cleanup
        // quapteryx.recycle = instance;
        return;
      }
      const output = getOutput(instance.exports.finalize()); // read the result
      // quapteryx.recycle = instance;
      return output;
    });
    return calculating;
  });
}

// Aim to take 30% of a 60fps frame window
quapteryx.target_time = 1000/60/3.3;
quapteryx.manager = (fuel, loop, exit) => {
  let canceler;
  let total_looped = 0;
  let total_fueled = 0;
  const loop1frame = () => { try {
    const start = performance.now();
    let done = false;
    let allowance = 6 * fuel;
    let last = start;
    let ms_taken = 1;
    let looped = 0;
    let fueled = 0;
    while (!(done = loop(allowance)) && (ms_taken = (last = performance.now()) - start) < quapteryx.target_time) {
      fueled += allowance;
      looped += 1;
      allowance = fuel;
    }
    total_looped += looped;
    total_fueled += fueled;
    console.debug({ looped, fueled, ms_taken, total_looped, total_fueled });
    if (done) return exit(true);
    return Object.assign(new Promise(resolve => {
      canceler = setTimeout(() => resolve(loop1frame()), 10);
    }), { cancel: () => {
      clearTimeout(canceler);
      return exit(false);
    } });
  } catch(err) {
    return Promise.reject(err);
  } };
  return loop1frame();
};
