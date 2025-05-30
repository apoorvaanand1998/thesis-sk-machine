WebAssembly.instantiateStreaming(fetch("SKeleton.wasm")).then(
    (obj) => {
        x = obj.instance.exports.main();
        console.log(x)
    },
);

// serve this using - python3 -m http.server 8000
