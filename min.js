WebAssembly.instantiateStreaming(fetch("minimal.wasm")).then(
    (obj) => {
        x = obj.instance.exports.main();
        console.log(x)
    },
);