WebAssembly.instantiateStreaming(fetch("sk.wasm")).then(
    (obj) => {
        x = obj.instance.exports.main();
        console.log(x)
    },
);