if (process.env.NODE_ENV === 'development') {
  require("../output/Main/index.js").main()
} else {
  require("./bundle.js")
}
