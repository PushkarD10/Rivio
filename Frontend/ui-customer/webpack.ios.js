const {
  merge
} = require('webpack-merge');
const common = require('./webpack.config.js');
const webpack = require('webpack');
const packageJSON = require("./package.json");
var path = require('path');

const getOutputFileDir = (mode) => mode == "development" ? "" : "ios/";
const outputFileName = "index_bundle.js";
const getOutputFileLocation = (mode) => getOutputFileDir(mode) + outputFileName;

module.exports = (env, argv) => {
  let plugins = [
    new webpack.DefinePlugin({
      __VERSION__: JSON.stringify(packageJSON.version),
      "window.configEnv": JSON.stringify(env)
    })
  ]
  return merge(common(env), {
    output: {
      filename: getOutputFileLocation(argv.mode),
      sourceMapFilename: getOutputFileLocation(argv.mode) + ".map"
    },
    plugins: plugins,
    devServer: {
      contentBase: path.join(__dirname, 'dist'),
      host: "0.0.0.0",
      inline: false,
      port: 8084
    }
  });
}