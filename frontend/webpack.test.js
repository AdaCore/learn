const path = require('path');
const webpack = require('webpack');
const nodeExternals = require('webpack-node-externals');
const isCoverage = process.env.NODE_ENV === 'coverage';

module.exports = {
  entry: [
    './src/index.ts'
  ],
  output: {
    // use absolute paths in sourcemaps (important for debugging via IDE)
    devtoolModuleFilenameTemplate: '[absolute-resource-path]',
    devtoolFallbackModuleFilenameTemplate: '[absolute-resource-path]?[hash]'
  },
  module: {
    rules: [].concat(
      isCoverage ? {
          test: /\.(js|ts)/,
          include: path.resolve('src'), // instrument only testing sources with Istanbul, after ts-loader runs
          loader: 'istanbul-instrumenter-loader'
      }: [],
      {
          test: /.js$/,
          exclude: /(node_modules|bower_components)/,
          loader: 'babel-loader',
      },
      {
          test: /\.ts$/,
          exclude: /(node_modules|bower_components)/,
          loader: 'ts-loader'
      }
    ),
  },
  target: 'node',  // webpack should compile node compatible code
  externals: [nodeExternals()], // in order to ignore all modules in node_modules folder

  plugins: [
    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery",
    })
  ]
};