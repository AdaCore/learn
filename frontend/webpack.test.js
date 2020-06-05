const path = require('path');
const webpack = require('webpack');
const nodeExternals = require('webpack-node-externals');

const isCoverage = function(env) {
  if (process.env.NODE_ENV === 'coverage') {
    return {
      test: /\.tsx?$/,
      include: path.resolve('src'), // instrument only testing sources with Istanbul, after ts-loader runs
      loader: 'istanbul-instrumenter-loader',
      exclude: /node_modules/
    };
  }
  return {};
}

module.exports = function(env) {
  return {
    entry: [
      './src/index.ts'
    ],
    output: {
      // use absolute paths in sourcemaps (important for debugging via IDE)
      devtoolModuleFilenameTemplate: '[absolute-resource-path]',
      devtoolFallbackModuleFilenameTemplate: '[absolute-resource-path]?[hash]'
    },
    resolve: {
      modules: [
        path.resolve('./src'),
      ],
      extensions: ['.ts']
    },
    module: {
      rules: [
        isCoverage(env),
        {
          test: /\.ts$/,
          exclude: /node_modules/,
          loader: 'ts-loader'
        }
      ],
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

};