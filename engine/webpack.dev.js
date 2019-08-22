const path = require('path');
const merge = require('webpack-merge');
const exec = require('child_process').exec;
const WatchPlugin = require('webpack-watch-files-plugin').default;
const ShellPlugin = require('webpack-shell-plugin');
const common = require('./webpack.common.js');

module.exports = merge(common, {
  mode: 'development',
  watch: true,
  devServer: {
    contentBase: path.join(__dirname, '_build', 'html'),
    watchContentBase: true,
    compress: false,
    port: 8080,
    hot: false,
    liveReload: true,
    index: 'index.html',
    publicPath:  '/_static/'
  },
  devtool: 'source-map',
  plugins: [
    new ShellPlugin({
      onBuildStart: ['make clean -j4'],
      onBuildEnd: ['make local -j4'],
      // dev=false here to force every build to trigger make, the default is
      // first build only.
      dev: true,
    }),
  ]
});
