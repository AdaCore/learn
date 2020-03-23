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
    contentBase: [path.join(__dirname, 'dist', 'html'), path.join(__dirname, 'dist')],
    watchContentBase: true,
    compress: false,
    host: '0.0.0.0',
    port: 8080,
    hot: true,
    liveReload: false,
    index: 'index.html',
    publicPath:  '/_static/'
  },
  devtool: 'source-map',
  plugins: [
    new ShellPlugin({
      onBuildStart: ['make cleanall'],
      onBuildEnd: ['make local'],
      // dev=false here to force every build to trigger make, the default is
      // first build only.
      dev: false,
    }),
  ]
});
