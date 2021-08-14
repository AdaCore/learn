const path = require('path');
const { merge } = require('webpack-merge');
const exec = require('child_process').exec;
const WatchPlugin = require('webpack-watch-files-plugin').default;
const ShellPlugin = require('webpack-shell-plugin-next');
const common = require('./webpack.common.js');

module.exports = function(env) {
  const common_config = common(env);

  const dev_config = {
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
        onBuildStart:{
          scripts: ['make cleanall -j4'],
          blocking: true,
          parallel: false
        },
        onBuildExit:{
          scripts: ['make local -j4'],
          blocking: false,
          parallel: true
        },
        // dev=false here to force every build to trigger make, the default is
        // first build only.
        dev: false,
      }),
    ]
  };

  return merge(common_config, dev_config);
}
