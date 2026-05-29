const path = require('path');
const { merge } = require('webpack-merge');
const exec = require('child_process').exec;
const WatchPlugin = require('webpack-watch-files-plugin').default;
const ShellPlugin = require('webpack-shell-plugin-next');
const common = require('./webpack.common.cjs');

module.exports = function(env) {
  const common_config = common(env);

  const dev_config = {
    mode: 'development',
    devServer: {
      static: [path.join(__dirname, 'dist', 'html'), path.join(__dirname, 'dist')],
      compress: false,
      host: '0.0.0.0',
      port: 8080,
      hot: true,
      liveReload: false,
      // devMiddleware: {
      //   index: 'index.html',
      // },
    },
    output: {
      publicPath:  '/_static/'
    },
    devtool: 'source-map',
    plugins: [
      new ShellPlugin({
        onBuildStart:{
          // Skip cleanall for per-unit builds: the doctree cache must survive
          // across webpack recompilations so that subsequent sphinx invocations
          // are incremental (near-instant) rather than full rebuilds.
          scripts: [process.env.UNIT ? 'echo "UNIT build: skipping cleanall"' : 'make cleanall'],
          blocking: true,
          parallel: false
        },
        onBuildExit:{
          scripts: [`make local${process.env.UNIT ? ' UNIT=' + process.env.UNIT : ''}`],
          blocking: false,
          parallel: false
        },
        // dev=false here to force every build to trigger make, the default is
        // first build only.
        dev: false,
      }),
      // Watch RST files so that editing content triggers a sphinx rebuild via
      // the ShellPlugin's onBuildExit. Scoped to the target unit when UNIT is
      // set, otherwise watches all content.
      new WatchPlugin({
        files: [
          path.join(
            __dirname, '..', 'content',
            process.env.UNIT ? `${process.env.UNIT}/**/*.rst` : '**/*.rst'
          )
        ]
      }),
    ]
  };

  return merge(common_config, dev_config);
}
