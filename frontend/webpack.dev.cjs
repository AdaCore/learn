const fs = require('fs');
const path = require('path');
const { merge } = require('webpack-merge');
const exec = require('child_process').exec;
const WatchPlugin = require('webpack-watch-files-plugin').default;
const ShellPlugin = require('webpack-shell-plugin-next');
const common = require('./webpack.common.cjs');

module.exports = function(env) {
  const common_config = common(env);
  const unit = process.env.UNIT || '';

  // Validate UNIT before webpack starts so a typo produces a clean error.
  // The Makefile $(error) catches the same mistake for direct `make` CLI use.
  if (unit) {
    const confIni = path.join(__dirname, '..', 'content', unit, 'conf.ini');
    if (!fs.existsSync(confIni)) {
      console.error(`\nERROR: UNIT="${unit}" is not a valid content unit.`);
      console.error(`       No conf.ini found at: ${confIni}`);
      console.error(`       Check for typos.\n`);
      process.exit(1);
    }
  }

  const dev_config = {
    mode: 'development',
    devServer: {
      static: [path.join(__dirname, 'dist', 'html'), path.join(__dirname, 'dist')],
      compress: false,
      host: '0.0.0.0',
      port: 8080,
      hot: true,
      // liveReload: for UNIT builds the browser must reload after sphinx
      // rewrites the HTML. For full-site builds keep it off (HMR only).
      liveReload: !!unit,
    },
    output: {
      publicPath:  '/_static/'
    },
    devtool: 'source-map',
    // /vagrant/{content,frontend} are VirtualBox shared folders (vboxsf).
    // vboxsf does not generate inotify events, so webpack's default
    // inotify-based watcher never fires for RST file changes. Force polling
    // when UNIT is set so WatchPlugin can detect edits made on the host.
    ...(unit ? { watchOptions: { poll: 1000, ignored: /node_modules/ } } : {}),
    plugins: [
      new ShellPlugin({
        onBuildStart:{
          // Skip cleanall for per-unit builds: the doctree cache must survive
          // across webpack recompilations so that subsequent sphinx invocations
          // are incremental (near-instant) rather than full rebuilds.
          scripts: [unit ? 'echo "UNIT build: skipping cleanall"' : 'make cleanall'],
          blocking: true,
          parallel: false
        },
        onBuildExit:{
          scripts: [`make local${unit ? ' UNIT=' + unit : ''}`],
          // blocking for UNIT builds: webpack must not signal "done" until
          // sphinx has finished writing HTML, otherwise the live-reload fires
          // before the new page content is on disk.
          blocking: !!unit,
          parallel: false
        },
        // dev=false here to force every build to trigger make, the default is
        // first build only.
        dev: false,
      }),
      // Watch RST files so that editing content triggers a sphinx rebuild via
      // the ShellPlugin's onBuildExit. Scoped to the target unit when UNIT is
      // set, otherwise watches all content.
      // Polling is used because vboxsf does not generate inotify events.
      new WatchPlugin({
        files: [
          path.join(
            __dirname, '..', 'content',
            unit ? `${unit}/**/*.rst` : '**/*.rst'
          )
        ]
      }),
    ]
  };

  return merge(common_config, dev_config);
}
