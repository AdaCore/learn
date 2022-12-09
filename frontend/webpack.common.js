const path = require('path');
const {CleanWebpackPlugin} = require('clean-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const Chunks2JsonPlugin = require('chunks-2-json-webpack-plugin');
const ESLintPlugin = require('eslint-webpack-plugin');

const myEslintOptions = {
  extensions: [`js`, `jsx`, `ts`],
  exclude: [`node_modules`],
};

const ts_rule = function(env) {
  let sandbox = false;

  if (env && env.sandbox) {
    sandbox = true;
  }

  const ifdef_opts = {
    SANDBOX: sandbox,
    "ifdef-verbose": true,
    "ifdef-triple-slash": false
  };

  return {
    test: /\.tsx?$/,
    use: [
      { loader: 'ts-loader' },
      { loader: 'ifdef-loader', options: ifdef_opts },
    ],
    exclude: /node_modules/
  };
};

const js_rule = function(env) {
  return {
    enforce: 'pre',
    test: /\.js$/,
    use: [
       { loader: 'source-map-loader' },
       { loader: 'babel-loader' }
    ]
  };
};

const scss_rule = function(env) {
  const postcss_opts = {
    postcssOptions: {
      plugins: function () {
        return [ require('autoprefixer') ];
      }
    }
  };

  return {
    test: /\.(scss|sass)$/,
    use: [
      { loader: MiniCssExtractPlugin.loader },
      { loader: 'css-loader' },
      { loader: 'postcss-loader', options: postcss_opts },
      { loader: 'sass-loader' }
    ]
  };
};

const css_rule = function(env) {
  return {
    test: /\.css$/,
    use: [
      { loader: MiniCssExtractPlugin.loader },
      { loader: 'css-loader' }
    ],
  };
};

const font_rule = function(env) {
  const fontloader_opts = {
    name: '[name].[ext]?[hash]',
    outputPath: 'fonts/',
    publicPath: '/_static/fonts/'
  };

  return {
    test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
    use: [
      { loader: 'file-loader', options: fontloader_opts }
    ]
  };
};

const img_rule = function(env) {
  const imgloader_opts = {
    name: '[name].[ext]',
    outputPath: 'img/',
    publicPath: '/_static/img/'
  };

  return {
    test: /\.(png|jpe?g|gif)$/,
    use: [
      { loader: 'file-loader', options: imgloader_opts }
    ]
  };
};

module.exports = function(env) {
  return {
    entry: './src/index.ts',
    output: {
      filename: '[name].[hash].js',
      path: path.resolve(__dirname, 'dist', 'html', '_static')
    },
    resolve: {
      modules: ['node_modules'],
      extensions: ['.webpack.js', '.web.js', '.ts', '.tsx', '.js'],
    },
    optimization: {
      splitChunks: {
        chunks: 'all'
      }
    },

    module: {
      rules: [
        ts_rule(env),
        js_rule(env),
        scss_rule(env),
        css_rule(env),
        font_rule(env),
        img_rule(env),
      ]
    },
    target: 'web',

    plugins: [
      new CleanWebpackPlugin(),
      new MiniCssExtractPlugin({
        filename: '[name].[hash].css',
      }),
      new Chunks2JsonPlugin(),
      new ESLintPlugin(myEslintOptions),
    ]
  };
};
