const path = require('path');
const webpack = require('webpack');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const SassLintPlugin = require('sass-lint-webpack');


module.exports = {
  entry: [
    './sphinx/learn_theme/static/src/index.ts'
  ],
  output: {
    filename: 'main.bundle.js',
    chunkFilename: '[name].bundle.js',
    path: path.resolve(__dirname, 'sphinx', 'learn_theme', 'static', 'dist')
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
    noParse: [/ace-builds.*/],
    rules: [
      {
        test: /\.tsx?$/,
        loader: 'awesome-typescript-loader',
        exclude: /node_modules/,
      },
      {
        enforce: "pre",
        test: /\.js$/,
        loader: "source-map-loader"
      },
      {
        test: /\.(scss|sass)$/,
        use: [{
          loader: MiniCssExtractPlugin.loader
        }, {
          loader: 'css-loader'
        }, {
          loader: 'postcss-loader',
          options: {
            plugins: function () {
              return [
                require('autoprefixer')
              ];
            }
          }
        }, {
          loader: 'sass-loader',
        }]
      },
      {
        test: /\.css$/,
        use: [MiniCssExtractPlugin.loader, 'css-loader'],
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: '[name].[ext]?[hash]',
              outputPath: 'fonts/',
              publicPath: '/_static/dist/fonts/'
            }
          }
        ]
      },
      {
        test: /\.(png|jpe?g|gif)$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: '[name].[ext]',
              outputPath: 'img/',
              publicPath: '/_static/dist/img/'
            }
          }
        ]
      },
    ]
  },
  target: 'web',

  plugins: [
    new CleanWebpackPlugin(),
    new MiniCssExtractPlugin({
      filename: 'style.css',
    }),
    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery"
    }),
//    new SassLintPlugin(),
  ]

};
