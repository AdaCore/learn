const merge = require('webpack-merge');
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const TerserPlugin = require('terser-webpack-plugin');
const common = require('./webpack.common.js');

const terser_config = function(env) {
  const staging = (env && env.staging);
  return {
    cache: true,
    parallel: true,
    sourceMap: staging,
    terserOptions: {
      output: {
        comments: staging,
      }
    }
  };
};

module.exports = function(env) {
  const common_config = common(env);
  const prod_config = {
    mode: 'production',
    optimization: {
      minimizer: [
        new TerserPlugin(terser_config(env)),
        new OptimizeCssAssetsPlugin({})
      ],
    }
  };

  return merge(common_config, prod_config);
};
