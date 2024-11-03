const { merge } = require('webpack-merge');
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");
const TerserPlugin = require('terser-webpack-plugin');
const common = require('./webpack.common.cjs');

const terser_config = function(env) {
  const staging = (env && env.staging);
  return {
    // cache: true,
    parallel: true,
    // sourceMap: staging,
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
        (compiler) => {
          new TerserPlugin(terser_config(env)).apply(compiler);
        },
        new CssMinimizerPlugin({}),
      ],
    }
  };

  return merge(common_config, prod_config);
};
