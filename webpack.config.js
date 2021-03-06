// https://github.com/purescript/spago#get-started-from-scratch-with-webpack-frontend-projects
'use strict';

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');
const isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');
const isWatch = process.argv.some(a => a === '--watch');

const plugins =
  isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
;

module.exports = {
  mode: "production",
  // mode: "development",

  // devtool: 'eval-source-map',

  devServer: {
    contentBase: path.resolve(__dirname, 'dist'),
    port: 4008,
    stats: 'errors-only'
  },

  entry: './src/index.js',

  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js'
  },

  module: {
    rules: [
      // {
      //   test: /\.purs$/,
      //   use: [
      //     {
      //       loader: 'purs-loader',
      //       options: {
      //         src: [
      //           'src/**/*.purs'
      //         ],
      //         spago: true,
      //         watch: isWebpackDevServer || isWatch,
      //         pscIde: true
      //       }
      //     }
      //   ]
      // },
      {
        test: /\.(png|jpg|gif)$/i,
        use: [
          {
            loader: 'url-loader',
            options: {
              limit: 8192,
            },
          },
        ],
      },
    ]
  },

  resolve: {
    modules: [ 'node_modules', ],
    extensions: [ '.js']
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    }),
    new HtmlWebpackPlugin({
      title: 'purescript-webpack-example',
      template: './src/index.html',
      inject: false  // See https://stackoverflow.com/a/38292765/3067181
    })
  ].concat(plugins)
};