var webpack = require('webpack');
var path = require('path');

// variables
var isProduction = process.argv.indexOf('-p') >= 0;
var sourcePath = path.join(__dirname, './src');
var outPath = path.join(__dirname, '../public');

// plugins
var HtmlWebpackPlugin = require('html-webpack-plugin');
var ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
  context: sourcePath,
  entry: {
    app: './index.js'
  },
  output: {
    path: outPath,
    publicPath: '/',
    filename: 'js/[name].js',
  },
  devtool: "source-map",
  target: 'web',
  resolve: {
    extensions: ['.css','.js', '.jsx']
    // Fix webpack's default behavior to not load packages with jsnext:main module
    // https://github.com/Microsoft/TypeScript/issues/11677

  },
  module: {
    loaders: [
      // .js, .jsx
      {
        test: /\.jsx?$/,
        use: {
          loader: 'babel-loader',
          query: {
            presets: ['react','es2015','stage-1']
          }
        },
        exclude: /node_modules/
      },
      // global css from libs in node_modules
     {
        test: /\.css$/,
        include: /node_modules/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: {loader: 'css-loader',query: {url:true , root: '/' }}
        })
      },
      // css modules for components
      {
        test: /\.css$/,
        exclude: /node_modules/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            {
              loader: 'css-loader',
              query: {
                url:true,
                modules: true,
                sourceMap: !isProduction,
                importLoaders: 1,
                localIdentName: '[local]__[hash:base64:5]',
                root: '/'
              }
            },
            {
              loader: 'postcss-loader'
            }
          ]
        })
      },
      {
        test: /\.(eot|png|svg|[ot]tf|woff2?)(\?v=\d+\.\d+\.\d+)?$/,
        loader: 'url-loader',
        query: {limit: 10000,name: 'images/[hash].[ext]'}
      },
      // static assets
      { test: /\.html$/, use: 'html-loader' },
      { test: /\.png$/, use: 'url-loader?limit=10000&name=images/[hash].[ext]' },
      { test: /\.jpg$/, use: 'file-loader' },
    ],
  },
  plugins: [
     new webpack.LoaderOptionsPlugin({
      options: {
        context: sourcePath,
        postcss: [
          require('postcss-import')({ addDependencyTo: webpack }),
          require('postcss-url')(),
          require('postcss-cssnext')(),
          require('postcss-reporter')(),
          require('postcss-browser-reporter')({ disabled: isProduction }),
        ]
      }
    }),
    new webpack.optimize.AggressiveMergingPlugin(),
    new ExtractTextPlugin({
      filename: 'css/styles.css',
      disable: !isProduction
    }),
    new HtmlWebpackPlugin({
      css: [ "css/styles.css" ],
      chunks: ['app'],
      template: 'index.html',
      filename: 'index.html'
    })
  ],
  devServer: {
    contentBase: sourcePath,
    hot: true,
    stats: {
      warnings: false
    },
    proxy: {
      '/players': {
        target: 'http://localhost:8080',
        secure: false
      }
    }
  },
  node: {
    // workaround for webpack-dev-server issue
    // https://github.com/webpack/webpack-dev-server/issues/60#issuecomment-103411179
    fs: 'empty',
    net: 'empty'
  }
};
