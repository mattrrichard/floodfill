var HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    entry: [
        './src/index.js'
    ],

    output: {
        path: "./dist",
        filename: "index.js"
    },

    resolve: {
        extensions: ['', '.js', '.elm'],
        modules_directories: ['node_modules']
    },

    devServer: {
        inline: true,
        progress: true
    },

    plugins: [
        new HtmlWebpackPlugin({
            template: 'src/index.html',
            inject: 'body',
            filename: 'index.html'
        })
    ],

    module: {
        loaders: [{
            test: /\.elm$/,
            exclude: [/elm-stuff/, /node_modules/],
            loader: 'elm-webpack?verbose=true&warn=true'
        }]
    }
};
