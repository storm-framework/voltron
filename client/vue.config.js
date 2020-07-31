/* eslint-disable @typescript-eslint/no-var-requires */

module.exports = {
  productionSourceMap: false,
  devServer: {
    proxy: {
      "/api": {
        target: "http://127.0.0.1:3000",
        changeOrigin: true
      }
    }
  }
};
