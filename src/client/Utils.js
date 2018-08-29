var baseUrl = process.env.SERVER_URL

exports.baseUrl = baseUrl

exports.btoa = function(str) {
  return window.btoa(str)
}
