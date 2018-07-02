const ping = require('../output/Ping').main

module.exports.handler = async (event, context, callback) => {
  const url = event.queryStringParameters.url
  const res = await ping(url)()
  const data = {
    body: JSON.stringify( res.result )
  }
  return data
}
