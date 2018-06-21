const ping = require('./output/Ping').main

module.exports.ping = async (event, context, callback) => {
  const url = event.queryStringParameters.url
  const res = await ping(url)()
  const data = {
    body: JSON.stringify( res.result )
  }
  return data 
}
