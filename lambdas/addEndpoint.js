const addEndpoint = require('../output/DB').insertEndpoint

const headers = {
  'Access-Control-Allow-Origin': '*'
}

module.exports.handler = async (event, context, callback) => {
  try {
    const res = await addEndpoint(event.body)()
    return {
      statusCode: res.statusCode,
      headers,
      body: res.body
    }
  } catch (err) {
    console.log('err', err)
    return {
      statusCode: 500,
      headers,
      body: 'Error'
    }
  }
}
