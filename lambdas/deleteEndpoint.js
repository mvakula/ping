const deleteEndpoint = require('../output/DB').deleteEndpoint

const headers = {
  'Access-Control-Allow-Origin': '*'
}

module.exports.handler = async (event, context, callback) => {
  try {
    const res = await deleteEndpoint(event.body)()
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
