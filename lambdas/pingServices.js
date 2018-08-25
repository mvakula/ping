const pingServices = require('../output/DB').pingServices

const headers = {
  'Access-Control-Allow-Origin': '*'
}

module.exports.handler = async (event, context, callback) => {
  try {
    const res = await pingServices()
    return {
      statusCode: 200,
      headers,
      body: JSON.stringify(res)
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
