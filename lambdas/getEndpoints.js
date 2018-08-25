const getEndpoints = require('../output/DB').getEndpoints

const headers = {
  'Access-Control-Allow-Origin': '*'
}

module.exports.handler = async (event, context, callback) => {
  try {
    const endpoints = await getEndpoints()
    return {
      statusCode: 200,
      headers,
      body: JSON.stringify(endpoints)
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
