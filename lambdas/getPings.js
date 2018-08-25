const getPings = require('../output/DB').getPings

const headers = {
  'Access-Control-Allow-Origin': '*'
}

module.exports.handler = async (event, context, callback) => {
  try {
    const pings = await getPings()
    return {
      statusCode: 200,
      headers,
      body: JSON.stringify(pings)
    }
  } catch (err) {
    console.log('err', err)
    return {
      statusCode: 500,
      headers,
      body: "Error"
    }
  }
}
