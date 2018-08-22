const getPings = require('../output/DB').getPings

module.exports.handler = async (event, context, callback) => {
  try {
    const pings = await getPings()
    return {statusCode: 200, body: pings }
  } catch (err) {
    console.log('err', err)
    return { statusCode: 500, body: "Error"}
  }
}
