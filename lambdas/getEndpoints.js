const getEndpoints = require('../output/DB').getEndpoints

module.exports.handler = async (event, context, callback) => {
  try {
    const res = await getEndpoints()
    return res
  } catch (err) {
    console.log('err', err)
    return { statusCode: 500, body: "Error"}
  }
}
