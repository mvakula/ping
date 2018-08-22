const pingServices = require('../output/DB').pingServices

module.exports.handler = async (event, context, callback) => {
  try {
    const res = await pingServices()
    return res
  } catch (err) {
    console.log('err', err)
    return { statusCode: 500, body: "Error"}
  }
}
