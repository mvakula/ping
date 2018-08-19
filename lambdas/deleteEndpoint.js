const deleteEndpoint = require('../output/DB').deleteEndpoint

module.exports.handler = async (event, context, callback) => {
  try {
    const res = await deleteEndpoint(event.body)()
    return res
  } catch (err) {
    console.log('err', err)
    return { statusCode: 500, body: "Error"}
  }
}
