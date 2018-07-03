const addEndpoint = require('../output/addEndpoint').main

module.exports.handler = async (event, context, callback) => {
  try {
    const res = await addEndpoint(event.body)()
    return res
  } catch (err) {
    console.log('err', err)
    return { statusCode: 500, body: "Error"}
  }
}
