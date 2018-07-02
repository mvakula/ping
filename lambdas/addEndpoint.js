const addEndpoint = require('../output/addEndpoint').main

module.exports.handler = async (event, context, callback) => {
  const res = await addEndpoint()
  return res
}
