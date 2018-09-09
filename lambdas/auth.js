const { authString, generatePolicy } = require('../output/Auth')

module.exports.handler = (event, context, callback) => {
  const authToken = event.authorizationToken
  return authToken == authString
    ? callback(null, generatePolicy('user')('Allow')('*'))
    : callback('Unauthorized')
}
