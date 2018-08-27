const { authString, generatePolicy } = require('../output/Auth')

module.exports.handler = (event, context, callback) => {
  const authToken = event.authorizationToken
  return authToken == authString
    ? callback(null, generatePolicy('userId123')('Allow')(event.methodArn))
    : callback('Unauthorized')
}
