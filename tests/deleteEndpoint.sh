curl http://localhost:3000/deleteEndpoint \
  -H "Content-Type: application/json" \
  --request DELETE \
  --data @tests/deleteEndpoint.json
