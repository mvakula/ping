
## Setup
Install [Serverless](https://github.com/serverless/serverless#quick-start)

Clone the repo
```
git clone https://github.com/mvakula/ping
cd ping
```

Install npm deps
```
npm i
```

Install [PureScript](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) and [psc-package](https://psc-package.readthedocs.io/en/latest/)

Install psc deps
```
psc-package install
```

Set env vars:
```
export AWS_ACCESS_KEY_ID=
export AWS_SECRET_ACCESS_KEY=
export SERVER_URL=      #you get this after deploying the server
export CLIENT_BUCKET=   #you get this after creating S3 bucket
export SLACK_WEBHOOK=   #optional
export SLACK_CHANNEL=   #optional
export PG_USER=
export PG_PW=
export PG_ENDPOINT=     #you get this after deploying the server
export PG_DB=
export AUTH_USER=
export AUTH_PASSWORD=
```

## Deploy

First, you need to manually create S3 bucket and set CLIENT_BUCKET env variable. After this you can deploy the client.
```
npm run build
npm run deploy:server
npm run deploy:client
```
This will create aws postgres, lambdas, etc and deploy client to S3 bucket.

Then you need to initialize db with `scripts/init.sql`.

## Running locally

Install [Postgres](https://www.postgresql.org/) 🐘

Initialize db with `scripts/init.sql`.

Set env variables (see above).

Then run client and server.
```
pulp build
npm start
npm run api
```
