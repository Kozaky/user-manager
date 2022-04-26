# README

The project can be build and run with [stack](http://haskellstack.org/), e.g.:

```shell
stack build
stack exec user-manager
```

Some examples of queries to the API:

```shell
curl -H 'Content-type: application/json' localhost:3000/users --data '{"name": "Dana", "email":"hola@hola.com", "password": "password"}'

curl -H 'Content-type: application/json' localhost:3000/users/620949efd5cd0a08b2000000

curl -H 'Content-type: application/json' -X PUT localhost:3000/users/620949efd5cd0a08b2000000 --data '{"email": "gocam.julio@gmail.com" }'
```

## HOW TO RUN TESTS

First of all, we need to create the following docker container:
```shell
docker run -d -p 27017:27017 \
  --name holajobs-users-test \
  -e MONGO_INITDB_ROOT_USERNAME=root \
  -e MONGO_INITDB_ROOT_PASSWORD=root \
  --mount type=bind,source=/Users/juliogc/Documents/dev/holajobs-config/mongo/mongo-entrypoints-test,destination=/docker-entrypoint-initdb.d/,readonly \
  --mount type=tmpfs,destination=/data/db \
  mongo \
```

Take into account that we need a valid source with the initial scripts for a mongoDB

After this, we will be able to run the tests as:
```shell
docker start /holajobs-users-test \
&& sleep 5 \
&& ENV=test stack test \
; docker stop /holajobs-users-test
```

The sleep command is necessary to give docker enough time to initialize the containers before we launch the tests