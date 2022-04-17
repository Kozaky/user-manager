This project is a small example for how to set up a web-server with
[servant-server](http://haskell-servant.readthedocs.io/) that uses
[persistent](https://www.stackage.org/package/persistent) for saving data to a
database.

You can build and run the project with [stack](http://haskellstack.org/), e.g.:

```shell
stack build
stack exec user-manager
```

Then you can query the server from a separate shell:

```shell
curl -H 'Content-type: application/json' localhost:3000/users --data '{"name": "Dana", "email":"hola@hola.com", "password": "password"}'

curl -H 'Content-type: application/json' localhost:3000/users/620949efd5cd0a08b2000000

curl -H 'Content-type: application/json' -X PUT localhost:3000/users/620949efd5cd0a08b2000000 --data '{"email": "gocam.julio@gmail.com" }'
```
