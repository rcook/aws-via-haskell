# dynamodb-demo by Richard Cook

Haskell application with main executable only built using Stack

## Set up local DynamoDB

* Follow [these instructions][local-dynamodb] to install and start DynamoDB on your local machine
* Alternatively, you can run these samples against a DynamoDB instance running on AWS by first connecting to it using the [AWS CLI][aws-cli] to set up your local credential cache

## Clone repository

```
git clone https://github.com/rcook/dynamodb-demo.git
```

## Install compiler

```
stack setup
```

## Build

```
stack build --fast
```

## Run application

```
stack exec dynamodb-demo-app
```

## Run tests

```
stack test
```

## Licence

Released under [MIT License][licence]

[aws-cli]: https://aws.amazon.com/cli/
[licence]: LICENSE
[local-dynamodb]: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html
