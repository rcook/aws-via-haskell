# dynamodb-demo by Richard Cook

[AWS via Haskell (part 1): DynamoDB][blog-post]

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
[blog-post]: http://blog.rcook.org/blog/2017/aws-via-haskell/
[licence]: LICENSE
[local-dynamodb]: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html
