# AWS via Haskell by Richard Cook

This is the full project that accompanies my [AWS via Haskell][aws-via-haskell] series of blog posts.

Note that the code in this repository is more actively maintained that the code originally posted on the blog and so will occasionally differ, especially as I consolidate code shared between the different examples.

## Setup

### Clone repository

```
git clone https://github.com/rcook/aws-via-haskell.git
```

### Install compiler

```
stack setup
```

### Build

```
stack build --fast
```

## Sections

### Part 1: DynamoDB

* [See blog post][dynamodb-post]
* [See code][dynamodb-code]

#### Set up local DynamoDB

* Follow [these instructions][local-dynamodb] to install and start DynamoDB on your local machine
* Alternatively, you can run these samples against a DynamoDB instance running on AWS by first connecting to it using the [AWS CLI][aws-cli] to set up your local credential cache

#### Run application

```
stack exec dynamodb-app
```

### Part 2: S3

* [See blog post][s3-post]
* [See code][s3-code]

#### Set up local S3

[localstack][localstack] can do this but has [some problems][bug].

#### Run application

```
stack exec s3-app
```

### Part 3: SQS

* [See blog post][sqs-post]
* [See code][sqs-code]

#### Set up local SQS

[localstack][localstack] can do this for you

#### Run application

```
stack exec sqs-app
```

### Part 4: SimpleDB

* [See blog post][sdb-post] (not posted yet)
* [See code][sdb-code]

#### Set up local SimpleDB

[localstack][localstack] can do this for you

#### Run application

```
stack exec sdb-app
```

## Licence

Released under [MIT License][licence]

[aws-cli]: https://aws.amazon.com/cli/
[aws-via-haskell]: http://blog.rcook.org/blog/2017/aws-via-haskell/
[bug]: https://github.com/brendanhay/amazonka/issues/432
[dynamodb-code]: dynamodb
[dynamodb-post]: http://blog.rcook.org/blog/2017/aws-via-haskell/
[licence]: LICENSE
[local-dynamodb]: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html
[localstack]: https://github.com/localstack/localstack
[s3-code]: s3
[s3-post]: http://blog.rcook.org/blog/2017/aws-via-haskell-s3/
[sdb-code]: sdb
[sdb-post]: http://blog.rcook.org/blog/2017/aws-via-haskell-sdb/
[sqs-code]: sqs
[sqs-post]: http://blog.rcook.org/blog/2017/aws-via-haskell-sqs/
