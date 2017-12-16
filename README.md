# AWS via Haskell by Richard Cook

A series of [blog posts][blog] by Me

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

* Blog post is work in pgress
* [See code][s3-code]

#### Set up local S3

There appears to be no way to do this unfortunately.

#### Run application

```
stack exec s3-app
```

## Licence

Released under [MIT License][licence]

[aws-cli]: https://aws.amazon.com/cli/
[blog]: http://blog.rcook.org/
[dynamodb-code]: dynamodb
[dynamodb-post]: http://blog.rcook.org/blog/2017/aws-via-haskell/
[licence]: LICENSE
[local-dynamodb]: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html
[s3-code]: s3
