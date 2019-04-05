package dynamodbx.common

trait Builder[A, B] extends (A => B)
