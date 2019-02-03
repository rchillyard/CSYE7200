package edu.neu.coe.csye7200.lbawscld

import org.scalatest.{FlatSpec, Matchers}
import spray.json._

class S3BucketSpec extends FlatSpec with Matchers {

  it should "generate s3 buckect json" in {

    S3Bucket.simpleTemplate.toJson.toString() shouldBe "{\"AWSTemplateFormatVersion\":\"2010-09-09\",\"Description\":\"Simple S3 Bucket template\",\"Parameters\":{\"DomainName\":{\"Type\":\"String\"}},\"Resources\":{\"testS3Bucket\":{\"Properties\":{\"BucketName\":{\"Fn::Join\":[\".\",[\"csye7200\",{\"Ref\":\"DomainName\"},\"lab\"]]}},\"Type\":\"AWS::S3::Bucket\"}}}"
  }

}
