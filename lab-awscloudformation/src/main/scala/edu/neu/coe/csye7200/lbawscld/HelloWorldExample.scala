package edu.neu.coe.csye7200.lbawscld

import com.monsanto.arch.cloudformation.model._
import com.monsanto.arch.cloudformation.model.resource._

object HelloWorldExample extends VPCWriter with App {

  val mySimpleVPC = `AWS::EC2::VPC`("mySimpleVPCExample",CidrBlock(10, 0, 0, 0, 16),AmazonTag.fromName("mySimpleVPCExample"))

  val simpleTemplate = Template(
      AWSTemplateFormatVersion = "2010-09-09",
      Description = "Simple VPC template",
      Parameters = None,
      Conditions = None,
      Mappings = None,
      Resources = Seq(mySimpleVPC),
      Outputs = None,
      Routables = None
    )
  writeStaxModule("HelloWorld.json", simpleTemplate)

}
