package scalapb.compiler

import java.io.{File, FileInputStream, PrintWriter}
import java.nio.file.Files

import scala.collection.JavaConverters._
import com.google.protobuf.DescriptorProtos.FileDescriptorSet
import com.google.protobuf.Descriptors.FileDescriptor
import org.scalatest.{FlatSpec, MustMatchers}

class ProtoValidationSpec extends FlatSpec with MustMatchers {
  def generateFileSet(files: Seq[(String, String)]) = {
    val tmpDir = Files.createTempDirectory("validation").toFile
    val fileNames = files.map { case (name, content) =>
      val file = new File(tmpDir, name)
      val pw   = new PrintWriter(file)
      pw.write(content)
      pw.close()
      file.getAbsoluteFile
    }
    val outFile = new File(tmpDir, "descriptor.out")

    require(com.github.os72.protocjar.Protoc.runProtoc(Array(
      "-I", tmpDir.toString,
      s"--descriptor_set_out=${outFile.toString}",
      "--include_imports"
    ) ++ fileNames.map(_.toString)) == 0, "protoc exited with an error")

    val fileset: Seq[FileDescriptor] = {
      val fin = new FileInputStream(outFile)
      val fileset = try {
        FileDescriptorSet.parseFrom(fin)
      } finally {
        fin.close()
      }
      fileset.getFileList.asScala.foldLeft[Map[String, FileDescriptor]](Map.empty) {
        case (acc, fp) =>
          val deps = fp.getDependencyList.asScala.map(acc)
          acc + (fp.getName -> FileDescriptor.buildFrom(fp, deps.toArray))
      }.values.toVector
    }
    fileset
  }

  def runValidation(files: (String, String)*): Unit = {
    val fileset = generateFileSet(files)
    val params = new GeneratorParams()
    val validation = new ProtoValidation(new DescriptorImplicits(params, fileset))
    fileset.foreach(validation.validateFile)
  }

  "simple message" should "validate" in {
    val r = runValidation(
      "file.proto" ->
        """
      |syntax = "proto2";
      |message Foo { };
    """.stripMargin)
  }

  "UNRECOGNIZED enum value" should "fail validation" in {
    intercept[GeneratorException] {
      runValidation(
        "file.proto" ->
          """
            |syntax = "proto2";
            |enum MyEnum { UNRECOGNIZED = 3; };
          """.stripMargin)
    }.message must include("The enum value 'UNRECOGNIZED' in MyEnum is not allowed due to conflict with the catch-all Unrecognized")
  }

}
