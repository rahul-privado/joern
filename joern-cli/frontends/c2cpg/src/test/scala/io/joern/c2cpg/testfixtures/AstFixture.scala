package io.joern.c2cpg.testfixtures

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.Cpg

object AstFixture {
  def apply(code: String, fileName: String = "file.c")(f: Cpg => Unit): Unit = {
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val cpg  = newEmptyCpg()
      val file = dir / fileName
      file.write(code)
      val config = Config(inputPath = dir.toString, includePathsAutoDiscovery = false, outputPath = dir.toString())
      new AstCreationPass(cpg, AstCreationPass.SourceFiles, config).createAndApply()
      f(cpg)
      file.delete()
    }
  }

  def createCpg(code: String): Cpg = {
    val cpg = newEmptyCpg()
    File.usingTemporaryDirectory("c2cpgtest") { dir =>
      val file = dir / "file.c"
      file.write(code)
      val config = Config(inputPath = dir.toString, includePathsAutoDiscovery = false, outputPath = dir.toString())
      new AstCreationPass(cpg, AstCreationPass.SourceFiles, config).createAndApply()
    }
    cpg
  }
}

trait AstFixture