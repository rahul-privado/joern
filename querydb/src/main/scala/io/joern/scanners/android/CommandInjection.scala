package io.joern.scanners.android

import io.joern.scanners._
import io.joern.console._
import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.macros.QueryMacros._
import io.shiftleft.semanticcpg.language._

object CommandInjection extends QueryBundle {
  implicit val engineContext: EngineContext = EngineContext(Semantics.empty)
  implicit val resolver: ICallResolver      = NoResolve

  @q
  def uiInputToExec(): Query =
    Query.make(
      name = "ui-input-to-exec",
      author = Crew.claudiu,
      title = "Command injection",
      description = "-",
      score = 4,
      withStrRep({ cpg =>
        cpg.method
          .nameExact("findViewById")
          .callIn
          .code(".*TextView.*")
          .filter { c =>
            def execCalls = cpg.method.nameExact("exec").callIn
            def sink      = execCalls.argument
            sink.reachableByFlows(c).nonEmpty
          }
          .l
      }),
      tags = List(QueryTags.android),
      codeExamples = CodeExamples(
        List("""
            |package com.example.slimandroid
            |
            |import android.os.Bundle
            |import android.widget.Button
            |import android.widget.TextView
            |import androidx.appcompat.app.AppCompatActivity
            |
            |class MainActivity : AppCompatActivity() {
            |    override fun onCreate(savedInstanceState: Bundle?) {
            |        super.onCreate(savedInstanceState)
            |        setContentView(R.layout.activity_main)
            |
            |        val sourceInput = findViewById<TextView>(R.id.sourceText)
            |        val result = findViewById<TextView>(R.id.outputText)
            |        val doButton = findViewById<Button>(R.id.doButton)
            |        doButton.setOnClickListener { e ->
            |            try {
            |                val p = Runtime.getRuntime().exec(sourceInput.text.toString() + "# matching")
            |                p.waitFor()
            |                result.text = p.inputStream.bufferedReader().readText()
            |            } catch(e:Exception) {
            |                result.text = "ERROR " + e.localizedMessage
            |                e.printStackTrace()
            |            }
            |        }
            |    }
            |}
            |""".stripMargin),
        List("""
            |package com.example.slimandroid
            |
            |import android.os.Bundle
            |import android.widget.Button
            |import android.widget.TextView
            |import androidx.appcompat.app.AppCompatActivity
            |
            |class MainActivity : AppCompatActivity() {
            |    override fun onCreate(savedInstanceState: Bundle?) {
            |        super.onCreate(savedInstanceState)
            |        setContentView(R.layout.activity_main)
            |
            |        val sourceInput = findViewById<TextView>(R.id.sourceText)
            |        val result = findViewById<TextView>(R.id.outputText)
            |        val doButton = findViewById<Button>(R.id.doButton)
            |        doButton.setOnClickListener { e ->
            |            try {
            |                val p = Runtime.getRuntime().exec("ls notmatching.txt")
            |                p.waitFor()
            |                result.text = p.inputStream.bufferedReader().readText()
            |            } catch(e:Exception) {
            |                result.text = "ERROR " + e.localizedMessage
            |                e.printStackTrace()
            |            }
            |        }
            |    }
            |}
            |""".stripMargin)
      )
    )
}
