package application.syntheticDataset

import java.io.File

import application.DataDirectory

object SyntheticData {

  val syntheticDir = new File(DataDirectory.dir, "synthetic")

  val modelFile: File = new File(syntheticDir, "augmentedModel100.h5")

  val reflmFile: File = new File(syntheticDir, "skull-19.json")

  val innerreflmFile: File = new File(syntheticDir, "skull-19_2(inner).json")

  val groundTruthMeshFile = new File(syntheticDir, "shapePoseMeshToClip.stl")

  val groundTruthMaps = new File(syntheticDir, "gtMaps")

  private val rawnames = Seq("correctedField-removed205-", "correctedField-removed102-", "correctedField-removed411-", "correctedField-removed686-")

  lazy val namesToTest: Seq[String] = rawnames.flatMap { rawname =>
    (0 until 10).map { i =>
      rawname + i.toString
    }
  }

  lazy val allNames: Seq[String] = namesToTest

  val inputLocation = new File(syntheticDir, "mesh")
  val landmarkLocation = new File(syntheticDir, "landmarks")
  val outputDirSeqGPMM = new File(syntheticDir, "outputSeqGPMM")
  val outputDirRANSAC = new File(syntheticDir, "outputRANSAC")
  val outputDirNRICP = new File(syntheticDir, "outputNRICP")
  val outputDirCPD = new File(syntheticDir, "outputCPD")

}
