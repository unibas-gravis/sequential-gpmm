package application.KITS

import application.DataDirectory

import java.io.File

object KitsData {

  val kitsDir = new File(DataDirectory.dir, "kits")

  val targetFile: File = new File(kitsDir, "mesh-initial")

  val modelFile = new File(kitsDir, "cleaned-withgpa.h5")

  val targetlmFile = new File(kitsDir, "landmarks-initial")

  val landmarksOnModelReferenceFile: File = new File(kitsDir, "case_00000-combined-5.json")

  val outputDirSeqGPMMkits = new File(kitsDir, "outputSeqGPMM")

  val gtmaplocation: File = new File (kitsDir, "mesh-labelmap")

//  The challenge data was taken from the official repository KITS2019 under the data license
//   (https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode)
//  a. Attribution
//      Retained information:
//          - identification of the creator(s) of the Licensed Material and any others designated to receive attribution:
//          KITS 2019 including Nicholas Heller, Dr. Christopher Weight, Dr. Niranjan Sathianathen
//          and the following medical students
//              Heather Kaluzniak
//              Keenan Moore
//              Joel Rosenberg
//              Paul Blake
//              Makinna Oestreich
//              Ed Walczak
//              Zach Rengel
//              Shane Raza
//              Zach Edgerton
//              Michael Tradewell
//              Aneri Shah
//          - The data description manuscript can be found at https://arxiv.org/abs/1904.00445
//          - Copyright notice: (not provided in KITS2019)
//          - Notice that refers to source Public License:
//          All data is licensed as CC BY-NC-SA.
//          - Notice that refers to the disclaimer of warranties: (not provided in KITS2019)
//          - URI or hyperlink to the Licensed Material: https://kits19.grand-challenge.org/data/
//      the Licensed material was modified; and
//      the Licensed Material is licensed under the
//      Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International Public License
//      https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode
//
//  b.ShareAlike
//       1. All data is licensed as CC BY-NC-SA.
//       2. https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode
//       3. No additional or different terms or conditions apply on the Adapted Material.


}
