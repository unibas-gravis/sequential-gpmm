# README #


## What is this repository for? ##

SSM shape fitting to targets with pathologies using sequential GPMM. Based on the MICCAI21 publication: Sequential Gaussian Process Regression for Simultaneous Pathology Detection and Shape Reconstruction.

**Data:**  

The files and results for the synthetic and forensic experiments are provided in this repository. The forensic example mandible was provided by the Institute of Forensic Medicine, Zurich. 

The challenge datasets can be downloaded from the official challenge repositories (AutoImplant: https://autoimplant.grand-challenge.org/ and KITS: https://kits19.grand-challenge.org/) under Commons Attribution-NonCommercial-ShareAlike 4.0 International Public License  (https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode). The target meshes, ground truth meshes, and label maps must then be extracted (for example, using Slicer). For each dataset, we provide the necessary landmarks, SSM, and an example result. The data is still licensed as CC BY-NC-SA.

**Implementation of other fitting methods:**

You can find the CPD and RNICP code in the following repository: https://github.com/madsendennis/template-registration-with-scala . Save the fitted meshes in outputDirNRICP and outputDirCPD under mesh<name>/fit.vtk, then generate the label maps by running application.syntheticDataset.<cpd,icp>LabelMapGeneration. You will then be able to run the synthetic data evaluation script in the sequentialGPMM repository. We provide the output variance estimation of the CPD approach in the sigma2s variable as well as the fitted meshes.

**Changes from initial MICCAI results**

* The CPD and RNICP label maps are generated using getCorrespondence and getIcpLabelMapData in TemplateRegistrationEvaluationHelpers. The script was copied from the original repository  https://github.com/madsendennis/template-registration-with-scala. In the initial submission, the label maps were directly generated in the original repository.
* The kidney model in the data folder has been updated to exclude translation from the shape parameters. The model that was used in the initial submission along with the generated fits and label maps can be found in the results folder. 
* The KITS and AutoImplant evaluation scripts now compare the ground truth pathology (overgrowth, implant) shape to the reconstructed one. The initial evaluation script compared the reconstruction to the provided pathological shape instead.