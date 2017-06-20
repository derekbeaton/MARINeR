FSLOUTPUTTYPE=NIFTI_GZ
basedir='/3volatile/grady_lab/jrieck/fmri/mariner'
subj=$1
subjdir=$basedir/MARINeR_Data/$subj
anatdir=$subjdir/anat
funcdir=$subjdir/func

echo "Skull Stripping T1"
bet $anatdir/${subj}_T1w $anatdir/${subj}_T1w_bet -m -R

echo "Registering T1 to standard space - FLIRT"
stdbrain=$basedir/templates/MNI152_T1_4mm_brain
flirt -in $anatdir/${subj}_T1w_bet -ref $stdbrain -omat $anatdir/anat2standard.mat -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof 12 -cost corratio -out $anatdir/${subj}_T1w_bet_MNI

echo "Inverting anat to standard transformation matrix"
convert_xfm -omat $anatdir/standard2anat.mat -inverse $anatdir/anat2standard.mat

echo "Registering functional to T1 - FLIRT"
flirt -in $funcdir/${subj}_task-onebacktask_run-01_bold -ref $anatdir/${subj}_T1w_bet -omat $anatdir/func12anat.mat -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof 6 -cost corratio -out $funcdir/${subj}_task-onebacktask_run-01_bold_ranat

flirt -in $funcdir/${subj}_task-onebacktask_run-02_bold -ref $anatdir/${subj}_T1w_bet -omat $anatdir/func22anat.mat -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof 6 -cost corratio -out $funcdir/${subj}_task-onebacktask_run-02_bold_ranat

convert_xfm -omat $anatdir/anat2func1.mat -inverse $anatdir/func12anat.mat
convert_xfm -omat $anatdir/anat2func2.mat -inverse $anatdir/func22anat.mat

echo "Regstering functionals to standard space - FLIRT"
flirt -in $funcdir/${subj}_task-onebacktask_run-01_bold_ranat -ref $stdbrain -applyxfm -init $anatdir/anat2standard.mat -out $funcdir/${subj}_task-onebacktask_run-01_bold_ranat_MNI

flirt -in $funcdir/${subj}_task-onebacktask_run-02_bold_ranat -ref $stdbrain -applyxfm -init $anatdir/anat2standard.mat -out $funcdir/${subj}_task-onebacktask_run-02_bold_ranat_MNI


