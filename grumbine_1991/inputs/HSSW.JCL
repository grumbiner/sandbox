# QSUB -o a14out
# QSUB -lT 29
# QSUB -lM   1MW
# QSUB -me

cd $HOME/hssw

#execute the model
time hssw < a14 
