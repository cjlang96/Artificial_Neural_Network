ANN_OBJS = param_def.f90 errors.f90 param_func.f90 Find_Weights.f90 Input_Format.f90 Output_Format.f90 Artificial_Neural_Network.f90

TANN_OBJS = Testing_ANN.f90

ANN: $(ANN_OBJS)
	gfortran -o $@ ${ANN_OBJS} 
	rm *.mod

TANN: $(TANN_OBJS)
	gfortran -o $@ ${TANN_OBJS}

AGE:
	cd Examples/AND_Gate/ ;\
	make AGE

MNISTF:
	cd Examples/Image_Recognition/ ;\
	make MNISTF

clean:
	rm ANN TANN
	cd Examples/AND_Gate/ ;\
	make clean
	cd Examples/Image_Recognition/ ;\
	make clean