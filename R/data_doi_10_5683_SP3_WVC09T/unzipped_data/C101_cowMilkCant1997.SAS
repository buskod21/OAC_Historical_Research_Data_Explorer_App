data milk;
	infile 'fomilk.prn';
	input cow per trt oil mon day milkkg fatp protp lactp casN totN;
Nratio=casN/totN;
fatyld=fatp/100*milkkg*1000;
protyld=protp/100*milkkg*1000;
lactyld=lactp/100*milkkg*1000;
Eyld=fatyld*9.4+protyld*5.75+lactyld*3.9;
proc print;
proc glm;
	class cow per trt;
	model milkkg fatp protp lactp casN totN Nratio fatyld protyld
		lactyld Eyld = cow per trt cow*per*trt / SS2 SS3;
	test h=cow per trt e=cow*per*trt;
	means trt / duncan e=cow*per*trt;
	lsmeans trt / pdiff e=cow*per*trt;
proc glm;
	class cow per oil mon;
	model milkkg fatp protp lactp casN totN Nratio fatyld protyld
		lactyld Eyld = cow per oil mon oil*mon cow*per*oil*mon
		/ SS2 SS3;
	test h=cow per oil mon oil*mon e=cow*per*oil*mon;
run;
