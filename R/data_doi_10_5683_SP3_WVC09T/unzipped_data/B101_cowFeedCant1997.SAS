data milk;
	infile 'fofeed.prn';
	input cow per trt oil mon day concDMI silDMI forDMI CPI NDFI EEI MEI;
hayDMI=forDMI-silDMI;
totalDMI=concDMI+forDMI;
FCratio=forDMI/concDMI;
if oil=0 then do;
	CPI2=concDMI*.195+silDMI*.119+hayDMI*.138;
	NDFI2=concDMI*.133+silDMI*.646+hayDMI*.540;
	EEI2=concDMI*.0133+silDMI*.0158+hayDMI*.0191;
	MEI2=concDMI*3.12+silDMI*2.29+hayDMI*1.87;
end;
if oil=2 then do;
	CPI2=concDMI*.199+silDMI*.119+hayDMI*.138;
	NDFI2=concDMI*.132+silDMI*.646+hayDMI*.540;
	EEI2=concDMI*.0519+silDMI*.0158+hayDMI*.0191;
	MEI2=concDMI*3.34+silDMI*2.29+hayDMI*1.87;
end;
proc print;
proc glm;
	class cow per trt;
	model concDMI silDMI hayDMI totalDMI FCratio CPI NDFI EEI MEI
		 CPI2 NDFI2 EEI2 MEI2 = cow per trt cow*per*trt / SS2 SS3;
	test h=cow per trt e=cow*per*trt;
	means trt / duncan e=cow*per*trt;
	lsmeans trt / pdiff e=cow*per*trt;
proc glm;
	class cow per oil mon;
	model concDMI silDMI hayDMI totalDMI FCratio CPI NDFI EEI MEI
		 CPI2 NDFI2 EEI2 MEI2 = cow per oil mon oil*mon 
		cow*per*oil*mon	/ SS2 SS3;
	test h=cow per oil mon oil*mon e=cow*per*oil*mon;
run;
