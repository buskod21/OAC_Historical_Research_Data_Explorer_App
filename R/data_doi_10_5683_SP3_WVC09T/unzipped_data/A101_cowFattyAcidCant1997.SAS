data fa;
	infile 'fofa.dat';
	input cow per trt oil mon C6 C8 C10 C12 C14 C16 C161
		C18 C181 C182 C183 C204 C205 C22 C226 fatyld;
C6yld=C6/100*fatyld;
C8yld=C8/100*fatyld;
C10yld=C10/100*fatyld;
C12yld=C12/100*fatyld;
C14yld=C14/100*fatyld;
C16yld=C16/100*fatyld;
C161yld=C161/100*fatyld;
C18yld=C18/100*fatyld;
C181yld=C181/100*fatyld;
C182yld=C182/100*fatyld;
C183yld=C183/100*fatyld;
C204yld=C204/100*fatyld;
C205yld=C205/100*fatyld;
C22yld=C22/100*fatyld;
C226yld=C226/100*fatyld;
n3ratio=(C183+C205+C226)/(C182+C204);
satratio=(C161+C181+C182+C183+C204+C205+C226)/(C6+C8+C10+C12+C14+C16+C18+C22);
proc print;
proc glm;
	class cow per trt;
	model C6 C8 C10 C12 C14 C16 C161 C18 C181 C182 C183
		C204 C205 C22 C226 C6yld C8yld C10yld C12yld
		C14yld C16yld C161yld C18yld C181yld C182yld C183yld
		C204yld C205yld C22yld C226yld n3ratio satratio
		= cow per trt / SS2 SS3;
	means trt / duncan;
	lsmeans trt / pdiff;
proc glm;
	class cow per oil mon;
	model C6 C8 C10 C12 C14 C16 C161 C18 C181 C182 C183
		C204 C205 C22 C226 C6yld C8yld C10yld C12yld
		C14yld C16yld C161yld C18yld C181yld C182yld C183yld
		C204yld C205yld C22yld C226yld n3ratio satratio
		= cow per oil mon oil*mon / SS2 SS3;
run;
