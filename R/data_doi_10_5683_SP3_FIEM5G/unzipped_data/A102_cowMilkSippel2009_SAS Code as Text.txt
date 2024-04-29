options linesize=80;
data milk;
	infile 'cla.dat';
	input cow sq per trt milk fat prot lac dmi bw;
if sq=1 then delete;
if trt=1 then trt=3;
if trt=4 then trt=1;
if trt=6 then trt=4;
if trt=5 then trt=2;
if trt=7 then trt=5;
if cow=2664 and per=5 then delete;

protfat=prot/fat;
fatp=fat/milk/10;
protp=prot/milk/10;
lacp=lac/milk/10;
NELI=1.65*DMI;
NELmaint=0.08*BW**0.75;
NELmilk=2.2*milk*(41.63*fatp+24.13*protp+21.60*lacp-11.72)/1000;
NELbal=NELI-NELmaint-NELmilk;

proc print;
proc glm;
	class sq cow trt per;
	model dmi bw milk fat prot lac protfat fatp protp lacp NELbal
		= sq cow(sq) per trt;
	test h=sq e=cow(sq);
	
	means trt /duncan;
	means sq /duncan e=cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;

proc mixed data=milk;
	class sq cow trt per;
	model dmi = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model bw = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model milk = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model fat = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model prot = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model lac = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model protfat = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model fatp = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model protp = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model lacp = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;
proc mixed data=milk;
	class sq cow trt per;
	model NELbal = sq per trt;
	random cow(sq);
	contrast 'linear'  trt -62.4 -40.4 -17.4 4.6 115.6;
	contrast 'quadratic' trt 0.611465 0.0570025 -0.366328
	    -0.62172 0.319584;

run;
