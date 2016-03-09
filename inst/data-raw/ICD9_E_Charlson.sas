%*************************************************************************;
%*  Date: February 2006                                                  *;
%*                                                                       *;
%*  This code is presented on the MCHP web site with permission from     *;
%*  Hude Quan.  The code was used in a study titled, "Coding algorithms  *;
%*  for defining comorbidities in ICD-9-CM and ICD-10 administrative     *;
%*  data." in Med Care, 43(11):1130-9.                                   *;
%*                                                                       *;
%*  NOTE:  This code has not been validated by MCHP.                     *;
%*************************************************************************;
libname comorbid 'D:\data\icd9';
%macro ICD9_E_CH(library=, dataset=, OUTPUT=);
data &library..&OUTPUT;
	set &library..&dataset (keep=dx_1-dx_16 EXIT_ALIVE cdr_key);

	/*Inhos_mort: dependent variable for Logistic regression*/
	if exit_alive = 'D' or exit_alive = 'S' then inhos_mort = 0;else inhos_mort = 1; 
	/*DC: Disease Codes*/
	%LET DC1=%STR('410','412');
	%LET DC2=%STR('39891','40201','40211','40291','40401','40403','40411','40413','40491','40493','4254','4255','4257','4258','4259','428');
	%LET DC3=%STR('0930','4373','440','441','4431','4432','4438','4439','4471','5571','5579','V434');
	%LET DC4=%STR('36234','430','431','432','433','434','435','436','437','438');
	%LET DC5=%STR('290','2941','3312');          
	%LET DC6=%STR('4168','4169','490','491','492','493','494','495','496','500','501','502','503','504','505','5064','5081','5088');
	%LET DC7=%STR('4465','7100','7101','7102','7103','7104','7140','7141','7142','7148','725');
	%LET DC8=%STR('531','532','533','534');      
	%LET DC9=%STR('07022','07023','07032','07033','07044','07054','0706','0709','570','571','5733','5734','5738','5739','V427');
	%LET DC10=%STR('2500','2501','2502','2503','2508','2509');
	%LET DC11=%STR('2504','2505','2506','2507');  
	%LET DC12=%STR('3341','342','343','3440','3441','3442','3443','3444','3445','3446','3449');
	%LET DC13=%STR('40301','40311','40391','40402','40403','40412','40413','40492','40493','582','5830','5831','5832','5834','5836','5837','585','586','5880','V420','V451','V56');
	%LET DC14=%STR('140','141','142','143','144','145','146','147','148','149','150','151','152','153','154','155','156','157','158','159','160','161','162','163','164','165','170','171','172','174','175','176','179','180','181','182','183','184','185','186','187','188','189','190','191','192','193','194','195','200','201','202','203','204','205','206','207','208','2386');
	%LET DC15=%STR('4560','4561','4562','5722','5723','5724','5728');
	%LET DC16=%STR('196','197','198','199');      
	%LET DC17=%STR('042','043','044');
	/**Myocardial Infarction**/
	%LET DIS1=MI;
	%LET LBL1=%STR(Myocardial Infarction);

	/**Congestive Heart Failure**/
	%LET DIS2=CHF;
	%LET LBL2=%STR(Congestive Heart Failure);

	/**Periphral Vascular Disease**/
	%LET DIS3=PVD;
	%LET LBL3=%STR(Periphral Vascular Disease);

	/**Cerebrovascular Disease**/
	%LET DIS4=CEVD;
	%LET LBL4=%STR(Cerebrovascular Disease);

	/**Dementia**/
	%LET DIS5=DEM;
	%LET LBL5=%STR(Dementia);

	/*Chronic Pulmonary Disease*/
	%LET DIS6=COPD;
	%LET LBL6=%STR(Chronic Pulmonary Disease);

	/**Connective Tissue Disease-Rheumatic Disease**/
	%LET DIS7=Rheum;
	%LET LBL7=%STR(Connective Tissue Disease-Rheumatic Disease);

	/**Peptic Ulcer Disease**/   
	%LET DIS8=PUD;
	%LET LBL8=%STR(Peptic Ulcer Disease);

	/**Mild Liver Disease **/
	%LET DIS9=MILDLD;
	%LET LBL9=%STR(Mild Liver Disease);

	/**Diabetes without complications**/
	%LET DIS10=DIAB_NC;
	%LET LBL10=%STR(Diabetes without complications);

	/**Diabetes with complications**/
	%LET DIS11=DIAB_C;
	%LET LBL11=%STR(Diabetes with complications);

	/**Paraplegia and Hemiplegia**/
	%LET DIS12=PARA;
	%LET LBL12=%STR(Paraplegia and Hemiplegia);

	/**Renal Disease**/
	%LET DIS13=RD;
	%LET LBL13=%STR(Renal Disease);

	/**Cancer**/
	%LET DIS14=CANCER;
	%LET LBL14=%STR(Cancer);

	/**Moderate or Severe Liver Disease**/
	%LET DIS15=MSLD;
	%LET LBL15=%STR(Moderate or Severe Liver Disease);

	/**Metastatic Carcinoma **/
	%LET DIS16=METS;
	%LET LBL16=%STR(Metastatic Carcinoma);

	/**AIDS/HIV**/
	%LET DIS17=HIV;
	%LET LBL17=%STR(AIDS/HIV);

	%do DI=1 %to 17;/*ICD9-E Charlson: 17 groups*/
		A&DI=0; 						
		%do DX=1 %to 16; 	/*DX_1 - DX_16*/
			B&DX=0;
			%do SN=3 %to 5;
				if substr(dx_&DX,1,&SN) in (&&DC&DI) then C&SN=1;else C&SN=0;
				B&DX=B&DX +C&SN;
				drop C&SN;
			%end;
			A&DI=A&DI+B&DX;
			DROP B&DX;
		%end;
		if A&DI>0 then 	ICD9_E_CH_&&DIS&DI=1;else ICD9_E_CH_&&DIS&DI=0;
		label ICD9_E_CH_&&DIS&DI = &&LBL&DI;
		DROP A&DI;	
	%end;
		
run;
%mend ICD9_E_CH;
%ICD9_E_CH(library=comorbid, dataset=icd9CM3, OUTPUT=ICD9_E_CH);


/*Output:  Frequencies and Logistic regression*/
ods listing close;
ods rtf file='d:\projects\hude\icd9\ICD9_E_CH.RTF';
PROC FREQ DATA=comorbid.ICD9_E_CH;
	TABLE ICD9_E_CH_MI--ICD9_E_CH_HIV / NOCUM;RUN;
proc logistic data=comorbid.ICD9_E_CH DESCENDING;
	model inhos_mort = ICD9_E_CH_MI--ICD9_E_CH_METS; /*EXCLUDING HIV*/
run;
ODS RTF CLOSE;
ODS LISTING;