/*Quan et al. :Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Med Care 2005 Nov; 43(11):1073-1077.*/
/*Enhanced ICD-9-CM Elixhauser*/
libname comorbid "D:\data\icd9";
%macro ICD9_E_EX (lib_in=, lib_out=, data_in=, data_out=);
data &lib_out..&data_out;
	set &lib_in..&data_in;

	/*Congestive Heart Failure*/
	%LET DC1=%STR('39891','40201','40211','40291','40401','40403','40411','40413','40491','40493','4254','4255','4257','4258','4259','428');
	%LET DIS1=CHF;		
	%LET LBL1=%STR(Congestive Heart Failure);

	/*Caridiac Arrhythmia*/
	%LET DC2=%STR('4260','42613','4267','4269','42610','42612','4270','4271','4272','4273','4274','4276','4278','4279','7850','99601','99604','V450','V533');
	%LET DIS2=Arrhy;	
	%LET LBL2=%STR(Caridiac Arrhythmia);

	/*Valvular Disease*/
	%LET DC3=%STR('0932','394','395','396','397','424','7463','7464','7465','7466','V422','V433');
	%LET DIS3=VD;		
	%LET LBL3=%STR(Valvular Disease);

	/*Pulmonary Circulation Disorders*/
	%LET DC4=%STR('4150','4151','416','4170','4178','4179');
	%LET DIS4=PCD;		
	%LET LBL4=%STR(Pulmonary Circulation Disorders);

	/*Peripheral Vascular Disorders*/
	%LET DC5=%STR('0930','4373','440','441','4431','4432','4438','4439','4471','5571','5579','V434');
	%LET DIS5=PVD;		
	%LET LBL5=%STR(Peripheral Vascular Disorders);

	/*Hypertension Uncomlicated*/
	%LET DC6=%STR('401');
	%LET DIS6=HPTN_UC;	
	%LET LBL6=%STR(Hypertension Uncomlicated);

	/*Hypertension comlicated*/
	%LET DC7=%STR('402','403','404','405');
	%LET DIS7=HPTN_C;	
	%LET LBL7=%STR(Hypertension comlicated);

	/*Paralysis*/
	%LET DC8=%STR('3341','342','343','3440','3441','3442','3443','3444','3445','3446','3449');
	%LET DIS8=Para;		
	%LET LBL8=%STR(Paralysis);

	/* Other Neurological Disorders*/
	%LET DC9=%STR('3319','3320','3321','3334','3335','33392','334','335','3362','340','341','345','3481','3483','7803','7843');
	%LET DIS9=OthND;	
	%LET LBL9=%STR(Other Neurological Disorders);

	/*Chronic Pulmonary Disease*/
	%LET DC10=%STR('4168','4169','490','491','492','493','494','495','496','500','501','502','503','504','505','5064','5081','5088');
	%LET DIS10=COPD;	
	%LET LBL10=%STR(Chronic Pulmonary Disease);

	/*Diabetes Uncomplicated*/
	%LET DC11=%STR('2500','2501','2502','2503');
	%LET DIS11=Diab_UC;	
	%LET LBL11=%STR(Diabetes Uncomplicated);

	/*Diabetes Complicated*/
	%LET DC12=%STR('2504','2505','2506','2507','2508','2509');
	%LET DIS12=Diab_C;	
	%LET LBL12=%STR(Diabetes Complicated);

	/*Hypothyroidism*/
	%LET DC13=%STR('2409','243','244','2461','2468');
	%LET DIS13=Hptothy;	
	%LET LBL13=%STR(Hypothyroidism);

	/*Renal Failure*/
	%LET DC14=%STR('40301','40311','40391','40402','40403','40412','40413','40492','40493','585','586','5880','V420','V451','V56');
	%LET DIS14=RF;		
	%LET LBL14=%STR(Renal Failure);

	/*Liver Disease*/
	%LET DC15=%STR('07022','07023','07032','07033','07044','07054','0706','0709','4560','4561','4562','570','571','5722','5723','5724','5728','5733','5734','5738','5739','V427');
	%LET DIS15=LD;		
	%LET LBL15=%STR(Liver Disease);

	/*Peptic Ulcer Disease excluding bleeding*/
	%LET DC16=%STR('5317','5319','5327','5329','5337','5339','5347','5349');
	%LET DIS16=PUD_NB;	
	%LET LBL16=%STR(Peptic Ulcer Disease excluding bleeding);

	/*AIDS/HIV*/
	%LET DC17=%STR('042','043','044');
	%LET DIS17=HIV;
	%LET LBL17=%STR(AIDS/HIV);

	/*Lymphoma*/
	%LET DC18=%STR('200','201','202','2030','2386');
	%LET DIS18=Lymp;
	%LET LBL18=%STR(Lymphoma);

	/*Metastatic Cancer*/
	%LET DC19=%STR('196','197','198','199');
	%LET DIS19=METS;
	%LET LBL19=%STR(Metastatic Cancer);

	/*Solid Tumor without Metastasis*/
	%LET DC20=%STR('140','141','142','143','144','145','146','147','148','149','150','151','152','153','154','155','156','157','158','159','160','161','162','163','164','165','166','167','168','169','170','171','172','174','175','176','177','178','179','180','181','182','183','184','185','186','187','188','189','190' '191','192','193','194','195');
	%LET DIS20=Tumor;
	%LET LBL20=%STR(Solid Tumor without Metastasis);

	/*Rheumatoid Arthsitis/collagen*/
	%LET DC21=%STR('446','7010','7100','7101','7102','7103','7104','7108','7109','7112','714','7193','720','725','7285','72889','72930');
	%LET DIS21=Rheum_A;
	%LET LBL21=%STR(Rheumatoid Arthsitis/collagen);


	/*Coagulopathy*/
	%LET DC22=%STR('286','2871','2873','2874','2875');
	%LET DIS22=Coag;
	%LET LBL22=%STR(Coagulopathy);

	/*Obesity*/
	%LET DC23=%STR('2780');
	%LET DIS23=Obesity;
	%LET LBL23=%STR(Obesity);

	/*Weight Loss*/
	%LET DC24=%STR('260','261','262','263','7832','7994');
	%LET DIS24=WL;
	%LET LBL24=%STR(Weight Loss);

	/*Fluid and Ecletrolyte Disorders*/
	%LET DC25=%STR('2536','276');
	%LET DIS25=Fluid;
	%LET LBL25=%STR(Fluid and Ecletrolyte Disorders);

	/*Blood Loss Anemia*/
	%LET DC26=%STR('2800');
	%LET DIS26=BLA;
	%LET LBL26=%STR(Blood Loss Anemia);

	/*Deficiency Anemia*/
	%LET DC27=%STR('2801','2808','2809','281');
	%LET DIS27=DA;
	%LET LBL27=%STR(Deficiency Anemia);

	/*Alcohol Abuse*/
	%LET DC28=%STR('2652','2911','2912','2913','2915','2918','2919','3030','3039','3050','3575','4255','5353','5710','5711','5712','5713','980','V113');
	%LET DIS28=Alcohol;
	%LET LBL28=%STR(Alcohol Abuse);

	/*Drug Abuse*/
	%LET DC29=%STR('292','304','3052','3053','3054','3055','3056','3057','3058','3059','V6542');
	%LET DIS29=Drug;
	%LET LBL29=%STR(Drug Abuse);

	/*Psychoses*/
	%LET DC30=%STR('2938','295','29604','29614','29644','29654','297','298');
	%LET DIS30=Psycho;
	%LET LBL30=%STR(Psychoses);

	/*Depression*/
	%LET DC31=%STR('2962','2963','2965','3004','309','311');
	%LET DIS31=Dep;
	%LET LBL31=%STR(Depression);


	%do DI=1 %to 31;			/*ICD9 Elixhauser: 31 groups*/
		A1=0; 
		%do DX=1 %to 16; 		/*Diagnosis codes: DX_1-DX_16*/
			B1=0;
			%do SN=3 %to 5;
				if substr(dx_&DX,1,&SN) in (&&DC&DI) then C1=1;ELSE C1=0;
				B1=B1+C1;
				drop C1;
			%end;
			A1=A1+B1;
			DROP B1;
		%end;
		if A1>0 then 	ICD9_E_Ex_&&DIS&DI=1;else ICD9_E_Ex_&&DIS&DI=0;
		label ICD9_E_Ex_&&DIS&DI=&&LBL&DI;
		DROP A1;
	%end;
run;
%mend ICD9_E_EX;
/*	lib_in: 	the library where the input data is
	lib_out: 	the library where the output data is
	data_in: 	the input data
	data_out: 	the output data*/
%ICD9_E_EX(lib_in=comorbid, lib_out=comorbid, data_in=icd9CM3, data_out=ICD9_E_EX);

