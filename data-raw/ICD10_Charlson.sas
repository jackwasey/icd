/*Quan et al. :Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Med Care 2005 Nov; 43(11):1073-1077.*/
/*ICD-10 Charlson */
libname comorbid 'D:\data\icd9';
%macro ICD10_CH (lib_in=, lib_out=, data_in=, data_out=);
data &lib_out..&data_out;
	set &lib_in..&data_in;

	/**Myocardial Infarction**/
	%LET DIS1=MI;
	%LET DC1=%STR('I21','I22','I252');
	%LET LBL1=%STR(Myocardial Infarction);

	/**Congestive Heart Failure**/
	%LET DIS2=CHF;
	%LET DC2=%STR('I43','I50','I099','I110','I130','I132',
	'I255','I420','I425','I426','I427','I428','I429','P290');
	%LET LBL2=%STR(Congestive Heart Failure);

	/**Periphral Vascular Disease**/
	%LET DIS3=PVD;
	%LET DC3=%STR('I70','I71', 'I731','I738','I739','I771',
	'I790','I792','K551','K558','K559','Z958','Z959');
	%LET LBL3=%STR(Periphral Vascular Disease);

	/**Cerebrovascular Disease**/
	%LET DIS4=CEVD;
	%LET DC4=%STR('G45','G46','I60','I61','I62','I63','I64','I65','I66','I67','I68','I69','H340');
	%LET LBL4=%STR(Cerebrovascular Disease);

	/**Dementia**/
	%LET DIS5=DEM;
	%LET DC5=%STR('F00','F01','F02','F03','G30','F051','G311');
	%LET LBL5=%STR(Dementia);

	/*Chronic Pulmonary Disease*/
	%LET DIS6=COPD;
	%LET DC6=%STR('J40','J41','J42','J43','J44','J45','J46','J47',
	'J60','J61','J62','J63','J64','J65','J66','J67',
	'I278','I279','J684','J701','J703');
	%LET LBL6=%STR(Chronic Pulmonary Disease);

	/**Connective Tissue Disease-Rheumatic Disease**/
	%LET DIS7=Rheum;
	%LET DC7=%STR('M05','M32','M33','M34','M06','M315','M351','M353','M360');
	%LET LBL7=%STR(Rheumatic Disease);

	/**Peptic Ulcer Disease**/   
	%LET DIS8=PUD;
	%LET DC8=%STR('K25','K26','K27','K28');
	%LET LBL8=%STR(Peptic Ulcer Disease);

	/**Mild Liver Disease **/
	%LET DIS9=MILDLD;
	%LET DC9=%STR('B18','K73','K74','K700','K701','K702','K703','K709',
	'K717','K713','K714','K715','K760','K762','K763','K764','K768','K769','Z944');  
	 %LET LBL9=%STR(Mild Liver Disease);

	/**Diabetes without complications**/
	%LET DIS10=DIAB_UC;
	%LET DC10=%STR('E100','E101','E106','E108','E109','E110','E111','E116','E118','E119',
	'E120','E121','E126','E128','E129',
	'E130','E131','E136','E138','E139',
	'E140','E141','E146','E148','E149');
	%LET LBL10=%STR(Diabetes without complications);

	/**Diabetes with complications**/
	%LET DIS11=DIAB_C;
	%LET DC11=%STR('E102','E103','E104','E105','E107',
	'E112','E113','E114','E115','E117',
	'E122','E123','E124','E125','E127',
	'E132','E133','E134','E135','E137',
	'E142','E143','E144','E145','E147');
	%LET LBL11=%STR(Diabetes with complications);

	/**Paraplegia and Hemiplegia**/
	%LET DIS12=PARA;
	%LET DC12=%STR('G81','G82','G041','G114','G801','G802',
	'G830','G831','G832','G833','G834','G839');    
	%LET LBL12=%STR(Paraplegia and Hemiplegia);
		
	/**Renal Disease**/
	%LET DIS13=RD;
	%LET DC13=%STR('N18','N19','N052','N053','N054','N055','N056','N057',
	'N250','I120','I131','N032','N033','N034','N035','N036','N037',
	'Z490','Z491','Z492','Z940','Z992');
	%LET LBL13=%STR(Renal Disease);

	/**Cancer**/
	%LET DIS14=CANCER;
	%LET DC14=%STR('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09',
	'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19',
	'C20','C21','C22','C23','C24','C25','C26',
	'C30','C31','C32','C33','C34','C37','C38','C39',
	'C40','C41','C43','C45','C46','C47','C48','C49',
	'C50','C51','C52','C53','C54','C55','C56','C57','C58',
	'C60','C61','C62','C63','C64','C65','C66','C67','C68','C69',
	'C70','C71','C72','C73','C74','C75','C76',
	'C81','C82','C83','C84','C85','C88',
	'C90','C91','C92','C93','C94','C95','C96','C97');
	%LET LBL14=%STR(Cancer);

	/**Moderate or Severe Liver Disease**/
	%LET DIS15=MSLD;
	%LET DC15=%STR('K704','K711','K721','K729','K765','K766','K767','I850','I859','I864','I982');
	%LET LBL15=%STR(Moderate or Severe Liver Disease);

	/**Metastatic Carcinoma **/
	%LET DIS16=METS;
	%LET DC16=%STR('C77','C78','C79','C80');
	%LET LBL16=%STR(Metastatic Carcinoma);

	/**AIDS/HIV**/
	%LET DIS17=HIV;
	%LET DC17=%STR('B20','B21','B22','B24');
	%LET LBL17=%STR(AIDS/HIV);

	%do DI=1 %to 17;		/*ICD10 Charlson: 17 groups*/
		A1=0; 						
		%do DX=1 %to 16; 	/*Diagnosis codes: DX_1 - DX_16*/
			B1=0;
			%do SN=3 %to 5;	/*SN: Number of string: 3,4,5*/
				if substr(dx_&DX,1,&SN) in (&&DC&DI) then C1=1;Else C1=0;
				B1=B1+C1;
				drop C1;
			%end;
			A1=A1+B1;
			DROP B1;
		%end;
		if A1>0 then 	ICD10_CH_&&DIS&DI =1;else ICD10_CH_&&DIS&DI=0;
		label 			ICD10_CH_&&DIS&DI = &&LBL&DI;
		DROP A1;	
	%end;
run;
%mend ICD10_CH ;
/*	lib_in: 	the library where the input data is
	lib_out: 	the library where the output data is
	data_in: 	the input data
	data_out: 	the output data*/
%ICD10_CH(lib_in=comorbid, lib_out=comorbid, data_in=icd10, data_out=ICD10_CH);


