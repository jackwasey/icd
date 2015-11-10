/*Quan et al. :Coding algorithms for defining comorbidities in ICD-9-CM and ICD-10 administrative data. Med Care 2005 Nov; 43(11):1073-1077.*/
/*ICD-10 Elixhauser*/
libname comorbid "D:\data\ICD9";
%macro ICD10_EX (lib_in=, lib_out=, data_in=, data_out=);
data &lib_out..&data_out;
	set &lib_in..&data_in;

	/*Congestive Heart Failure*/
	%LET DC1=%STR('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428','I429','I43','I50','P290');
	%LET DIS1=CHF;		
	%LET LBL1=%STR(Congestive Heart Failure);

	/*Caridiac Arrhythmia*/
	%LET DC2=%STR('I441','I442','I443','I456','I459','I47','I48','I49','R000','R001','R008','T821','Z450','Z950');
	%LET DIS2=Arrhy;	
	%LET LBL2=%STR(Caridiac Arrhythmia);

	/*Valvular Disease*/
	%LET DC3=%STR('A520','I05','I06','I07','I08','I091','I098','I34','I35','I36','I37','I38','I39','Q230','Q231','Q232','Q233','Z952','Z953','Z954');
	%LET DIS3=VD;		
	%LET LBL3=%STR(Valvular Disease);

	/*Pulmonary Circulation Disorders*/
	%LET DC4=%STR('I26','I27','I280','I288','I289');
	%LET DIS4=PCD;		
	%LET LBL4=%STR(Pulmonary Circulation Disorders);

	/*Peripheral Vascular Disorders*/
	%LET DC5=%STR('I70','I71','I731','I738','I739','I771','I790','I792','K551','K558','K559','Z958','Z959');
	%LET DIS5=PVD;		
	%LET LBL5=%STR(Peripheral Vascular Disorders);

	/*Hypertension Uncomlicated*/
	%LET DC6=%STR('I10');
	%LET DIS6=HPTN_UC;	
	%LET LBL6=%STR(Hypertension Uncomlicated);

	/*Hypertension comlicated*/
	%LET DC7=%STR('I11','I12','I13','I15');
	%LET DIS7=HPTN_C;	
	%LET LBL7=%STR(Hypertension comlicated);

	/*Paralysis*/
	%LET DC8=%STR('G041','G114','G801','G802','G81','G82','G830','G831','G832','G833','G834','G839');
	%LET DIS8=Para;		
	%LET LBL8=%STR(Paralysis);

	/* Other Neurological Disorders*/
	%LET DC9=%STR('G10','G11','G12','G13','G20','G21','G22','G254','G255','G312','G318','G319','G32','G35','G36','G37','G40','G41','G931','G934','R470','R56');
	%LET DIS9=OthND;	
	%LET LBL9=%STR(Other Neurological Disorders);

	/*Chronic Pulmonary Disease*/
	%LET DC10=%STR('I278','I279','J40','J41','J42','J43','J44','J45','J46','J47','J60','J61','J62','J63','J64','J65','J66','J67','J684','J701','J703');
	%LET DIS10=COPD;	
	%LET LBL10=%STR(Chronic Pulmonary Disease);

	/*Diabetes Uncomplicated*/
	%LET DC11=%STR('E100','E101','E109','E110','E111','E119','E120','E121','E129','E130','E131','E139','E140','E141','E149');
	%LET DIS11=Diab_UC;	
	%LET LBL11=%STR(Diabetes Uncomplicated);

	/*Diabetes Complicated*/
	%LET DC12=%STR('E102','E103','E104','E105','E106','E107','E108','E112','E113','E114','E115','E116','E117','E118','E122','E123','E124','E125','E126','E127','E128','E132','E133','E134','E135','E136','E137','E138','E142','E143','E144','E145','E146','E147','E148');
	%LET DIS12=Diab_C;	
	%LET LBL12=%STR(Diabetes Complicated);

	/*Hypothyroidism*/
	%LET DC13=%STR('E00','E01','E02','E03','E890');
	%LET DIS13=Hptothy;	
	%LET LBL13=%STR(Hypothyroidism);

	/*Renal Failure*/
	%LET DC14=%STR('I120','I131','N18','N19','N250','Z490','Z491','Z492','Z940','Z992');
	%LET DIS14=RF;		
	%LET LBL14=%STR(Renal Failure);

	/*Liver Disease*/
	%LET DC15=%STR('B18','I85','I864','I982','K70','K711','K713','K714','K715','K717','K72','K73','K74','K760','K762','K763','K764','K765','K766','K767','K768','K769','Z944');
	%LET DIS15=LD;		
	%LET LBL15=%STR(Liver Disease);

	/*Peptic Ulcer Disease excluding bleeding*/
	%LET DC16=%STR('K257','K259','K267','K269','K277','K279','K287','K289');
	%LET DIS16=PUD_NB;	
	%LET LBL16=%STR(Peptic Ulcer Disease excluding bleeding);

	/*AIDS/HIV*/
	%LET DC17=%STR('B20','B21','B22','B24');
	%LET DIS17=HIV;
	%LET LBL17=%STR(AIDS/HIV);

	/*Lymphoma*/
	%LET DC18=%STR('C81','C82','C83','C84','C85','C88','C96','C900','C902');
	%LET DIS18=Lymp;
	%LET LBL18=%STR(Lymphoma);

	/*Metastatic Cancer*/
	%LET DC19=%STR('C77','C78','C79','C80');
	%LET DIS19=METS;
	%LET LBL19=%STR(Metastatic Cancer);

	/*Solid Tumor without Metastasis*/
	%LET DC20=%STR('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09','C10','C11','C12','C13','C14','C15','C16','C17','C18','C19','C20','C21','C22','C23','C24','C25','C26','C30','C31','C32','C33','C34','C37','C38','C39','C40','C41','C43','C45','C46','C47','C48','C49','C50','C51','C52','C53','C54','C55','C56','C57','C58','C60','C61','C62','C63','C64','C65','C66','C67','C68','C69','C70','C71','C72','C73','C74','C75','C76','C97');
	%LET DIS20=Tumor;
	%LET LBL20=%STR(Solid Tumor without Metastasis);

	/*Rheumatoid Arthsitis/collagen*/
	%LET DC21=%STR('L940','L941','L943','M05','M06','M08','M120','M123','M30','M310','M311','M312','M313','M32','M33','M34','M35','M45','M461','M468','M469');
	%LET DIS21=Rheum_A;
	%LET LBL21=%STR(Rheumatoid Arthsitis/collagen);


	/*Coagulopathy*/
	%LET DC22=%STR('D65','D66','D67','D68','D691','D693','D694','D695','D696');
	%LET DIS22=Coag;
	%LET LBL22=%STR(Coagulopathy);

	/*Obesity*/
	%LET DC23=%STR('E66');
	%LET DIS23=Obesity;
	%LET LBL23=%STR(Obesity);

	/*Weight Loss*/
	%LET DC24=%STR('E40','E41','E42','E43','E44','E45','E46','R634','R64');
	%LET DIS24=WL;
	%LET LBL24=%STR(Weight Loss);

	/*Fluid and Ecletrolyte Disorders*/
	%LET DC25=%STR('E222','E86','E87');
	%LET DIS25=Fluid;
	%LET LBL25=%STR(Fluid and Ecletrolyte Disorders);

	/*Blood Loss Anemia*/
	%LET DC26=%STR('D500');
	%LET DIS26=BLA;
	%LET LBL26=%STR(Blood Loss Anemia);

	/*Deficiency Anemia*/
	%LET DC27=%STR('D508','D509','D51','D52','D53');
	%LET DIS27=DA;
	%LET LBL27=%STR(Deficiency Anemia);

	/*Alcohol Abuse*/
	%LET DC28=%STR('F10','E52','G621','I426','K292','K700','K703','K709','T51','Z502','Z714','Z721');
	%LET DIS28=Alcohol;
	%LET LBL28=%STR(Alcohol Abuse);

	/*Drug Abuse*/
	%LET DC29=%STR('F11','F12','F13','F14','F15','F16','F18','F19','Z715','Z722');
	%LET DIS29=Drug;
	%LET LBL29=%STR(Drug Abuse);

	/*Psychoses*/
	%LET DC30=%STR('F20','F22','F23','F24','F25','F28','F29','F302','F312','F315');
	%LET DIS30=Psycho;
	%LET LBL30=%STR(Psychoses);

	/*Depression*/
	%LET DC31=%STR('F204','F313','F314','F315','F32','F33','F341','F412','F432');
	%LET DIS31=Dep;
	%LET LBL31=%STR(Depression);


	%do DI=1 %to 31;			/*ICD10 Elixhauser: 31 groups*/
		A1=0; 
		%do DX=1 %to 16; 		/*Diagnosis codes: DX_1 - DX_16*/
			B1=0;
			%do SN=3 %to 5;		
				if substr(dx_&DX,1,&SN) in (&&DC&DI) then C1=1;ELSE C1=0;
				B1=B1+C1;
				drop C1;
			%end;
			A1=A1+B1;
			DROP B1;
		%end;
		if A1>0 then 	ICD10_EX_&&DIS&DI=1;else ICD10_EX_&&DIS&DI=0;
		label ICD10_EX_&&DIS&DI=&&LBL&DI;
		DROP A1;
	%end;
run;
%mend ICD10_EX;
/*	lib_in: 	the library where the input data is
	lib_out: 	the library where the output data is
	data_in: 	the input data
	data_out: 	the output data*/
%ICD10_EX(lib_in=comorbid, lib_out=comorbid, data_in=icd10, data_out=ICD10_EX);


