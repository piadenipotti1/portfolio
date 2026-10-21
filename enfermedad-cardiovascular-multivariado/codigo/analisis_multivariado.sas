OPTIONS LOCALE=SPANISH;

PROC IMPORT DATAFILE="heart_disease_uci.csv" /* ajustar a la ruta local */
    OUT=work.corazon
    DBMS=CSV
    REPLACE;
    GETNAMES=YES;
RUN;

DATA work.corazon2;
    SET work.corazon; 
    IF num IN (1, 2,3,4) THEN Diagnóstico = 'Con enfermedad';
    IF num= 0 THEN Diagnóstico = 'Sin enfermedad'; 
  
    IF age < 45 THEN Grupo_Edad = 'Menor a 45';
    ELSE IF age >= 45 AND age < 60 THEN Grupo_Edad = '45-59';
    ELSE IF age >= 60 THEN Grupo_Edad = '60 o mas';
    
RUN;




/*COMPONENTES PRINCIPALES*/
ods output Eigenvectors=autovectores_corazon;
ods output Eigenvalues=autovalores_corazon;
proc princomp data=corazon2  
out=result   prefix=CP;   
var trestbps chol thalch ca oldpeak;
run;
ods output close;
ods output close;

*Correlaciones entre las CP y las variables;
proc corr data=result noprob nosimple nomiss;
var CP1 CP2 CP3;
with trestbps chol thalch ca oldpeak;
run;


/*GRAFICO DE SCORES*/
proc sgplot data=result;
styleattrs datasymbols=(trianglefilled)
datacontrastcolors=(green magenta);
scatter x=CP1 y=CP2 / group=Diagnóstico ; 
xaxis label="CP1"   values=(-3.5 to 5 by 0.5) offsetmin=0 offsetmax=0;
yaxis label="CP2"   values=(-4.5 to 5 by 0.5) offsetmin=0 offsetmax=0;
RefLine 0 / axis=x;
RefLine 0 / axis=y;
keylegend / location=outside position=right across=1;
run;

proc sgplot data=result;
styleattrs datasymbols=(triangleFilled)
datacontrastcolors=(green magenta);
scatter x=CP1 y=CP3 / group=Diagnóstico ; 
xaxis label="CP1"   values=(-3.5 to 5 by 0.5) offsetmin=0 offsetmax=0;
yaxis label="CP3"   values=(-4.5 to 5 by 0.5) offsetmin=0 offsetmax=0;
RefLine 0 / axis=x;
RefLine 0 / axis=y;
keylegend / location=outside position=right across=1;
run;






/* ANALISIS DE CORRESPONDENCIA*/
PROC CORRESP DATA=corazon2 MCA observed chi PLOTS=ALL dimens=2 outc=factor;
    tables Diagnóstico Grupo_Edad cp; 
RUN;

PROC SGPLOT data=factor;
   refline 0 / axis=x lineattrs=(color=gray pattern=dotted);
   refline 0 / axis=y lineattrs=(color=gray pattern=dotted);
   scatter x=Dim1 y=Dim2 / datalabel=_NAME_  
           markerattrs=(symbol=circlefilled color=blue size=8) 
           datalabelattrs=(size=8 family="Arial"); 
   xaxis label="DIM1" values=(-1 to 1.5 by 0.1) ;
   yaxis label="DIM2" values=(-1.5 to 3 by 0.1);
RUN;






/*SELECCION PASO A PASO DE VARIABLES*/
%let _stdvar = ;
proc stepdisc data=corazon2 sle=0.05 sls=0.05 bsscp tsscp;
class Diagnóstico;
var trestbps chol oldpeak ca age thalch;
run;
%put &_stdvar;

/*ANALISIS DISCRIMINANTES CON LAS SELECCIONADAS*/
proc discrim data=corazon2
crossvalidate noclassify crosslisterr outstat=salida1 manova;
class Diagnóstico; var oldpeak ca thalch;
run;

