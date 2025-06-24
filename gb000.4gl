###############################################################################
# RUTINA  : gb000.4gl
# VERSION : 1.0
# FECHA   : 17/09/91
# OBJETIVO: REalizar rutinas comunes
# AUTOR   : Rudy Von Landwuest
# COMPILAR:  gb000.4gl 
# Modif :(PCV) 13/09/2000 En la tabla gbcfm  se aumento el campo mfir 
# 	   actualimetne la tabla tiene 4 campos user,nmod,nopr,mfir
# V 1.0.1  Aumenta la rutina f0251 control tabla aduag
###############################################################################
#@001 JPR 18/05/2009 Se agrego la funcion get_nombre4gi para obtener el nombre del *.4gi
#@002 LSL 31/06/2009 Rutina para obtener el mensaje al usuario
#@003 JPR 16/07/2009 Rutina de escritura en log
#@004 ATE 11/04/2011 Redondeo
#@005 GAC	07/04/2017	Formatear caracteres extraños de texto
#@006 JTD	26/11/2018	Redondeos
###############################################################################
DATABASE tbsfi

#@006 - Inicio
DEFINE
  		g_valr DECIMAL(4,2)   #Valor Redondeo

FUNCTION f0100_prepara_sentencias_gb000()
DEFINE
    l_SQL CHAR(500)
  
    LET l_SQL = "SELECT pcprmvalo FROM pcprm WHERE pcprmflag = ? "
    PREPARE p01_sel_pcpr FROM l_SQL         
END FUNCTION
#@006 - Fin

FUNCTION f0000_open_database_gb000()
	DEFINE	l_db  CHAR(50),
		l_string CHAR(80)
	WHENEVER ERROR CONTINUE
	LET l_db = NULL
	LET l_db = fgl_getenv("DBSFI") CLIPPED
	IF l_db IS NULL  or l_db ="" THEN	
		LET l_db = "tbsfi"
	END IF
	LET l_string = "DATABASE ",l_db CLIPPED
	CLOSE DATABASE 
	DATABASE l_db
	IF STATUS <> 0 THEN 
		ERROR "NO PUDE ABRIR BASE DE DATOS"
		SLEEP 2
		RETURN FALSE
	END IF
	WHENEVER ERROR STOP
	RETURN TRUE
	############
END FUNCTION 

FUNCTION f0010_buscar_database_gb000()
	DEFINE	l_db  CHAR(15),
		l_string CHAR(20)
	WHENEVER ERROR CONTINUE
	LET l_db = NULL
	LET l_db = fgl_getenv("DBSFI") CLIPPED
	IF l_db IS NULL  or l_db ="" THEN	
		LET l_db = "tbsfi"
	END IF
	RETURN l_db
END FUNCTION

FUNCTION f0100_redondeo_gb000(m,d)
	 DEFINE m DECIMAL(15),
		x DECIMAL(15),
		r DECIMAL(15),
		l DECIMAL(15),
		d SMALLINT
	 IF m = 0
	    THEN RETURN 0
	 END IF
	 IF m < 0
	    THEN LET l = -1
	    ELSE LET l =  1
	 END IF
	 LET x = m * (10 ** d)
	 LET m = m * (10 ** d) USING "#############&"
	 LET r = x - m
	 IF  r	>= 0.5
	     THEN LET m = (m + 1) / (10 ** d)
	     ELSE LET m = m / (10 ** d)
	 END IF
	 RETURN m * l
END FUNCTION

FUNCTION f0200_ayuda_gb000()
  DEFINE salir CHAR(1)
  OPEN WINDOW w1_gb000 AT 2,44 WITH FORM "gb000a"
    ATTRIBUTE (FORM LINE 1)
    PROMPT " " FOR CHAR salir
       ON KEY (CONTROL-C)
          CLOSE WINDOW w1_gb000
          RETURN
    END PROMPT
  CLOSE WINDOW w1_gb000
END FUNCTION

FUNCTION f0251_in_usuario_agencia_gb000(l_user)
	DEFINE  l_user  LIKE adusr.adusrusrn,
		l1      RECORD LIKE aduag.*,
		l_qry   CHAR(600) ,
		l_agen  SMALLINT,
		l_flag  SMALLINT-- 0= TODO,1=AGENCIAS
		LET l_agen = 0
		LET l_flag = 1
		SELECT adusragen INTO l_agen
		FROM adusr
		WHERE adusrusrn = l_user
		IF STATUS=NOTFOUND THEN
			LET l_qry = " IN (0 "
		ELSE
			LET l_qry = " IN (",l_agen USING "<<<"
		END IF
      		DECLARE q_aduag CURSOR FOR
       	       	SELECT *
                FROM aduag
               	WHERE aduagusrn = l_user
               	ORDER BY aduagusrn,aduagagen
      		FOREACH q_aduag INTO l1.*
              		IF l1.aduagagen = -1 THEN
                      		LET l_qry = " 1=1"
                      		LET l_qry = " 1=1"
                      		LET l_flag = 0
                      EXIT FOREACH
              	END IF
              	LET l_qry = l_qry CLIPPED,",",l1.aduagagen USING "<<<"
      		END FOREACH
      		IF l_flag = 1 THEN
              		LET l_qry = l_qry CLIPPED,")"
      		END IF
      		RETURN l_flag,l_qry
END FUNCTION

FUNCTION f0300_oficina_gb000(l_nofi)
	DEFINE l1 RECORD LIKE gbofi.*,
	       l_nofi LIKE gbofi.gbofinofi
	INITIALIZE l1.* TO NULL
	SELECT * INTO l1.*
	FROM gbofi
	WHERE gbofinofi = l_nofi
	IF l1.gbofihost IS NULL THEN
		LET l1.gbofihost= "tbsfi"
	END IF
	RETURN l1.*
END FUNCTION

FUNCTION f0310_fecha_gb000(l_fech)
	DEFINE l_fech  DATE,
	       l_nano  SMALLINT,
	       l_char  CHAR(10),
	       l_date  CHAR(5)
	LET l_date = fgl_getenv("DBDATE")
	IF l_date = "DMY4/" OR l_date = "dmy4/" THEN
	    LET l_char = l_fech USING "dd/mm/yyyy"
	    LET l_nano = YEAR(l_fech)
	    IF l_nano < 1910 THEN
	        LET l_char[7,8] = "20"
	    END IF
	    RETURN l_char
	ELSE
	    RETURN l_fech
	END IF
END FUNCTION 

FUNCTION f0400_UserFirma_gb000(l_user,l_modulo,l_nopr)
	DEFINE l_user	like gbcfm.gbcfmuser,
	       l_modulo like gbcfm.gbcfmmodn,
	       l_nopr   like gbcfm.gbcfmnopr
	DELETE FROM gbcfm WHERE gbcfmuser = l_user
	#INSERT INTO gbcfm VALUES (l_user,l_modulo,l_nopr) 
	INSERT INTO gbcfm VALUES (l_user,l_modulo,l_nopr,0)#version 2.1.1
END FUNCTION

FUNCTION f0400_tipo_cambio_gb000(l_ttrn,l_cmon,l_mvia,t0)
	DEFINE t0 RECORD LIKE gbpmt.*,
		l_ttrn SMALLINT, # 1 debito o 2 abono en cta
		l_cmon SMALLINT, #moneda de la operacion
		l_mvia SMALLINT, # moneda de la via
		l_tcam LIKE gbpmt.gbpmttcof  #tipo de cambio
	#------------------------------------------#
	#determina el valor del tipo de cambio para 
	#convertir en la moneda de la via
	#------------------------------------------#
	LET l_tcam = t0.gbpmttcof
	IF l_cmon = 4 OR l_mvia = 4 THEN
		LET l_tcam = t0.gbpmttufv
	END IF
	# debito en cta , o cobro de prestamos
	IF l_ttrn = 1 THEN
        	IF l_mvia = 1 AND l_cmon = 2 THEN
			LET l_tcam = t0.gbpmttcve
        	END  IF
		IF l_mvia = 2 AND l_cmon = 1 THEN
			LET l_tcam = t0.gbpmttcco
		END IF 
		IF l_mvia = 1 AND l_cmon = 4 THEN
		 	LET l_tcam = t0.gbpmttufv
		END IF 
		IF l_mvia = 4 AND l_cmon = 1 THEN
		 	LET l_tcam = t0.gbpmttufv # 1/t0.gbpmttufv
		END IF 
		#---para estos casos de $ y ufv se guarda factor en el tcambio-#
		IF l_mvia = 2 AND l_cmon = 4 THEN
		 	LET l_tcam = t0.gbpmttcco/t0.gbpmttufv
		END IF 
		IF l_mvia = 4 AND l_cmon = 2 THEN
		 	LET l_tcam = t0.gbpmttcve/t0.gbpmttufv
		END IF 
	ELSE
		#Abono en cta/desembolso
        	IF l_mvia = 1 AND l_cmon = 2 THEN
			LET l_tcam = t0.gbpmttcco
        	END  IF
		IF l_mvia = 2 AND l_cmon = 1 THEN
			LET l_tcam = t0.gbpmttcve
		END IF 
		IF l_mvia = 1 AND l_cmon = 4 THEN
		 	LET l_tcam = t0.gbpmttufv
		END IF 
		IF l_mvia = 4 AND l_cmon = 1 THEN
		 	LET l_tcam = t0.gbpmttufv # 1/t0.gbpmttufv
		END IF 
		#-- para estos casos de $ y ufv se guarda factor en el tcambio-#
		IF l_mvia = 2 AND l_cmon = 4 THEN
		 	LET l_tcam = t0.gbpmttcve/t0.gbpmttufv
		END IF 
		IF l_mvia = 4 AND l_cmon = 2 THEN
		 	LET l_tcam = t0.gbpmttcco/t0.gbpmttufv
		END IF 
	END IF
RETURN l_tcam
END FUNCTION

FUNCTION f0500_impt_para_trans_gb000(l_cmon,l_mvia,l_impt,l_tcam)
	DEFINE l_impo DECIMAL(14,2),
	       l_cmon DECIMAL(2,0), #moneda de la operacion
	       l_mvia DECIMAL(2,0), #moneda de la via
	       l_impt DEC(14,2),
	       l_tcam LIKE gbpmt.gbpmttcof,
	       l_imptvia DEC(14,2)

	LET l_imptvia = l_impt
	IF l_mvia = 1 AND l_cmon = 2 THEN
               	LET l_imptvia = f0100_redondeo_gb000((l_impt*l_tcam),2)
	END IF
	IF l_mvia = 2 AND l_cmon = 1 THEN
               	LET l_imptvia = f0100_redondeo_gb000((l_impt/l_tcam),2)
	END IF
	IF l_mvia = 1 AND l_cmon = 3 THEN
               	LET l_imptvia = f0100_redondeo_gb000((l_impt*l_tcam),2)
	END IF
	 #---------ufv ver 3.0.4 -----------#
        IF l_mvia = 4 AND l_cmon = 1 THEN
                LET l_imptvia = f0100_redondeo_gb000((l_impt/l_tcam),2)
        END IF
        IF l_mvia = 1 AND l_cmon = 4 THEN
                LET l_imptvia = f0100_redondeo_gb000((l_impt*l_tcam),2)
        END IF
        IF l_mvia = 4 AND l_cmon = 2 THEN
                LET l_imptvia = f0100_redondeo_gb000((l_impt*l_tcam),2)
        END IF
        IF l_mvia = 2 AND l_cmon = 4 THEN
                LET l_imptvia = f0100_redondeo_gb000((l_impt/l_tcam),2)
        END IF
RETURN l_imptvia
END FUNCTION

FUNCTION f7000_impts_para_contab_gb000(l_impt,l_cmon,t0)
DEFINE
  t0 RECORD LIKE gbpmt.*,
  l_impt  DEC(14,2),
  l_cmon  SMALLINT,
  l_impi  DEC(14,2),
  l_impc  DEC(14,2)

  LET l_impi = l_impt
  LET l_impc = l_impt

	IF l_cmon = 3 THEN
		LET l_cmon = 2
	END IF
  IF l_cmon = 1 AND t0.gbpmtmimp = 2 THEN
    LET l_impi = f0100_redondeo_gb000((l_impi/t0.gbpmttcof),2)
  END IF
  IF l_cmon = 2 AND t0.gbpmtmimp = 1 THEN
    LET l_impi = f0100_redondeo_gb000((l_impi*t0.gbpmttcof),2)
  END IF
  IF l_cmon = 1 AND t0.gbpmtmcon = 2 THEN
    LET l_impc = f0100_redondeo_gb000((l_impc/t0.gbpmttcof),2)
  END IF
  IF l_cmon = 2 AND t0.gbpmtmcon = 1 THEN
    LET l_impc = f0100_redondeo_gb000((l_impc*t0.gbpmttcof),2)
  END IF
  IF l_cmon = 4 AND t0.gbpmtmimp = 1 THEN		# V 3.0.0
    LET l_impi = f0100_redondeo_gb000((l_impi*t0.gbpmttufv),2)
  END IF
  IF l_cmon = 4 AND t0.gbpmtmimp = 2 THEN		# V 3.0.0
    LET l_impi = f0100_redondeo_gb000((l_impi/t0.gbpmttufv),2)
  END IF

  RETURN l_impi,l_impc
END FUNCTION

FUNCTION f5100_buscar_firma_gb000(l_user,l_modulo,l_nopr)
	DEFINE l_user	like gbcfm.gbcfmuser,
	       l_modulo like gbcfm.gbcfmmodn,
	       l_nopr   like gbcfm.gbcfmnopr,
	       l_mfir   LIKE gbcfm.gbcfmmfir
	INITIALIZE l_mfir TO NULL
	SELECT gbcfmmfir INTO l_mfir
	FROM gbcfm 
	WHERE gbcfmuser = l_user
		AND gbcfmmodn = l_modulo
		AND gbcfmnopr = l_nopr
	IF STATUS = NOTFOUND THEN
		RETURN FALSE
	END IF
	IF l_mfir = 9 THEN
		RETURN TRUE
	END IF
	RETURN FALSE
END FUNCTION

FUNCTION f1000_notas_gb000()
	DEFINE	t50	RECORD
			desc1	LIKE 	gbnot.gbnotdes1,
			desc2	LIKE 	gbnot.gbnotdes2,
			desc3	LIKE 	gbnot.gbnotdes3,
			marca	LIKE 	gbnot.gbnotmrcb
		END RECORD,
		gb_des1 CHAR(045),
		gb_des2 CHAR(045),
		gb_des3 CHAR(045)
	SELECT gbnotdes1,gbnotdes2,gbnotdes3,gbnotmrcb INTO t50.*
	FROM gbnot
	LET gb_des1 = NULL
	LET gb_des2 = NULL
	LET gb_des3 = NULL
	IF t50.marca = "S" THEN
		LET gb_des1 = t50.desc1
		LET gb_des2 = t50.desc2
		LET gb_des3 = t50.desc3
	END IF
	RETURN gb_des1,gb_des2,gb_des3
END FUNCTION

FUNCTION f1100_dav_gb000(l_num) 
	DEFINE 	l_num	DECIMAL(16,0),
		l_bas   CHAR(16),
		l_cod   CHAR(16),
		l_tot   INTEGER,
		x	CHAR(1),
		i	SMALLINT,
		largo	SMALLINT,
		l_aux   SMALLINT,
		l_banco	SMALLINT 
	LET l_bas ="1212121212121212"
	LET l_banco = 0
	SELECT crctlcbco INTO l_banco
		FROM crctl
	LET l_cod = l_banco USING "&&&",l_num USING "&&&&&&&&&&&&&" CLIPPED
	LET l_tot = 0
	FOR i = 1 TO length(l_cod)
		LET l_aux = (l_bas[i,i] * l_cod[i,i]) 
		IF l_aux > 9 THEN
			LET l_aux = l_aux - 9
		END IF
		LET l_tot = l_tot + l_aux
	END FOR
	LET l_tot = l_tot mod 10
	IF l_tot > 0 THEN
		LET l_tot = 10 - l_tot 
	END IF
	RETURN l_tot
END FUNCTION 
		

FUNCTION f0001_gbsrl_gb000(l_tabl)
	DEFINE  l_corr DEC(20,0),
		l_stat SMALLINT,     -- 0= OK, 1=FALLA
		l_msj  CHAR(90),
		l_tabl CHAR(5),
		s1     CHAR(200),
		s2     CHAR(200)

	LET l_corr = 0
	LET l_msj = ""
	LET s1 = "SELECT gbsrlcorr FROM gbsrl WHERE gbsrltabl = '",l_tabl,"'",
		" FOR UPDATE"

	#LET s2 = "UPDATE gbsrl SET gbsrlcorr=  gbsrlcorr + 1",
	LET s2 = "UPDATE gbsrl SET gbsrlcorr= ?",# V 4.0.0
		" WHERE gbsrltabl = '",l_tabl,"'"
       	PREPARE select_gbsrl FROM s1
       	PREPARE update_gbsrl FROM s2

       	DECLARE q_gbsrl CURSOR FOR select_gbsrl
       	OPEN q_gbsrl
       	FETCH q_gbsrl INTO l_corr
       	LET l_stat = STATUS
       	IF l_stat <> 0 THEN -- No encontro Registro
               LET l_msj = "NO PUEDE RECUPERAR CORRELATIVO TABLA: ",l_tabl
               RETURN 1,l_corr,l_msj
       	END IF
       	CLOSE q_gbsrl
       	#EXECUTE update_gbsrl
       	LET l_corr = l_corr + 1                                 # V 4.0.0
       	EXECUTE update_gbsrl USING l_corr                       # V 4.0.0
       	IF STATUS  <> 0 THEN
               LET l_msj = "NO PUEDE ACTUALIZAR CORRELATIVO TABLA:",l_tabl
               RETURN 1,l_corr,l_msj
       	END IF
       	RETURN 0,l_corr,l_msj
END FUNCTION


FUNCTION f6100_cabecera_gb000(l_nemp,l_nmod,l_titu,l_fdia,l_vers)
	DEFINE 	l_nemp    	CHAR(33),
	       	l_nmod    	CHAR(16),
	       	l_titu    	CHAR(33),
	       	l_fdia    	DATE,
		l_vers		CHAR(6),
	       	l_string  	CHAR(33),
	       	i         	SMALLINT
#------ DISPLAY DEL MODULO ---------------------------------#
        LET i                  = ((16 - LENGTH(l_nmod)) / 2)
        LET l_string           = " "
        LET l_string[i+1,16-i] = l_nmod
        DISPLAY l_string CLIPPED AT 4,2
#------ DISPLAY DEL NOMBRE DE LA EMPRESA -------------------#
        LET i                  = ((33 - LENGTH(l_nemp)) / 2)
        LET l_string           = " "
        LET l_string[i+1,33-i] = l_nemp
        DISPLAY l_string CLIPPED AT 4,24
#------ DISPLAY DE LA FECHA --------------------------------#
        DISPLAY l_fdia   USING "   dd/mm/yyyy   " AT 4,63
#------ DISPLAY DEL TITULO DEL PROGRAMA --------------------#
        LET i                  = ((33 - LENGTH(l_titu)) / 2)
        LET l_string           = " "
        LET l_string[i+1,33-i] = l_titu
        DISPLAY l_string AT 5,24
#------ DISPLAY DE LA VERSION DEL PROGRAMA -----------------#
	LET l_string           = " Ver. ",l_vers
        LET i                  = 77 - LENGTH(l_string)
        DISPLAY l_string CLIPPED," " AT 22,i
END FUNCTION

#@001 inicio
-------------------------------------------------------------------------------
FUNCTION print_nombre4gi_gb000(p_version, p_y)  
-------------------------------------------------------------------------------
DEFINE
	p_version CHAR(20),
	l_x, p_y  SMALLINT
	
	LET l_x = 77
	
	LET p_version = get_nombre4gi_gb000() CLIPPED, " ", p_version CLIPPED
	LET l_x = l_x - LENGTH(p_version)
	
	DISPLAY " ", p_version CLIPPED, " " AT p_y, l_x
END FUNCTION

-------------------------------------------------------------------------------
FUNCTION get_nombre4gi_gb000()
-------------------------------------------------------------------------------
define
	l_nombre4gi CHAR(50),
	l_index		  SMALLINT,
	l_length    SMALLINT
   
	LET l_nombre4gi = arg_val(0)
	LET l_length    = LENGTH(l_nombre4gi CLIPPED)

	IF l_nombre4gi[l_length-3,l_length] = ".4gi" THEN
		LET l_nombre4gi = l_nombre4gi[1,l_length-4]
	END IF

	FOR l_index=l_length TO 1 STEP -1
		IF l_nombre4gi[l_index,l_index]="/" THEN
			EXIT FOR
		END IF
	END FOR

	LET l_nombre4gi = l_nombre4gi[l_index+1,l_length]

	RETURN l_nombre4gi
END FUNCTION
#@001 fin

#@002 inicio
FUNCTION f5020_muestra_mensaje_gb000(l_cage)
DEFINE
	l_cage LIKE pcmpc.pcmpccage,
	l_mens LIKE gbdac.gbdacmens,

  ar ARRAY[3] OF RECORD
    linea CHAR(40)
  END RECORD

	LET l_mens = null	
	SELECT gbdacmens INTO l_mens
    FROM gbdac
   WHERE gbdaccage = l_cage
     AND gbdactmen != '9'
	IF SQLCA.SQLCODE = 100 THEN
		RETURN
	END IF
  LET ar[1].linea = l_mens[1,40]
  LET ar[2].linea = l_mens[41,80]
  LET ar[3].linea = l_mens[81,120]
  CALL SET_COUNT(3)
  OPEN WINDOW win_gb000b AT 9,16 WITH FORM "gb000b" ATTRIBUTE (FORM LINE 1)
  LET INT_FLAG = FALSE
  DISPLAY ARRAY ar TO s1.*
    ON KEY (CONTROL-C)
      EXIT DISPLAY
  END DISPLAY
  LET INT_FLAG = FALSE
  CLOSE WINDOW win_gb000b
END FUNCTION
#@002 fin

FUNCTION f5030_log_gb000(p_mensaje, p_spool)
DEFINE
	p_mensaje CHAR(300),
	p_spool   CHAR(100),
	l_comando CHAR(500)

  LET p_mensaje = TODAY USING "dd-mm-yy", " ", TIME, " ", p_mensaje CLIPPED
	LET l_comando = "echo ", p_mensaje CLIPPED, " >> ", p_spool CLIPPED
	
	run l_comando CLIPPED
END FUNCTION

#@004 ini
FUNCTION f0100_redondeo2_gb000(a,b,c)
#a=numero a redondear
#b=decimales a redondear, este parametro al parecer no tiene utilidad pero no ha se descartado
#c=orientacion del redondeo. 0=inferior, 1=normal, 2=superior

	DEFINE
		a DECIMAL(15),
		b,c SMALLINT,
		d DECIMAL(15),
		e DECIMAL(15),
		f DECIMAL(15),
		n DECIMAL(15),
		dif decimal(10,2)
	
	IF a=0 THEN RETURN 0,0 END IF
	IF a<0 THEN LET f=-1 ELSE LET f=1 END IF
	LET d=a*(10 ** b)
	LET a=a*(10 ** b) USING "#############&"
	LET e=d-a
	IF  e>=0.5 THEN
		LET a=(a+1)/(10 ** b) 
	ELSE 
		LET a=a/(10 ** b) 
	END IF
	LET a=a*f
	CASE c
		WHEN 0 #redondeo inferior
			CALL f0100_redon_inferior_gb000(a) RETURNING n
		WHEN 1 #redondeo normal
               let n=a
		WHEN 2 #redondeo superior
			CALL f0100_redon_superior_gb000(a) RETURNING n
		OTHERWISE #redondeo normal
               let n=a			
	END CASE  
	     
          let dif = n-a

          return n,dif
                
END FUNCTION

FUNCTION f0100_redon_inferior_gb000(a)
	DEFINE
		a DECIMAL(15,2),
		b DECIMAL(15,2),
		c INTEGER,
		d DECIMAL(15,2),
		e DECIMAL(15,2),
		f DECIMAL(15,2),
		g DECIMAL(15,2)
	
	IF a=0 THEN RETURN 0 END IF
	IF a<0 THEN LET b=a*-1 ELSE LET b=a END IF
	LET b=b*10
	LET c=b
	LET d=b-c
	IF d<0.5 THEN LET e=0 ELSE LET e=0.5 END IF
	LET f=b-d+e
	LET g=f/10
	IF a<0 THEN LET a=g*-1 ELSE LET a=g END IF
	RETURN a
END FUNCTION

FUNCTION f0100_redon_superior_gb000(a)
	DEFINE
		a DECIMAL(15,2),
		b DECIMAL(15,2),
		c INTEGER,
		d DECIMAL(15,2),
		e DECIMAL(15,2),
		f DECIMAL(15,2),
		g DECIMAL(15,2)
	
	IF a=0 THEN RETURN 0 END IF
	IF a<0 THEN LET b=a*-1 ELSE LET b=a END IF
	LET b=b*10
	LET c=b
	LET d=b-c
	IF d=0 THEN
		LET e=0
	ELSE
		IF d<0.5 THEN
			LET e=0.5
		ELSE
			IF d>0.5 THEN
				LET e=1
			ELSE
				LET e=0.5
			END IF
		END IF
	END IF
	LET f=b-d+e
	LET g=f/10
	IF a<0 THEN LET a=g*-1 ELSE LET a=g END IF
	RETURN a
END FUNCTION
#@004 fin

FUNCTION valida_evento(l_coderr)
DEFINE
   l_coderr  INTEGER,
   l_comando CHAR(100),
   l_stterr INTEGER

   LET l_comando = "sh /u/bexe/coderr.com ", l_coderr 
   RUN l_comando RETURNING l_stterr

   RETURN l_stterr
END FUNCTION

FUNCTION escribe_log_gb000(l_logfile, l_msge)
DEFINE
   l_logfile CHAR(100),
   l_fecha   CHAR(23),
   l_msge    CHAR(100),
   l_comando CHAR(150)

   LET l_fecha = EXTEND(CURRENT,YEAR TO MINUTE)
   LET l_msge = l_fecha[1,16], " ", l_msge CLIPPED
   LET l_comando = "echo '", l_msge CLIPPED, "' >> ", l_logfile CLIPPED
   RUN l_comando CLIPPED
END FUNCTION


#Inicio@005
 FUNCTION f1100_cleantext_gb000(str)
DEFINE strText CHAR(1500),
		str CHAR(1500)
  
	LET strText = str CLIPPED
	LET strText = f1200_Replace_gb000(strText,"\"","&quot;")
	LET strText = f1200_Replace_gb000(strText, "'", "&quot;")
	LET strText = f1200_Replace_gb000(strText, "¥", "&Ntilde;")
  LET strText = f1200_Replace_gb000(strText, "`", " ")
  LET strText = f1200_Replace_gb000(strText, '"', " ")
  LET strText = f1200_Replace_gb000(strText, "´", " ")
	
	RETURN strText
  
END FUNCTION

FUNCTION f1200_Replace_gb000(str, strFind, strReplace)
	DEFINE str CHAR(5000),
		otra CHAR(5000),
		strReplace CHAR(5000),
		strFind CHAR(5000),
		i INTEGER,
		j INTEGER,
		pos INTEGER,
		tam INTEGER,
		tamr INTEGER,
		tamf INTEGER

	LET pos = 0
	LET i = 1
	LET j = i
	LET tam = LENGTH(str)
	LET tamr = LENGTH(strReplace)
	LET tamf = LENGTH(strFind)
	
	IF tamf = 0 THEN LET tamf=1 END IF
	FOR i=1 TO LENGTH(str)
			IF f1300_comparacion_gb000(str,strFind,i) = TRUE THEN
				LET pos = i
				LET otra = f1400_agregar_gb000(otra, strReplace, j)
				LET j = j + (tamr-1)
				LET i = i + (tamf-1)
			ELSE
				LET otra[j] = str[i]
			END IF
			LET j = j + 1
	END FOR
	LET str = otra
	RETURN str

END FUNCTION

FUNCTION f1300_comparacion_gb000(cadena, strReplace, i)
	DEFINE cadena CHAR(5000),
		strReplace CHAR(5000),
		i INTEGER,
		k INTEGER,
		tam INTEGER
		LET tam = LENGTH(strReplace)
		
		# Caso espacio en blanco
		IF tam = 0 THEN LET tam=1 END IF
		IF (i+(tam-1)) <= LENGTH(cadena) THEN
			FOR k = 1 TO tam
				IF cadena[i] <> strReplace[k] THEN
					RETURN FALSE
				END IF
				LET i = i + 1
			END FOR
		ELSE
			RETURN FALSE
		END IF
		RETURN TRUE
END FUNCTION

FUNCTION f1400_agregar_gb000(otra, strReplace, pos)
	DEFINE otra CHAR(5000),
		strReplace CHAR(5000),
		pos INTEGER,
		i INTEGER,
		tam INTEGER
		
		LET tam = LENGTH(strReplace)
		
		FOR i = 1 TO tam
			LET otra[(pos-1)+i] = strReplace[i]
		END FOR
		
		RETURN otra

END FUNCTION
#Fin@005

#@006 Inicio
FUNCTION f0100_redondeo3_gb000(a,b,c)
#a=numero a redondear
#b=decimales a redondear, este parametro al parecer no tiene utilidad pero no ha se descartado
#c=orientacion del redondeo. 0=inferior, 1=normal, 2=superior
	DEFINE
		a DECIMAL(15),
		b,c SMALLINT,
		d DECIMAL(15),
		e DECIMAL(15),
		f DECIMAL(15),
		n DECIMAL(15),
		dif decimal(10,2)
	 ,l_val INTEGER 
	
  LET l_val = 485
 	EXECUTE p01_sel_pcpr USING l_val INTO g_valr
 	IF g_valr IS NULL OR g_valr = '' THEN
				LET g_valr = 0.99
 	END IF
 	
	IF a=0 THEN RETURN 0,0 END IF
	IF a<0 THEN LET f=-1 ELSE LET f=1 END IF
	LET d=a*(10 ** b)
	LET a=a*(10 ** b) USING "#############&"
	LET e=d-a	
	
	IF  e>=g_valr THEN
		LET a=(a+1)/(10 ** b) 
	ELSE 
		LET a=a/(10 ** b) 
	END IF
	LET a=a*f
	CASE c
		WHEN 0 #redondeo inferior
			CALL f0100_redon_inferior1_gb000(a) RETURNING n
		WHEN 1 #redondeo normal
               let n=a
		WHEN 2 #redondeo superior
			CALL f0100_redon_superior1_gb000(a) RETURNING n
		OTHERWISE #redondeo normal
               let n=a
	END CASE
          let dif = n-a
          return n,dif
END FUNCTION

FUNCTION f0100_redon_inferior1_gb000(a)
	DEFINE
		a DECIMAL(15,2),
		b DECIMAL(15,2),
		c INTEGER,
		d DECIMAL(15,2),
		e DECIMAL(15,2),
		f DECIMAL(15,2),
		g DECIMAL(15,2)
	
	IF a=0 THEN RETURN 0 END IF
	IF a<0 THEN LET b=a*-1 ELSE LET b=a END IF
	LET b=b*10
	LET c=b
	LET d=b-c
	IF d<g_valr THEN LET e=0 ELSE LET e=g_valr END IF
	LET f=b-d+e
	LET g=f/10
	IF a<0 THEN LET a=g*-1 ELSE LET a=g END IF
	RETURN a
END FUNCTION

FUNCTION f0100_redon_superior1_gb000(a)
	DEFINE
		a DECIMAL(15,2),
		b DECIMAL(15,2),
		c INTEGER,
		d DECIMAL(15,2),
		e DECIMAL(15,2),
		f DECIMAL(15,2),
		g DECIMAL(15,2)
	
	IF a=0 THEN RETURN 0 END IF
	IF a<0 THEN LET b=a*-1 ELSE LET b=a END IF
	LET b=b*10
	LET c=b
	LET d=b-c
	IF d=0 THEN
		LET e=0
	ELSE
		IF d<g_valr THEN
			LET e=g_valr
		ELSE
			IF d>g_valr THEN
				LET e=1
			ELSE
				LET e=g_valr
			END IF
		END IF
	END IF
	LET f=b-d+e
	LET g=f/10
	IF a<0 THEN LET a=g*-1 ELSE LET a=g END IF
	RETURN a
END FUNCTION
#@006 Fin
