###############################################################################
# PROGRAMA: gb001.4gl
# VERSION : 1.0
# OBJETIVO: RUTINA DE IMPRESION 
# AUTOR   : JAVIER NAVAS CLOUZET
# FECHA   : 21/10/92
# COMPILAR: gb001.4gl
###############################################################################
#	#@001	BBZ	09/08/2024	Ampliacion de caracteres de variable
#
###############################################################################
DATABASE tbsfi

FUNCTION f0100_imprimir_gb001(l_arch)
DEFINE
  #l_arch CHAR(50)   # Archivo de impresion		#@001
  l_arch CHAR(90)   # Archivo de impresion		#@001
  
	CALL f0300_proceso_gb001(l_arch)
END FUNCTION

FUNCTION f0300_proceso_gb001(l_arch)
DEFINE
  p	RECORD 
    z0  CHAR(2),
    a	  CHAR(1),
    b	  CHAR(1),
    c	  CHAR(1),
    d	  CHAR(1),
    e	  CHAR(1),
    c1  CHAR(5),
    n1  CHAR(40),
    c2  CHAR(5),
    n2  CHAR(40),
    c3  CHAR(5),
    n3  CHAR(40),
    c4  CHAR(5),
    n4  CHAR(40),
    c5  CHAR(5),
    n5  CHAR(40),
    z1  CHAR(2)
  END RECORD,
  #l_arch CHAR(50),  # Archivo de impresion	#@001
  l_arch CHAR(90),   # Archivo de impresion		#@001
  l_flag SMALLINT,
	i	     SMALLINT,
	max_c  SMALLINT,
	reg	   INTEGER ,
	l_cod  CHAR(5),
	l_nom  CHAR(30),
	aux_c  CHAR(5),
  aux_n  CHAR(30)

	OPEN WINDOW w1_gb001a AT 13,35 WITH FORM "gb001a" ATTRIBUTES(FORM LINE 1)
	
	DISPLAY l_arch TO mensaje

  ####################################
  # RUTINA DE SELECCION
  ####################################
	DECLARE c_cursor SCROLL CURSOR FOR
		SELECT adprncprn,adprndesc FROM adprn

	LET max_c = 5
	LET reg   = 1
	LET i = 1
	OPEN c_cursor 
	INITIALIZE p.* TO NULL 
	FETCH  ABSOLUTE i c_cursor INTO p.c1, p.n1
	IF status = NOTFOUND THEN
		ERROR "NO EXISTE REGISTRO"
	END IF
	LET i = i + 1
	FETCH  ABSOLUTE i c_cursor INTO p.c2, p.n2
	LET i = i + 1
	FETCH  ABSOLUTE i c_cursor INTO p.c3, p.n3
	LET i = i + 1
	FETCH  ABSOLUTE i c_cursor INTO p.c4, p.n4
	LET i = i + 1
	FETCH  ABSOLUTE i c_cursor INTO p.c5, p.n5

  INPUT BY NAME p.* WITHOUT DEFAULTS 
    ON KEY (CONTROL-C,INTERRUPT)
      LET INT_FLAG = TRUE
			LET l_cod = NULL
			LET l_nom = NULL
			EXIT INPUT

		ON KEY (CONTROL-M)
			CASE
        WHEN INFIELD(a) 
					LET l_cod = p.c1
					LET l_nom = p.n1
				WHEN INFIELD(b)
					LET l_cod = p.c2
					LET l_nom = p.n2
				WHEN INFIELD(c)
					LET l_cod = p.c3
					LET l_nom = p.n3
				WHEN INFIELD(d)
					LET l_cod = p.c4
					LET l_nom = p.n4
				WHEN INFIELD(e)
					LET l_cod = p.c5
					LET l_nom = p.n5
			END CASE
			EXIT INPUT
			
		BEFORE FIELD z0
			IF reg = 1 THEN
				NEXT FIELD a
			END IF
			LET reg = reg -1
			LET p.c5 = p.c4
			LET p.n5 = p.n4
			LET p.c4 = p.c3
			LET p.n4 = p.n3
			LET p.c3 = p.c2
			LET p.n3 = p.n2
			LET p.c2 = p.c1
			LET p.n2 = p.n1
			LET p.c1 = null        
			LET p.n1 = null        
			FETCH  ABSOLUTE reg c_cursor INTO p.c1, p.n1
			DISPLAY BY NAME p.*
			NEXT FIELD a

		BEFORE FIELD a
			DISPLAY " " TO a
			LET p.a = null
      DISPLAY p.c1 TO c1 ATTRIBUTE(REVERSE)
      DISPLAY p.n1 TO n1 ATTRIBUTE(REVERSE)

		AFTER FIELD a
			LET p.a = null
  		DISPLAY p.c1 TO c1 ATTRIBUTE(NORMAL)
  		DISPLAY p.n1 TO n1 ATTRIBUTE(NORMAL)
			LET l_cod = p.c1
			LET l_nom = p.n1

		BEFORE FIELD b
			DISPLAY " " TO a
			IF p.c2 IS NULL THEN NEXT FIELD a END IF
  		DISPLAY p.c2 TO c2 ATTRIBUTE(REVERSE)
  		DISPLAY p.n2 TO n2 ATTRIBUTE(REVERSE)

		AFTER FIELD b
			LET p.b = null
  		DISPLAY p.c2 TO c2 ATTRIBUTE(NORMAL)
  		DISPLAY p.n2 TO n2 ATTRIBUTE(NORMAL)
			LET l_cod = p.c2
			LET l_nom = p.n2

		BEFORE FIELD c
			LET p.c = null
			DISPLAY " " TO b
			IF p.c3 IS NULL THEN NEXT FIELD b END IF
  		DISPLAY p.c3 TO c3 ATTRIBUTE(REVERSE)
  		DISPLAY p.n3 TO n3 ATTRIBUTE(REVERSE)

		AFTER FIELD c
  		DISPLAY p.c3 TO c3 ATTRIBUTE(NORMAL)
  		DISPLAY p.n3 TO n3 ATTRIBUTE(NORMAL)
			LET l_cod = p.c3
			LET l_nom = p.n3
		BEFORE FIELD d
			LET p.d = null
			DISPLAY " " TO c
			IF p.c4 IS NULL THEN NEXT FIELD c END IF
  		DISPLAY p.c4 TO c4 ATTRIBUTE(REVERSE)
  		DISPLAY p.n4 TO n4 ATTRIBUTE(REVERSE)

		AFTER FIELD d
			DISPLAY " " TO d
  		DISPLAY p.c4 TO c4 ATTRIBUTE(NORMAL)
  		DISPLAY p.n4 TO n4 ATTRIBUTE(NORMAL)
			LET l_cod = p.c4
			LET l_nom = p.n4

		BEFORE FIELD e
			LET p.e = null
			DISPLAY " " TO d
			IF p.c5 IS NULL THEN NEXT FIELD d END IF
  		DISPLAY p.c5 TO c5 ATTRIBUTE(REVERSE)
  		DISPLAY p.n5 TO n5 ATTRIBUTE(REVERSE)

		AFTER FIELD e
  		DISPLAY p.c5 TO c5 ATTRIBUTE(NORMAL)
  		DISPLAY p.n5 TO n5 ATTRIBUTE(NORMAL)
			LET l_cod = p.c5
			LET l_nom = p.n5

		BEFORE FIELD z1
			DISPLAY " " TO e
			LET aux_c = null
			LET aux_n = null 
			LET i = reg + max_c
			FETCH  ABSOLUTE i c_cursor INTO aux_c, aux_n
			IF STATUS = NOTFOUND THEN
				NEXT FIELD e
			END IF
			LET reg = reg + 1
			LET p.c1 = p.c2
			LET p.n1 = p.n2
			LET p.c2 = p.c3
			LET p.n2 = p.n3
			LET p.c3 = p.c4
			LET p.n3 = p.n4
			LET p.c4 = p.c5
			LET p.n4 = p.n5
			LET p.c5 = aux_c
			LET p.n5 = aux_n
			DISPLAY BY NAME p.*
			NEXT FIELD e
	END INPUT
  CLOSE c_cursor

  IF NOT INT_FLAG THEN
	  CALL f0500_imprimir_gb001(l_cod,l_arch)
  END IF
  LET INT_FLAG = FALSE

  CLOSE WINDOW w1_gb001a
END FUNCTION

FUNCTION f0500_imprimir_gb001(l_impr,l_arch)
DEFINE
  l_impr SMALLINT,
  l_term CHAR(50),  # ID de terminal
  #l_arch CHAR(50),  # Archivo de impresion		#@001
  l_arch CHAR(90),   # Archivo de impresion		#@001
  l_argu CHAR(500),  # Argumento RUN
  l1	RECORD LIKE adprn.*,
  l2	RECORD LIKE adtrm.*

	IF l_impr = 1 THEN
		LET l_term = fgl_getenv("TERM")
		SELECT * INTO l2.*
		FROM adtrm
		WHERE adtrmtype = l_term
		IF STATUS = NOTFOUND THEN
			ERROR "IMPRESORA ESCLAVA NO INSTALADA ..."
			RETURN
		END IF
		LET l_term = l_term CLIPPED
		LET l_arch = l_arch CLIPPED
		LET l_argu = "rm ",l_term CLIPPED,".r"
		RUN l_argu
		LET l_argu = "cat /u/bexe/",l2.adtrmcadi CLIPPED," ", l_arch CLIPPED," /u/bexe/",l2.adtrmcadf CLIPPED, " > ",l_term CLIPPED,".r"
		RUN l_argu
		LET l_argu = "cat ",l_term CLIPPED,".r > /dev/tty"
	ELSE
		SELECT * INTO l1.*
		  FROM adprn
		 WHERE adprncprn = l_impr
		IF STATUS = NOTFOUND THEN
			ERROR "IMPRESORA NO INSTALADA ..."
			RETURN
		END IF
		LET l_argu = l1.adprncomm CLIPPED," ",l_arch CLIPPED
	END IF
	DISPLAY "Imprimiendo ...." TO mensaje
	RUN l_argu
END FUNCTION
