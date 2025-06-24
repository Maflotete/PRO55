###############################################################################
# PROGRAMA: cn559.4gl
# VERSION : 4.1.2
# OBJETIVO: Estado de Ganacias y Perdidas
# AUTOR   : Rogger Valle Temoche
# FECHA   : 14/01/2018
# COMPILAR: gb000.4go gb001.4go
###############################################################################
#MODIFICACION
#@004 JDV   02/09/2019 EGP x Analista
#@005 JDV   03/10/2019 EGP x Analista --Sin central --Con central
#@006 CBF   25/02/2020 Unificación de EGP de OI
#@007 JDV   21/05/2021 Cambio en metodología: Gastos x interes total
#@008 JDV   06/07/2021 Cambio en metodología: - GASTOS POR INTERESES CENTRAL
#           - Gastos de Personal y Directorio C.,Gastos por Serv. Recibidos de Terceros C.
#           - Impuestos y Contribuciones Central
#@009 JDV   26/08/2021 Distribuir provisiones por analista origen - Incluir TPP,TR
#@010 JDV   14/09/2021 No considerar cobranzas ni judicial en numero de analistas -GASTOS POR INTERESES CENTRAL
#@011 JDV   20/09/2021 Proceso automático mostrar ingresos finales
#@012 JDV   28/10/2021 Nueva formula ingresos por intereses de agencia
#@013 MCV   18/03/2022 Considerar nuevos filtros al generar reporte y actualizacion de analistas a los prestamos
#@014 MCV   26/04/2022 Cambio de formula para cálculo de ajustes de importe
#@015 JDV   13/10/2022 Mostrar la última sede de analista según fecha de desembolso
#@016 MCV   20/03/2024 Transferencia de cartera de creditos cancelados de un analista retirado
#@017 JDV   05/06/2024 Buscar analistas en mes anterior
#@018 JDV   06/06/2024 Agregar concepto 226,229 - Prefijo:22
#@019 JDV   12/07/2024 Corrección de la resta de 3 meses
#@020 ACP   12/11/2024 Cambio de formula para trimestres sin fondeo
#@021 MCV   06/05/2025 Cambio en formula de transferencia de cartera de creditos cancelados
###############################################################################
DATABASE tbsfi
DEFINE
         t2 RECORD
            AGEN  SMALLINT,
			   NIVE  SMALLINT,
			   NORD  SMALLINT,
			   CRUB  SMALLINT,
			   DRUB  CHAR(80),
			   SALD  DECIMAL(14,2)
         END RECORD,
			g_sede	CHAR(100),	#@001
       	g_sald  DECIMAL(14,2),
         t0      RECORD LIKE gbpmt.*,
         t3      RECORD LIKE gbpmt.*,
         p1 RECORD
            fech  DATE,
            agei  SMALLINT, #@008
            agef  SMALLINT, #@008
            deta  CHAR(1), #@004
            tipo  SMALLINT, #@004
            det1  CHAR(1), #@004
            det2  CHAR(1), #@004
            vouc	CHAR(1),
            unif  CHAR(1) #@005
         END RECORD,
         g_act LIKE cndtr.cndtrimpi,
         g_pas LIKE cndtr.cndtrimpi,
         g_ing LIKE cndtr.cndtrimpi,
         g_egr LIKE cndtr.cndtrimpi,
         g_res LIKE cndtr.cndtrimpi,
         g_cajus LIKE cnplc.cnplccnta,
         g_pos   SMALLINT,
         g_nombplaz CHAR(60),
         g_opc SMALLINT,
         x   CHAR(1),
			g_des1	CHAR(50), #@003
         ######################
         # VARIABLES GLOBALES #
         ######################
         m1 RECORD
            o1 CHAR(1),
            d1 CHAR(25),
            o2 CHAR(1),
            d2 CHAR(25),
            o3 CHAR(1),
            d3 CHAR(25),
            o4 CHAR(1),
            d4 CHAR(25)
         END RECORD,
         g_string CHAR(79),
         g_mone   CHAR(30),
         g_desc   CHAR(30),
         g_ancho  SMALLINT,
         g_col    SMALLINT,
         g_opcion CHAR(01),
         g_spool  CHAR(020),
         g_raya   CHAR(79),
         g_raya1  CHAR(132),
         version  CHAR(008),
         g_flag   SMALLINT,
         g_agen   SMALLINT,
         s1       CHAR(300),
         g_user	CHAR(3),
         g_shtml 	CHAR(10000),
         g_cmon   SMALLINT,
         g_dcto   DECIMAL(14,2),
         l_maxt   DECIMAL(14,2),
         l_maxs   DECIMAL(14,2),
         l_maxd   DECIMAL(14,2),
         g_codi   SMALLINT,
         g_tipo   SMALLINT, #@005
         g_hora   CHAR(8) #@005

MAIN
  DEFER INTERRUPT
  OPTIONS PROMPT LINE 22,
          ERROR LINE 23
  #LET version = "ver.1.0.0"
  LET version = "ver.1.1.0" #@004

  LET g_user = arg_val(1)
  IF NOT f0000_open_database_gb000()
   THEN EXIT PROGRAM
  END IF

  SET ISOLATION TO DIRTY READ #@004
  SET LOCK MODE TO WAIT 2 #@004

  IF NOT f6050_usuario_ea017() THEN
    ERROR " NO EXISTE USUARIO "
    SLEEP 2
    EXIT PROGRAM
  END IF

  #@005 ini
  LET g_tipo = NULL
  LET p1.fech = NULL
  LET g_tipo = arg_val(2) #Reproceso
  LET p1.fech = arg_val(3)
  #@005 ini

  IF (g_tipo IS NULL) THEN #@005
      OPEN FORM ea017_01  FROM "cn559a"
      DISPLAY FORM ea017_01
	     {#@001 ini
	LET g_sede=f0310_nombre_sede_ea017(1)
	DISPLAY g_sede,g_sede TO g_sede,g_sed1}
	#@001 fin
      CALL f6100_cabecera_ea017()
      CALL f6200_carga_menu_ea017()
      CALL f3000_crea_temporal_ea017()
      CALL f002_preparar_sentencias_cn559() #@004
      CALL f0300_proceso_ea017()

   ELSE #@005 ini
      CALL STARTLOG("/u/sfi/spool/cn559.log")
      CALL f3000_crea_temporal_ea017()

      CALL f002_preparar_sentencias_cn559()
      IF (p1.fech IS NULL) THEN
         LET p1.fech = f017_buscar_max_eaccc_cn559()
      END IF

      LET p1.agei = 1 #@009
      LET p1.agef = 99 #@009

      CALL f013_crear_ind_tmp_cn559()
      CALL f008_llenar_eaccc_cn559()
      #72-73-74

      #--Acumulado--#
      LET p1.det2 = "S" #Incluye judicial
      LET p1.tipo = 1 #Acumulado
      CALL f016_detalle_egp_cn559(2)
      LET p1.tipo = 2 #Mensual
      CALL f016_detalle_egp_cn559(2)

      LET p1.det2 = "N"
      LET p1.tipo = 1 #Acumulado
      CALL f016_detalle_egp_cn559(1)
      LET p1.tipo = 2 #Mensual
      CALL f016_detalle_egp_cn559(1)

      CALL f017_altas_egp_cn559(p1.fech)

   END IF #@005 fin

END MAIN

#################
# PROCESO CENTRAL
#################

#@004 ini
FUNCTION f002_preparar_sentencias_cn559()
   #DEFINE   l_sql CHAR(1000) #@013
   DEFINE l_sql CHAR(10000) #@013

   LET l_sql = "DELETE FROM TMP_DATOS"
   PREPARE p00_del_tmp_datos FROM l_sql

   LET l_sql = "DELETE FROM TMP_SALDOS"
   PREPARE p01_del_tmp_saldos FROM l_sql

   LET l_sql = "INSERT INTO TMP_SALDOS",
   " SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)",
   " FROM TMP_CNDTR",
   " WHERE CNDTRTDOC NOT IN(96,55,56)",
   " AND CNDTRAGEN NOT IN(41,99)",
   " GROUP BY 1,2"
   PREPARE p02_ins_tmp_saldos FROM l_sql

   LET l_sql = "INSERT INTO TMP_SALDOS_CENT",
   " SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)",
   " FROM TMP_CNDTR",
   " WHERE CNDTRTDOC IN(55)",
   " AND CNDTRAGEN NOT IN(41,99)",
   " GROUP BY 1,2"
   PREPARE p04_ins_tmp_saldos_cent FROM l_sql

   LET l_sql = "INSERT INTO TMP_SALDOS_AG",
   " SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)",
   " FROM TMP_CNDTR",
   " WHERE CNDTRTDOC NOT IN(96)",
   " GROUP BY 1,2"
   PREPARE p06_ins_tmp_saldos_ag FROM l_sql

   LET l_sql = "INSERT INTO TMP_SALDOS_COBR",
   " SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)",
   " FROM TMP_CNDTR",
   " WHERE CNDTRTDOC IN(56)",
   " AND CNDTRAGEN NOT IN(41,99)",
   " AND CNDTRTDOC NOT IN(96)",
   " GROUP BY 1,2"
   PREPARE p08_ins_tmp_saldos_cobr FROM l_sql

   LET l_sql = "INSERT INTO TMP_SALDOS_COBR",
   " SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)",
   " FROM CNDTR",
   " WHERE CNDTRFDOC BETWEEN ? AND ?",
   " AND CNDTRTDOC IN(56)",
   " AND CNDTRAGEN NOT IN(41,99)",
   " AND CNDTRTDOC NOT IN(96)",
   " AND CNDTRMRCB = 0",
   " GROUP BY 1,2"
   PREPARE p09_ins_tmp_saldos_mens_cobr FROM l_sql

   LET l_sql = "SELECT GBOFINOFI",
   " FROM GBOFI",
   " WHERE gbofinofi BETWEEN ? AND ?" #@008
   PREPARE p10_sel_gbofi FROM l_sql
   DECLARE q10_sel_gbofi CURSOR FOR p10_sel_gbofi

   LET l_sql = "DELETE FROM tmp_eaccc"
   PREPARE p11_del_tmp_eaccc FROM l_sql

   LET l_sql = "INSERT INTO tmp_eaccc",
   " SELECT eacccfcie,eacccagen,eacccnpre,eacccsals,",
   " eaccccana,eacccesta,eacccpdvg,eacccpros,eacccpcls,eacccfcas,",
   " (pow((1+(cast(eaccctasa as decimal(16,2))/100)),12) - 1) * 100, eacccdatr", #@009
   " FROM eaccc",
   " WHERE eacccfcie = ?",
   " AND eacccagen between ? AND ?" #@008
   PREPARE p12_ins_tmp_eaccc FROM l_sql

   #@013 Inicio
   LET l_sql= " INSERT into tmp_nprx",
   " SELECT b.npre, a.cana " ,
   " FROM tmp_eaccc a,tmp_eaccc b",
   " WHERE a.fcie = ?",
   " and b.fcie = ?",
   " and a.npre = b.npre",
   " and a.cana != b.cana"
   PREPARE p12_ins_tmp_nprx FROM l_sql

   LET l_sql = " UPDATE tmp_eaccc b" ,
   " SET b.cana = (SELECT distinct a.cana",
 	" FROM tmp_nprx a",
 	" WHERE a.npre = b.npre)",
   " WHERE b.fcie = ?",
   " AND b.npre in (select npre from tmp_nprx)"
   PREPARE p12_upd_tmp_eaccc FROM l_sql

   LET l_sql= " INSERT into tmp_nprx",
   " SELECT b.eacdcnpre, a.eacdccana " ,
   " FROM tmp_eacdc a,tmp_eacdc b",
   " WHERE a.eacdcfreg = ?",
   " and b.eacdcfreg = ?",
   " and a.eacdcnpre = b.eacdcnpre",
   " and a.eacdccana != b.eacdccana"
   PREPARE p12_ins_tmp_eacdc_nprx FROM l_sql

   LET l_sql = " UPDATE tmp_eacdc b" ,
   " SET b.eacdccana = (SELECT distinct a.cana",
 	" FROM tmp_nprx a",
 	" WHERE a.npre = b.eacdcnpre)",
   " WHERE b.eacdcfreg = ?",
   " AND b.eacdcnpre in (select npre from tmp_nprx)"
   PREPARE p12_upd_tmp_eacdc_01 FROM l_sql

   LET l_sql = "DELETE FROM tmp_nprx"
   PREPARE p00_del_tmp_nprx FROM l_sql

      LET l_sql = "DELETE FROM tmp_eacdc"
   PREPARE p00_del_tmp_eacdc FROM l_sql

   LET l_sql = "insert into tmp_nprx",
   " select eacdcnpre, pcmpcrseg from pcmpc, tmp_eacdc",
   " where eacdcnpre = pcmpcnpre",
   " and pcmpcstat = 9",
   " and pcmpcfsta between ? and ?",
   " and eacdcfreg = ?",
   " and eacdccana <> pcmpcrseg",
   " and pcmpcrseg <> 9999"
   PREPARE p12_ins_tmp_nprx_pcmpc FROM l_sql

   LET l_sql = "update tmp_eacdc b",
   " set b.eacdccana =  (SELECT a.cana FROM tmp_nprx a WHERE a.npre = b.eacdcnpre)",
   " where b.eacdcnpre in (select npre from tmp_nprx)"
   PREPARE p12_upd_tmp_eacdc FROM l_sql


   LET l_sql = "DELETE from tmp_rseg ",
   " WHERE rseg not in (select distinct eaccccana from eaccc where eacccfcie = ?)",
   " AND rseg <> -1"
   PREPARE p00_del_tmp_part_rseg FROM l_sql

   LET l_sql = "INSERT INTO tmp_eacdc",
   " SELECT eacdcfreg, eacdcagen, eacdccana, eacdcnpre, NVL(sum(eacdcpdvg),0)",
   " FROM eacdc",
   " WHERE eacdcfreg = ?",
   " AND eacdccest in (3,5,6)",
   " group by 1,2,3,4 "
   PREPARE p16_ins_tmp_eacdc FROM l_sql

      LET l_sql = " INSERT into tmp_nprx ",
   " SELECT b.eacdcnpre, a.cana ",
   " FROM tmp_pctcn a, tmp_eacdc b",
   " WHERE a.tipo = 1",
   " and a.npre = b.eacdcnpre",
   " and a.cana != b.eacdccana",
   " and a.cana <> 9999"
   PREPARE p19_ins_tmp_pctcn_nprx FROM l_sql

   LET l_sql = " INSERT into tmp_nprx ",
   " SELECT b.eacdcnpre, a.cana ",
   " FROM tmp_pctcn a, tmp_eacdc b",
   " WHERE a.tipo = 1",
   " and a.npre = b.eacdcnpre",
   " and a.cana != b.eacdccana"
   PREPARE p19_ins_tmp_pctcn_nprx_02 FROM l_sql

   LET l_sql = " update tmp_pctcn",
   " set cana = (select distinct eacdccana  from tmp_eacdc where eacdcnpre = npre )",
   " where tipo = 1",
   " and npre in (select a.npre from tmp_nprx a)"
   PREPARE p19_ins_tmp_pctcn_tpo_1 FROM l_sql

   LET l_sql = " update tmp_pctcn",
   " set cana = (select distinct eacdccana  from tmp_eacdc where eacdcnpre = npre )",
   " where tipo = 1",
   " and npre not in (select a.npre from tmp_nprx a)"
   PREPARE p19_ins_tmp_pctcn_tpo_1_02 FROM l_sql

   LET l_sql = " INSERT into tmp_nprx",
   " SELECT b.npre, a.cana ",
   " FROM tmp_pctcn a,tmp_pctcn b",
   " WHERE a.tipo = 2",
   " and b.tipo = 1",
   " and a.npre = b.npre",
   " and a.cana != b.cana"
   PREPARE p19_ins_tmp_pctcn_nprx_03 FROM l_sql

   LET l_sql = "  UPDATE tmp_pctcn b",
   " SET b.cana = (SELECT distinct a.cana",
   " FROM tmp_nprx a",
   " WHERE a.npre = b.npre)",
   " WHERE b.tipo = 1",
   " AND b.npre in (select npre from tmp_nprx)"
   PREPARE p19_ins_tmp_pctcn_tpo_1_03 FROM l_sql

   LET l_sql = "  UPDATE tmp_pctcn",
   " SET cana = (SELECT eacdccana",
   " FROM tmp_eacdc",
   " WHERE eacdcnpre = npre AND eacdcfreg = ?)",
   " WHERE tipo = 1"
   PREPARE p19_ins_tmp_pctcn_tpo_1_04 FROM l_sql

   LET l_sql = "  UPDATE tmp_pctcn",
   " SET cana = (SELECT pcmpcrseg",
   " FROM pcmpc",
   " WHERE pcmpcnpre = npre)",
   " WHERE tipo = 1",
   " AND cana is null"
   PREPARE p19_ins_tmp_pctcn_tpo_1_05 FROM l_sql
    #@013 Fin

   LET l_sql = "SELECT COUNT(DISTINCT cana)",
   " FROM tmp_eaccc",
   " WHERE fcie = ?",
   " AND agen = ?",
   " AND esta !='CASTIGADO'", #---Falta quitar el saldo de administrador
   " AND cana > -1"
   PREPARE p13_sel_cont_tmp_eaccc FROM l_sql

   #@010 ini
   LET l_sql = "SELECT COUNT(DISTINCT cana)",
   " FROM tmp_eaccc",
   " WHERE fcie = ?",
   " AND agen = ?",
   " AND esta !='CASTIGADO'",
   " AND cana NOT IN (6666,9999)",
   " AND cana > -1"
   PREPARE p13_sel_cont_tmp_eaccc_sin_cob FROM l_sql
   #@010 fin

   LET l_sql = "DELETE FROM tmp_rseg"
   PREPARE p14_del_tmp_rseg FROM l_sql

   LET l_sql = "INSERT INTO tmp_rseg",
   " SELECT distinct agen,cana",
   " from tmp_eaccc",
   " where fcie = ?",
   " and esta !='CASTIGADO'"
   PREPARE p15_ins_tmp_rseg FROM l_sql

   LET l_sql = " select distinct rseg",
   " from tmp_rseg",
   " where agen = ?",
   " order by 1"
   PREPARE p14_cana_tmp_eaccc FROM l_sql
   DECLARE q14_cana_tmp_eaccc CURSOR FOR p14_cana_tmp_eaccc

   LET l_sql = "select gbfirnomb",
   " from gbfir",
   " where gbfircfun = ?"
   PREPARE p14_sel_gbfir FROM l_sql

   LET l_sql = "SELECT DISTINCT CODI,DRUB",
   " FROM TMP_DATOS",
   " WHERE CODI IN (",
   " select pcprmvalo",
   " from pcprm",
   " where pcprmflag = 555",
   " and pcprmdato = ?)",
   " UNION",
   " select CAST(pcprmvalo AS SMALLINT),pcprmdesc",
   " from pcprm",
   " where pcprmflag = 555",
   " and pcprmdato = ?",
   " and pcprmvalo = 64", #@005
   " ORDER BY 1"
   PREPARE p14_sel_rubr_det FROM l_sql
   DECLARE q14_sel_rubr_det CURSOR FOR p14_sel_rubr_det

   LET l_sql = "DELETE FROM TMP_DATOS_CANA"
   PREPARE p15_sel_tmp_datos_cana FROM l_sql

   LET l_sql = "INSERT INTO TMP_DATOS_CANA",
   " VALUES (?,?,?,?)"
   PREPARE p16_ins_tmp_datos_cana FROM l_sql

   LET l_sql = "select NVL(sum(pdvg),0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and agen = ?",
   " and cana = ?",
   " and esta!='CASTIGADO'"
   PREPARE p17_sel_pdvg FROM l_sql

   #@008 ini
   LET l_sql = "select NVL(sum(eacdcpdvg),0)",
   #" FROM eacdc", #@013
   " FROM tmp_eacdc",  #@013
   " WHERE eacdcfreg = ?",
   " AND eacdcagen = ?",
   " AND eacdccana = ?"
   #" AND eacdccest in (3,5,6)" #@013
   PREPARE p17_sel_eacdc_pdvg FROM l_sql
   #@008 fin

   LET l_sql = "DELETE FROM tmp_pctcn"
   PREPARE p18_del_tmp_pctcn FROM l_sql

   {LET l_sql = "INSERT INTO tmp_pctcn",
   " select pctcnagen,pctcnnpre,0,SUM(pctcnimpi) * -1",
   " from pctcn",
   " where pctcnftra between ? and ?",
   " and pctcncctb[1,4] IN(",
   " select pcprmvalo",
   " from pcprm",
   " where pcprmflag = 555",
   " and pcprmdato = 1)",
   " group by 1,2,3"}
  LET l_sql = "INSERT INTO tmp_pctcn",
  " select 1,pctcncctb,pctcnagen,pctcnnpre,pcmpcrseg,SUM(pctcnimpi) * -1",
   " from pctcn,pcmpc,efpar",
   " where pctcnftra between ? and ?",
   " and pctcnnpre = pcmpcnpre",
   " and efparpfij = 92",
   " and (",
   " case",
   " WHEN efpardes3='4' THEN pctcncctb[1,4]",
   " WHEN efpardes3='6' THEN pctcncctb[1,6]",
   " WHEN efpardes3='8' THEN pctcncctb[1,8]",
   " WHEN efpardes3='10' THEN pctcncctb[1,10]",
   " else pctcncctb end)= trim(efpardes2)",
   " and efparcorr = 8",
   " and efparint1 = 1", #@018
   " and pctcnpost = 9",
   " and efparmrcb = 0",
   " and pcmpcmrcb = 0",
   " group by 1,2,3,4,5"
   PREPARE p19_ins_tmp_pctcn FROM l_sql

   #@018 ini
   LET l_sql = "INSERT INTO tmp_pctcn",
   " select 1,pctcncctb,pctcnagen,pctcnnpre,pcmpcrseg,SUM(pctcnimpi) * -1",
    " from pctcn,pcmpc,efpar",
    " where pctcnftra between ? and ?",
    " and pctcnnpre = pcmpcnpre",
    " and efparpfij = 92",
    " and efparcorr = 8",
    " and efparint1 = 2",
    " and efparint2 = pctcnpref",
    " and trim(efpardes2)=pctcnccon",
    " and trim(efpardes3)=pctcncctb[1,8]",
    " and pctcnpost = 9",
    " and efparmrcb = 0",
    " and pcmpcmrcb = 0",
    " group by 1,2,3,4,5"
    PREPARE p19_ins_tmp_pctcn_pref FROM l_sql
   #@018 fin

   LET l_sql = "UPDATE tmp_pctcn",
   " SET cana = ?",
   " WHERE npre = ?"
   PREPARE p22_upd_tmp_pctcn FROM l_sql

   LET l_sql = "SELECT NVL(SUM(impi),0)",
   " FROM tmp_pctcn",
   " WHERE agen = ?",
   " AND cana = ?",
   " AND tipo = ?" #@012
   PREPARE p13_sel_impi_tmp_pctcn FROM l_sql

   LET l_sql = "SELECT NVL(SUM(SALT),0)",
   " FROM TMP_DATOS_CANA",
   " WHERE CODI = ?",
   " AND AGEN = ?",
   " AND CANA = ?"
   PREPARE p14_sel_salt_cana FROM l_sql

   LET l_sql = "CREATE index tmp_eaccc_01 ON tmp_eaccc(fcie,npre) using btree";
   PREPARE p15_indx01_tmp_eaccc FROM l_sql

   LET l_sql = "CREATE index tmp_eaccc_02 ON tmp_eaccc(fcie,agen,cana) using btree";
   PREPARE p16_indx02_tmp_eaccc FROM l_sql

   LET l_sql = "UPDATE tmp_pctcn",
   " SET cana = (SELECT cana FROM tmp_eaccc",
   " WHERE fcie = ? AND tmp_eaccc.npre = tmp_pctcn.npre)",
   " WHERE cana = 0"
   PREPARE p17_upt_tmp_pctcn FROM l_sql

   LET l_sql = "UPDATE tmp_pctcn",
   " SET cana = 0",
   " WHERE cana IS NULL"
   PREPARE p18_upt_tmp_pctcn FROM l_sql

   LET l_sql = "select NVL(SUM(sals),0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and agen = ?",
   #" and tipo IS NOT NULL",
   " and esta !='CASTIGADO'"
   PREPARE p19_sel_sals_agen FROM l_sql

   LET l_sql = "select NVL(SUM(sals),0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and agen = ?",
   " and cana = ?",
   " and esta !='CASTIGADO'"
   PREPARE p20_sel_sals_cana FROM l_sql

   LET l_sql = "DELETE FROM tmp_sudli"
   PREPARE p21_del_tmp_sudli FROM l_sql

   LET l_sql = "insert into tmp_sudli",
   " select suliqcemp,suliquneg,rseg,(nvl(suliqsuel,0) + ",
   " ( select nvl(sum(sudliimpt),0)",
   " from tbsai:sudli",
   " where sudlicemp = suliqcemp",
   " and sudlifech = suliqfech",
   " and sudlitcon = 13",
   " and sudlicorr = 1))* ?",
   " from tbsai:suliq,tmp_codia",
   " where suliqcemp = cemp",
   " and suliqfech = ?",
   " and tipo = 1"
   PREPARE p22_ins_tmp_sudli FROM l_sql

   LET l_sql = "SELECT NVL(SUM(impt),0)",
   " FROM tmp_sudli",
   " WHERE uneg = ?",
   " AND rseg IN (select distinct cana from tmp_eaccc where fcie=? and agen=?)"
   PREPARE p23_sel_tmp_sudli_uneg FROM l_sql

   LET l_sql = "SELECT NVL(SUM(impt),0)",
   " FROM tmp_sudli",
   " WHERE uneg = ?",
   " AND rseg = ?"
   PREPARE p24_sel_tmp_sudli_rseg FROM l_sql

   LET l_sql = "select NVL(SUM(sals),0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and agen = ?",
   #" and tipo IS NOT NULL",
   " and esta in ('VENCIDO 2','EJECUCION')"
   PREPARE p25_sel_sven_agen FROM l_sql

   LET l_sql = "select NVL(SUM(sals),0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and agen = ?",
   " and cana = ?",
   " and esta in ('VENCIDO 2','EJECUCION')"
   PREPARE p26_sel_sven_cana FROM l_sql

   LET l_sql = "DELETE FROM tmp_codia"
   PREPARE p27_del_tmp_codia FROM l_sql

   LET l_sql = "INSERT INTO tmp_codia",
   " SELECT 1,pcfirrseg,suliqcemp",
   " FROM pcfir,aduns,tbsai:suliq",
   " WHERE pcfiruser = adunsusrn",
   " and adunscemp = suliqcemp",
   " and suliqfech = ?",
   " and suliqcarg IN (",
   " select pcprmvalo",
   " from pcprm",
   " where pcprmflag = 555",
   " and pcprmdato = 3",
   " ) "
   PREPARE p28_ins_tmp_codia_1 FROM l_sql

   LET l_sql = "INSERT INTO tmp_codia",
   " SELECT 0,pcfirrseg,suliqcemp",
   " FROM pcfir,aduns,tbsai:suliq",
   " WHERE pcfiruser = adunsusrn",
   " and adunscemp = suliqcemp",
   " and suliqfech = ?",
   " and suliqcarg IN (",
   " select pcprmvalo",
   " from pcprm",
   " where pcprmflag = 555",
   " and pcprmdato = 3",
   " ) "
   PREPARE p28_ins_tmp_codia_0 FROM l_sql

   LET l_sql = "DELETE FROM tb_agenc"
   PREPARE p29_del_tb_agenc FROM l_sql

   #----CREANDO INDICES---#
   LET l_sql = "CREATE INDEX TMP_SALDOS_01 ON TMP_SALDOS(CNTA,AGEN);"
   PREPARE p30_indx_tmp_saldos FROM l_sql

   LET l_sql = "CREATE INDEX TMP_SALDOS_CEN_01 ON TMP_SALDOS_CENT(CNTA,AGEN);"
   PREPARE p31_indx_tmp_saldos_cent FROM l_sql

   LET l_sql = "CREATE INDEX TMP_SALDOS_AG_01 ON TMP_SALDOS_AG(CNTA,AGEN);"
   PREPARE p32_indx_tmp_saldos_ag FROM l_sql

   LET l_sql = "CREATE INDEX TMP_SALDOS_CB_01 ON TMP_SALDOS_COBR(CNTA,AGEN);"
   PREPARE p33_indx_tmp_saldos_cobr FROM l_sql

   LET l_sql = "SELECT pcprmdato,pcprmvalo",
   " FROM pcprm",
   " WHERE pcprmflag = 248",
   " ORDER BY 2"
   PREPARE p34_sel_pcprm FROM l_sql
   DECLARE q34_sel_pcprm CURSOR FOR p34_sel_pcprm

   LET l_sql = "UPDATE tmp_datos",
   " SET AGEN = ?",
   " WHERE AGEN = ?"
   PREPARE p35_upd_agen_tmp_datos FROM l_sql

   LET l_sql = "DELETE FROM tmp_varp"
   PREPARE p36_del_tmp_varp FROM l_sql

   #@008 ini
   {LET l_sql ="create index tmp_varp_01 on tmp_varp(npre);"
   PREPARE p37_ind_tmp_varp_01 FROM l_sql

   LET l_sql = "create index tmp_varp_02 on tmp_varp(fcie);"
   PREPARE p38_ind_tmp_varp_02 FROM l_sql

   LET l_sql = "insert into tmp_varp(fcie,agen,npre,cana,tpov)",
   " select fcie,agen,npre,cana,nvl(pros,0)+nvl(pcls,0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and esta!='CASTIGADO';"}
   LET l_sql = "insert into tmp_varp",
   " select pcpcnagen,pcpcnnpre,nvl(cana,pcmpcrseg),sum(pcpcnmont)",
   " from pcpcn,pcmpc,efpar,outer tmp_eaccc",
   " where pcpcnfcie = fcie",
   " and pcpcnnpre = pcmpcnpre",
   " and pcpcnnpre = npre",
   " and pcpcnfcie = ?",
   " and efparpfij = 92",
   " and (",
   " case",
   " WHEN efpardes3='4' THEN pcpcncnta[1,4]",
   " WHEN efpardes3='6' THEN pcpcncnta[1,6]",
   " WHEN efpardes3='8' THEN pcpcncnta[1,8]",
   " WHEN efpardes3='10' THEN pcpcncnta[1,10]",
   " else pcpcncnta end)= trim(efpardes2)",
   " and efparcorr = 7",
   " and pcpcnmrcb = 0",
   " and efparmrcb = 0",
   " group by 1,2,3"
   #@008 fin
   PREPARE p39_ins_tmp_varp FROM l_sql

   {#@008 ini
   LET l_sql = "DELETE FROM tmp_vari"
   PREPARE p40_del_tmp_vari FROM l_sql

   LET l_sql = "insert into tmp_vari",
   " select a.npre npre,(a.tpov - b.tpov) vari",
   " from tmp_varp a, tmp_varp b",
   " where a.npre = b.npre",
   " and a.fcie = ?",
   " and b.fcie = ?"
   PREPARE p41_ins_tmp_vari FROM l_sql

   LET l_sql = "UPDATE tmp_varp",
   " SET texp=1",
   " WHERE npre in (",
   " select npre from tmp_vari)"
   PREPARE p42_upd_tmp_varp_1 FROM l_sql

   LET l_sql = "UPDATE tmp_varp",
   " SET vari=(select vari from tmp_vari b",
   " where b.npre = tmp_varp.npre)",
   " WHERE fcie = ?;"
   PREPARE p43_upd_tmp_varp_prov FROM l_sql

   LET l_sql = "update tmp_varp",
   " set texp = ?,",
   " vari = tpov",
   " where fcie = ?",
   " and texp is null;"
   PREPARE p44_upd_tmp_varp_tipo FROM l_sql
    }#@008

   LET l_sql = "SELECT nvl(sum(sals),0)",
   " FROM tmp_eaccc",
   " WHERE fcie=?",
   " AND fcas=?",
   " AND agen=?",
   " AND cana=?",
   " AND esta='CASTIGADO'"
   PREPARE p45_sel_tmp_eaccc_cast FROM l_sql

   {#@008 ini
   LET l_sql = "select nvl(sum(vari),0)",
   " from tmp_varp",
   " where agen = ?",
   " and cana = ?",
   " and texp in (?,?);"
    }
    LET l_sql = "select nvl(sum(impo),0)",
    " from tmp_varp",
    " where agen = ?",
    " and cana = ?"
   PREPARE p46_sel_tmp_varp_varp FROM l_sql

   LET l_sql = "drop index tmp_eaccc_01"
   PREPARE p47_drop_ind_tmp_eaccc_01 FROM l_sql

   LET l_sql = "drop index tmp_eaccc_02"
   PREPARE p48_drop_ind_tmp_eaccc_02 FROM l_sql

   LET l_sql = "drop index tmp_saldos_01"
   PREPARE p49_drop_ind_tmp_saldos_01 FROM l_sql

   LET l_sql = "drop index tmp_saldos_cen_01"
   PREPARE p50_drop_ind_tmp_saldos_cen_01 FROM l_sql

   LET l_sql = "drop index tmp_saldos_ag_01"
   PREPARE p51_drop_ind_tmp_saldos_ag_01 FROM l_sql

   LET l_sql = "drop index tmp_saldos_cb_01"
   PREPARE p52_drop_ind_tmp_saldos_cb_01 FROM l_sql

   {#@008 ini
   LET l_sql = "drop index tmp_varp_01"
   PREPARE p53_drop_ind_tmp_varp_01 FROM l_sql

   LET l_sql = "drop index tmp_varp_02"
   PREPARE p54_drop_ind_tmp_varp_02 FROM l_sql
   }#@008 fin

   LET l_sql = "select cast (pcprmvalo as decimal(14,2))",
   " from pcprm",
   " where pcprmflag = 555",
   " and pcprmdato = 4"
   PREPARE p55_sel_param_sueld FROM l_sql

   {#@008 ini
   LET l_sql = "select nvl(sum(vari),0)",
   " from tmp_varp",
   " where agen = ?",
   " and texp in (?,?);"
   }
   LET l_sql = "select nvl(sum(a.impo),0)",
   " from tmp_varp a,tmp_rseg b",
   " where a.agen = b.agen",
   " and a.cana = b.rseg",
   " and a.agen = ?"
   #@008 fin
   PREPARE p56_sel_tmp_varp_agen FROM l_sql

   LET l_sql = "SELECT nvl(sum(sals),0)",
   " FROM tmp_eaccc",
   " WHERE fcie=?",
   " AND fcas=?",
   " AND agen=?",
   " AND esta='CASTIGADO'"
   PREPARE p57_sel_tmp_eaccc_cast FROM l_sql

   {#@008 ini
   LET l_sql = "DELETE FROM tmp_varp",
   " WHERE fcie =? ",
   " AND cana NOT IN (SELECT rseg FROM tmp_codia",
   " WHERE tipo = ?)"
   PREPARE p58_del_tmp_varp FROM l_sql
    } #@008 fin

   LET l_sql = "select pcprmdesc",
   " from pcprm",
   " where pcprmflag = ?",
   " and pcprmdato = ?",
   " and pcprmvalo = ?"
   PREPARE p59_sel_desc_pcprm FROM l_sql

   LET l_sql = "SELECT NVL(SUM(SALT),0)",
   " FROM TMP_DATOS_CANA",
   " WHERE CODI = ?",
   " AND AGEN = ?"
   PREPARE p60_sel_salt_agen FROM l_sql

   LET l_sql = "select pcfirrseg",
   " from tbsai:suliq,aduns,pcfir",
   " where suliqfech = ?",
   " and suliqcemp = adunscemp",
   " and adunsusrn = pcfiruser",
   #@007 ini
   #" and suliqcarg in (17,65,92,109)"
   " and suliqcarg IN (",
   " select pcprmvalo",
   " from pcprm where pcprmflag = 555",
   " and pcprmdato = 10)"
   #@007 fin
   PREPARE p61_sel_rseg FROM l_sql
   DECLARE q61_sel_rseg CURSOR FOR p61_sel_rseg

   LET l_sql = "DELETE FROM tmp_eaccc",
   " where fcie = ?",
   " and cana = ?"
   PREPARE p62_del_rseg_eaccc FROM l_sql

   LET l_sql = "DELETE FROM TMP_SALDOS_CENT"
   PREPARE p63_del_tmp_saldos_cent FROM l_sql

   LET l_sql = "DELETE FROM TMP_SALDOS_AG"
   PREPARE p64_del_tmp_saldos_ag FROM l_sql

   LET l_sql = "DELETE FROM TMP_SALDOS_COBR"
   PREPARE p65_del_tmp_saldos_cobr FROM l_sql

   LET l_sql = "DELETE FROM tmp_cndtr"
   PREPARE p66_del_tmp_cndtr FROM l_sql

   LET l_sql = "INSERT INTO tmp_cndtr",
   " SELECT CNDTRCNTA,CNDTRAGEN,CNDTRTDOC,NVL(SUM(CNDTRIMPI),0)",
   " FROM CNDTR",
   " WHERE CNDTRFDOC <= ?",
   " AND CNDTRAGEN BETWEEN ? AND ?", #@008
   " AND CNDTRMRCB = 0",
   " GROUP BY 1,2,3"
   PREPARE p67_ins_tmp_cndtr_acum FROM l_sql

   LET l_sql = "INSERT INTO tmp_cndtr",
   " SELECT CNDTRCNTA,CNDTRAGEN,CNDTRTDOC,NVL(SUM(CNDTRIMPI),0)",
   " FROM CNDTR",
   " WHERE CNDTRFDOC BETWEEN ? AND ?",
   " AND CNDTRAGEN BETWEEN ? AND ?", #@008
   " AND CNDTRMRCB = 0",
   " GROUP BY 1,2,3"
   PREPARE p68_ins_tmp_cndtr_mens FROM l_sql

   LET l_sql = "CREATE INDEX tmp_cndtr_001 ON tmp_cndtr(CNDTRTDOC,CNDTRAGEN)"
   PREPARE p69_ind_tmp_cndtr FROM l_sql

   LET l_sql = "DROP INDEX tmp_cndtr_001"
   PREPARE p70_drop_ind_tmp_cndtr FROM l_sql

   #---PREPARE DE BUSQUEDA DE SALDOS---#
   LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
   " FROM TMP_SALDOS ",
   " WHERE CNTA MATCHES ?",
   " AND AGEN = ?"
   PREPARE s_saldo FROM l_SQL

   LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
   " FROM TMP_SALDOS_CENT ",
   " WHERE CNTA MATCHES ?",
   " AND AGEN = ?"
   PREPARE s_saldo_d FROM l_SQL

   LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
   " FROM TMP_SALDOS_COBR ",
   " WHERE CNTA MATCHES ?",
   " AND AGEN = ?"
   PREPARE s_saldo_c FROM l_SQL

   LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
   " FROM TMP_SALDOS_AG ",
   " WHERE CNTA MATCHES ?",
   " AND AGEN = ?"
   PREPARE s_saldo_g FROM l_SQL

   #---Ajuste de ingresos por provision--#
   LET l_SQL ="DELETE FROM tmp_cobr"
   PREPARE p71_del_tmp_cobr FROM l_sql

   LET l_SQL = "INSERT INTO tmp_cobr",
   " SELECT agen,sum(sals)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and cana = '9999'",
   " and agen NOT IN (19,98)", #--UNIDAD
   " and esta !='CASTIGADO'",
   " group by 1"
   PREPARE p72_ins_tmp_cobr FROM l_sql

   LET l_sql = "DELETE FROM tmp_ingf"
   #PREPARE p73_sel_pdvg FROM l_sql
   PREPARE p73_del_tmp_ingf FROM l_sql

   {LET l_sql = " SELECT distinct agen,cana",
   " from tmp_eaccc",
   " where fcie = ?",
   " and esta !='CASTIGADO'",
   " union",
   " select distinct agen,cana",
   " from tmp_eaccc",
   " where fcie = ?",
   " and esta !='CASTIGADO'"}
   #@007 ini
   LET l_sql = "SELECT distinct agen,cana",
   " FROM tmp_rseg_ini"
   #@007 fin
   PREPARE p74_sel_tot_rseg FROM l_sql
   DECLARE q74_sel_tot_rseg CURSOR FOR p74_sel_tot_rseg

   LET l_sql = "INSERT INTO tmp_ingf VALUES",
   #" (?,?,?,?,?,?,?,?)" #@013 --@012 --@008
   " (?,?,?,?,?,?,?,?,?)"  #@013
   PREPARE p75_ins_tmp_ingf FROM l_sql

   LET l_sql = "SELECT NVL(SUM(ingf),0)",
   " FROM tmp_ingf",
   " WHERE agen = ?",
   " AND cana = ?",
   " AND cana > -1" #@009
   PREPARE p76_sel_ingm FROM l_sql

   LET l_sql = "SELECT cana,nvl(ingf,0)", #@011
   " FROM tmp_ingf",
   " WHERE agen = ?",
   " AND cana NOT IN (SELECT rseg FROM tmp_rseg WHERE agen=?)",
   " AND ingf > 0"
   PREPARE p77_sel_ingf_no_cons FROM l_sql
   DECLARE q77_sel_ingf_no_cons CURSOR FOR p77_sel_ingf_no_cons

   LET l_sql = "SELECT NVL(SUM(sals),0)",
   " FROM tmp_cobr"
   PREPARE p78_sel_tot_cob FROM l_sql

   LET l_sql = "SELECT NVL(SUM(sals),0)",
   " FROM tmp_cobr",
   " WHERE agen = ?"
   PREPARE p79_sel_tot_agen FROM l_sql

   LET l_sql = "INSERT INTO TMP_DATOS VALUES",
   " (?,?,?,?,?,?)"
   PREPARE p80_ins_tmp_datos FROM l_sql

   LET l_sql = "SELECT NVL(SUM(SALT),0)",
   " FROM TMP_DATOS",
   " WHERE CODI = ?",
   " AND AGEN = ?"
   PREPARE p81_sel_tmp_datos_salt FROM l_sql

   LET l_sql = "SELECT NVL(SUM(ingf),0), COUNT(DISTINCT cana)",
   " FROM tmp_ingf",
   " WHERE agen = ?",
   " AND cana IN (SELECT rseg FROM tmp_rseg WHERE agen=? and cana>-1)" #@009
   PREPARE p82_sel_ingf_tot FROM l_sql

   #@005 ini
   LET l_sql = "DELETE FROM tmp_cnegs",
   " WHERE tipo = ?"
   PREPARE p83_del_tmp_cnegs FROM l_sql

   LET l_sql = "INSERT INTO tmp_cnegs(age1,cod1,sal1)",
   " SELECT AGEN,CODI,NVL(SUM(SALT),0)",
   " FROM TMP_DATOS",
   " GROUP BY 1,2"
   PREPARE p84_ins_tmp_cnegs FROM l_sql

   LET l_sql = "UPDATE tmp_cnegs",
   " SET tipo = ?",
   " WHERE tipo IS NULL"
   PREPARE p85_upd_tmp_cnegs FROM l_sql

   LET l_sql = "SELECT age1,cod1",
   " FROM tmp_cnegs",
   " WHERE tipo = ?"
   PREPARE p86_sel_tmp_cnegs FROM l_sql
   DECLARE q86_sel_tmp_cnegs CURSOR FOR p86_sel_tmp_cnegs

   LET l_sql = "UPDATE tmp_cnegs",
   " SET sal2 = ?",
   " WHERE tipo = ?",
   " AND age1 = ?",
   " AND cod1 = ?"
   PREPARE p86_upd_tmp_cnegs_02 FROM l_sql

   LET l_sql = "DELETE FROM tmp_cnega",
   " WHERE tipo = ?",
   " AND subt = ?"
   PREPARE p87_del_tmp_cnega FROM l_sql

   LET l_sql = "INSERT INTO tmp_cnega (age1,rse1,cod1,sal1)",
   " SELECT AGEN,CANA,CODI,SUM(SALT)", #@008
   " FROM TMP_DATOS_CANA",
   " GROUP BY 1,2,3" #@008
   PREPARE p88_ins_tmp_cnega FROM l_sql

   LET l_sql = "UPDATE tmp_cnega",
   " SET tipo = ?,",
   " subt = ?",
   " WHERE tipo IS NULL",
   " AND subt IS NULL"
   PREPARE p89_upd_tmp_cnega FROM l_sql

   LET l_sql = "UPDATE tmp_cnega",
   " SET sal2 = (",
   "   SELECT SUM(SALT)", #@008
   "   FROM TMP_DATOS_CANA",
   "   WHERE AGEN = age1",
   "   AND CANA = rse1",
   "   AND CODI = cod1",
   "   )",
   " WHERE tipo = ?",
   " AND subt = ?"
   PREPARE p90_upd_tmp_cnega_02 FROM l_sql

   LET l_sql = "INSERT INTO cnheg",
   " VALUES (0,?,0,?,?,?)"
   PREPARE p91_ins_cnheg FROM l_sql

   LET l_sql = "UPDATE tmp_cnegs",
   " SET ntra = ?"
   PREPARE p92_upd_tmp_cnegs FROM l_sql

   LET l_sql = "INSERT INTO cnegs",
   " SELECT ntra,tipo,age1,cod1,sal1,sal2",
   " FROM tmp_cnegs",
   " WHERE (sal1!=0 or sal2!=0)" #@011
   PREPARE p93_ins_cnegs FROM l_sql

   LET l_sql = "UPDATE tmp_cnega",
   " SET ntra = ?"
   PREPARE p94_upd_tmp_cnega FROM l_sql

   LET l_sql = "INSERT INTO cnega",
   " SELECT ntra,tipo,subt,age1,rse1,cod1,sal1,sal2",
   " FROM tmp_cnega",
   " WHERE (sal1!=0 or sal2!=0)" #@011
   PREPARE p95_ins_cnega FROM l_sql

   LET l_sql = "SELECT MAX(eacccfcie)",
   " FROM eaccc"
   PREPARE p96_max_eaccc FROM l_sql

   LET l_sql = "select MAX(cnhegntra)",
   " from cnheg",
   " where cnhegfreg = ?"
   PREPARE p97_max_cnheg FROM l_sql

   LET l_sql = "delete from cnegs",
   " where cnegsntra = ?"
   PREPARE p98_del_cnegs FROM l_sql

   LET l_sql = "delete from cnega",
   " where cnegantra =?"
   PREPARE p99_del_cnega FROM l_sql

   LET l_sql = "delete from cnheg",
   " where cnhegntra = ?"
   PREPARE p100_del_cnheg FROM l_sql
   #@005 fin

   #@006 inicio
    LET l_sql = "SELECT DISTINCT(pcprmvalo::NUMERIC)pcprmvalo FROM gbofi, pcprm",
    " WHERE gbofinofi = pcprmdato AND pcprmflag=184 AND ",
    " gbofinofi NOT IN (6,8,9,11,18,19,10,16,20,21,22,23,4,26,27,24,39,35,36,38,46,54,55,45,40,13,98,",
    " 12,25,58,57,37,66,69,64,52) ",
    " AND gbofinofi IN (SELECT agen FROM tb_agenc)",
    " AND gbofinofi BETWEEN ? AND ?", #@008
    " ORDER BY pcprmvalo ASC"
    PREPARE p_agen FROM l_sql
    DECLARE q_agen CURSOR FOR p_agen

    LET l_sql = "SELECT nofi,desc FROM tmp_sedes",
    " WHERE valo = ? "
    PREPARE p_sede FROM l_sql
    DECLARE q_sede CURSOR FOR p_sede

    LET l_sql = "INSERT INTO tmp_sedes (nofi,desc,valo)",
    " SELECT gbofinofi, gbofidesc, pcprmvalo FROM gbofi, pcprm",
    " WHERE gbofinofi = pcprmdato AND pcprmflag=184 AND ",
    #" pcprmvalo NOT IN (6,8,9,11,18,19,10,16,20,21,22,23,4,26,27,24,39,35,36,38,46,54,55,45,40,13,98) ",
    #@008 ini
    " pcprmvalo not in (select cast(pcprmvalo as smallint)",
    " from pcprm",
    " where pcprmflag = 555",
    " and pcprmdato = 23)",
    #@008 fin
    " AND gbofinofi IN (SELECT agen FROM tb_agenc)"
    PREPARE p101_ins_tmp_sedes FROM l_sql

    LET l_sql = "UPDATE tmp_sedes ",
    " SET valo = ?",
    " WHERE nofi = ?"
    PREPARE p102_upd_tmp_sedes FROM l_sql

    LET l_sql = "DELETE FROM tmp_sedes"
    PREPARE p103_del_tmp_sedes FROM l_sql
   #@006 fin

   #@007 ini
   LET l_sql = "DELETE FROM tmp_adeu"
   PREPARE p104_del_tmp_adeu FROM l_sql

   LET l_sql = "INSERT INTO tmp_adeu",
   " SELECT saacracre,samadptea,",
   " CASE WHEN samadcmon=1 THEN samadmdes-NVL(capitalpagado,0)",
   " ELSE (samadmdes-NVL(capitalpagado,0))*? END saldos",
   " FROM tbsai:saacr INNER JOIN tbsai:samad ON samadcacr=saacracre",
   " LEFT JOIN (select sahpgnpre sahpgnpre1,sum(sahpgtotc) capitalpagado",
   " from tbsai:sahpg WHERE sahpgmrcb=0",
   " and sahpgfdep <=? group by 1)",
   " ON samadnpre=sahpgnpre1 WHERE samadmrcb=0 AND saacrmrcb=0",
   " AND samadfdes <=?",
   " and saacracre != 0",
   " and samadcmon != 0",
   " and saacrorig != 0",
   " and (CASE WHEN samadcmon=1 THEN samadmdes-NVL(capitalpagado,0)",
   " else (samadmdes-NVL(capitalpagado,0))*? end) !=0"
   PREPARE p105_ins_tmp_adeu FROM l_sql

   LET l_sql = "select first 1 gbhtctcof",
   " from tbsai:gbhtc",
   " where gbhtcfech = ?",
   " order by gbhtcfpro desc;"
   PREPARE p106_sel_tcof FROM l_sql

   LET l_sql ="SELECT SUM(sald)",
   " FROM tmp_adeu"
   PREPARE p107_sel_sald_adeu FROM l_sql

   LET l_sql ="SELECT SUM(tea0 * sald)",
   " FROM tmp_adeu"
   PREPARE p108_sel_tea_sald_adeu FROM l_sql

   LET l_sql = "select pcprmvalo",
   " from pcprm where pcprmflag = ?",
   " and pcprmdato = ?"
   PREPARE p109_sel_pcprm_valo FROM l_sql

   #@019 ini
   #--@019 LET l_sql = "select DATE(?) - ? UNITS MONTH",
   LET l_sql = "select LAST_DAY(DATE(?) - ? UNITS MONTH)",
   #@019 fin
   " from gbpmt"
   PREPARE p110_sel_fini_trim FROM l_sql

   LET l_sql = "select NVL(SUM(decode(samadcmon,1,samadmliq,samadmliq*?)),0)",
   " from tbsai:samad",
   " where samadfdes between ? and ?",
   " and samadmliq > 0",
   " and samadmrcb = 0"
   PREPARE p111_sel_samad_liq FROM l_sql

   LET l_sql = "select nvl(sum(decode(tstdccmon,1,tstdcimpo,tstdcimpo*?)),0)* -1",
   " from tbsai:tstdc",
   " where tstdcftra between ? and ?",
   " and tstdcpref = 1",
   " and tstdccorr in (select pcprmvalo",
   " from pcprm where pcprmflag = 555 and pcprmdato=8)",
   " and tstdcmrcb = 0"
   PREPARE p112_sel_tstdc_impo FROM l_sql

   LET l_sql= "select sum(decode(cpdtrcmon,1,nvl(cpdtrimpt,0)+nvl(cpdtrimcg,0),",
   " (nvl(cpdtrimpt,0)+nvl(cpdtrimcg,0))*?))",
   " FROM  tbsai:cpmcp,tbsai:cpdtr",
   " WHERE cpdtrftra between ? and ?",
   " AND cpmcpctcp in (select pcprmvalo",
   " from pcprm where pcprmflag = 555 and pcprmdato=9)",
   " AND cpmcpncpg = cpdtrncpg",
   " AND cpdtrclsc = 2",
   " AND cpdtrmrcb = 0"
   PREPARE p113_sel_cpmcp_impt FROM l_sql

   LET l_sql = "SELECT NVL(SUM(SALT),0)",
   " FROM TMP_DATOS",
   " WHERE CODI = ?"
   PREPARE p114_sel_tmp_datos_salt_codi FROM l_sql

   LET l_sql = "select NVL(SUM(sals),0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and esta !='CASTIGADO'"
   PREPARE p115_sel_sals FROM l_sql

   LET l_sql = "DELETE FROM TMP_DATOS",
   " WHERE CODI = ?"
   PREPARE p116_del_tmp_datos_salt_codi FROM l_sql

   LET l_sql = "DELETE FROM tmp_rseg_ini"
   PREPARE p117_del_tmp_rseg_ini FROM l_sql

   LET l_sql = "insert into tmp_rseg_ini",
   " SELECT distinct agen,cana",
   " from tmp_eaccc",
   " where fcie = ?",
   " and esta !='CASTIGADO'"
   PREPARE p118_ins_tmp_rseg_ini FROM l_sql

   LET l_sql = "select NVL(SUM(sals),0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and agen not in (select pcprmvalo",
   " from pcprm where pcprmflag = 555",
   " and pcprmdato = 11)",
   " and esta !='CASTIGADO'"
   PREPARE p119_sel_sals_sin_vehi FROM l_sql

   LET l_sql = "select NVL(SUM(sals),0)",
   " from tmp_eaccc",
   " where fcie = ?",
   " and agen = ?",
   " and agen not in (select pcprmvalo",
   " from pcprm where pcprmflag = 555",
   " and pcprmdato = 11)",
   " and esta !='CASTIGADO'"
   PREPARE p120_sel_sals_agen_sin_vehi FROM l_sql

   LET l_sql ="select gbofinofi,(? - date(pcprmdesc))/30",
   " from gbofi,pcprm",
   " where pcprmflag = 555",
   " and pcprmdato = 12",
   " and gbofinofi = pcprmvalo",
   " order by 1"
   PREPARE p121_sel_anti_sede FROM l_sql
   DECLARE q121_sel_anti_sede CURSOR FOR p121_sel_anti_sede

   LET l_sql = "select efparint1",
   " from efpar",
   " where efparpfij = 92",
   " and efparcorr = ?",
   " and ((?>efpardec2 and efpardec1 is null)",
   " or (?>efpardec1 and ?<=efpardec2)",
   " or (?>=efpardec1 and ?<=efpardec2 and efpardec1=0)",
   " )",
   " and efparmrcb = 0"
   PREPARE p122_buscar_item_sede FROM l_sql

   LET l_sql ="DELETE FROM tmp_area"
   PREPARE p123_del_tmp_area FROM l_sql

   LET l_sql ="INSERT INTO tmp_area VALUES(?,?,?,?)"
   PREPARE p124_ins_tmp_area from l_sql

   LET l_sql = "select efparint1,cast(efpardes3 as decimal(14,2))",
   " from efpar",
   " where efparpfij = 92",
   " and efparcorr = ?",
   " and efparmrcb = 0",
   " order by efparint1"
   PREPARE p125_sel_item_area FROM l_sql
   DECLARE q125_sel_item_area CURSOR FOR p125_sel_item_area

   LET l_sql = "SELECT SUM(nume)",
   " FROM tmp_area",
   " WHERE area = ?",
   " AND item = ?"
   PREPARE p126_sel_nume_item FROM l_sql

   LET l_sql = "SELECT SUM(nume)",
   " FROM tmp_area",
   " WHERE area = ?"
   PREPARE p127_sel_nume_tota FROM l_sql

   LET l_sql = "SELECT agen,nume",
   " FROM tmp_area",
   " WHERE area = ?",
   " AND item = ?"
   PREPARE p128_sel_agen_nume FROM l_sql
   DECLARE q128_sel_agen_nume CURSOR FOR p128_sel_agen_nume

   LET l_sql = "select trim(pcprmdesc)",
   " from pcprm where pcprmflag = 555",
   " and pcprmdato = 15",
   " and pcprmvalo = ?",
   " order by cast(pcprmdesc as smallint)"
   PREPARE p129_sel_detalle_codi FROM l_sql
   DECLARE q129_sel_detalle_codi CURSOR FOR p129_sel_detalle_codi

   LET l_sql = "DELETE FROM tmp_subt"
   PREPARE p130_del_tmp_subt FROM l_sql

   LET l_sql = "INSERT INTO tmp_subt VALUES(?,?,?,0,NULL)"
   PREPARE p133_ins_tmp_subt FROM l_sql

   LET l_sql = "select subt,porc FROM tmp_subt",
   " WHERE codi = ?"
   PREPARE p134_sel_subt_porc FROM l_sql
   DECLARE q134_sel_subt_porc CURSOR FOR p134_sel_subt_porc

   LET l_sql = "UPDATE tmp_subt set impt = ?,",
   " drub = ?",
   " WHERE codi = ?",
   " AND subt = ?"
   PREPARE p135_upd_tmp_subt FROM l_sql

   LET l_sql = "select subt,drub,impt FROM tmp_subt"
   PREPARE p135_sel_subt_impt FROM l_sql
   DECLARE q135_sel_subt_impt CURSOR FOR p135_sel_subt_impt

   LET l_sql = "SELECT DISTINCT DRUB",
   " FROM TMP_DATOS",
   " WHERE CODI = ?"
   PREPARE p136_sel_drub FROM l_sql

   LET l_sql = "SELECT NVL(SUM(impt),0)",
   " FROM tmp_subt WHERE codi = ?",
   " AND subt = ?"
   PREPARE p137_sel_impt_codi FROM l_sql

   LET l_sql = "SELECT DISTINCT AGEN",
   " FROM TMP_DATOS",
   " ORDER BY 1"
   PREPARE p138_sel_tmp_datos_salt FROM l_sql
   DECLARE q138_sel_temp_datos_salt CURSOR FOR p138_sel_tmp_datos_salt
  #@007 fin

  #@015 ini
  LET l_sql = " DELETE FROM tmp_pctao";
  PREPARE p139_del_tmp_pctao FROM l_sql

  LET l_sql = "INSERT INTO tmp_pctao",
  " select eacccagen,pctaorseg,eacccfdes fdes,ROW_NUMBER() OVER (",
	" PARTITION by pctaorseg ORDER BY eacccfdes desc) as fila",
  " from eaccc,pctao",
  " where eacccfcie = ?",
  " and eaccccana=pctaorseg",
  " and eacccnpre=pctaonpre",
  " and pctaorseg!=9999"
  PREPARE p140_ins_tmp_pctao FROM l_sql
  #@015 fin

  #@008 ini
  LET l_sql = "DELETE FROM tmp_rseo";
  PREPARE p139_del_tmp_rseo FROM l_sql

  LET l_sql = "INSERT INTO tmp_rseo",
  #@015 ini
  #" select pctaorseg,nvl((select distinct b.agen from tmp_eaccc b",
  #" where b.fcie = ? and b.cana=pctaorseg",
  #" and pctaorseg!=9999),pctaoagen),",
  " select pctaorseg,nvl(",
  " (select max(agen) from tmp_pctao where rseg = pctaorseg and fila = 1),",
  " pctaoagen),",
  #@015 fin
  " sum(sals)",
  " from tmp_eaccc,pctao",
  " where fcie = ?",
  " and npre = pctaonpre",
  " and cana = 9999",
  " and trim(esta)!='CASTIGADO'",
  " group by 1,2"
  PREPARE p140_ins_tmp_reso FROM l_sql

  LET l_sql = "SELECT NVL(SUM(sals),0)",
  " FROM tmp_rseo",
  " WHERE ageo = ?"
  PREPARE p141_sals_ageo FROM l_sql

  LET l_sql = "SELECT NVL(SUM(sals),0)",
  " FROM tmp_rseo",
  " WHERE ageo = ?",
  " AND rseo = ?"
  PREPARE p142_sals_rseo FROM l_sql

  LET l_sql = "SELECT count(distinct rseg)",
  " FROM tmp_rseg",
  " WHERE agen = ?",
  " AND rseg >-1" #@008
  PREPARE p143_sel_nume_rseg FROM l_sql

  LET l_sql = "SELECT GBOFINOFI, GBOFIDESC",
  " FROM GBOFI",
  " WHERE GBOFINOFI NOT IN (6,8,9,11,18,19,10,16,20,21,22,",
  " 23,4,26,27,24,39,35,36,38,46,54,55,45,40,13,98)",
  " AND GBOFINOFI IN (SELECT agen FROM tb_agenc)",
  " AND gbofinofi BETWEEN ? AND ?", #@008
  " ORDER BY GBOFINOFI"
  PREPARE p144_sel_oficina FROM l_sql
  DECLARE q_oficina CURSOR FOR p144_sel_oficina

  LET l_sql = "DELETE FROM tmp_pvrep"
  PREPARE p145_del_tmp_pvrep FROM l_sql

  LET l_sql = "INSERT INTO tmp_pvrep",
  " select pvrepftra,pvreptipo,nvl(agen,pcmpcagen),nvl(cana,pcmpcrseg),pvrepnpre,pvrepimpo",
  " from pvrep,pcmpc, outer tmp_eaccc",
  " where pvrepnpre = pcmpcnpre",
  " and pvrepftra in (?,?)",
  " and fcie = ?",
  " and pvrepnpre = npre",
  " and pvreptipo in (1,2)",
  " and pvrepmrcb = 0"
  PREPARE p146_ins_tmp_pvrep FROM l_sql

  LET l_sql = "INSERT INTO tmp_varp",
  " select a.agen,a.npre,a.cana,a.impo-b.impo",
  " from tmp_pvrep a,tmp_pvrep b",
  " where a.npre = b.npre",
  " and a.ftra = ?",
  " and b.ftra = ?",
  " and a.tipo = ?",
  " and b.tipo = ?",
  " and (a.impo - b.impo) > 0"
  PREPARE p147_ins_tmp_varp_01 FROM l_sql

  LET l_sql = "INSERT INTO tmp_varp",
  " select agen,npre,cana,impo",
  " from tmp_pvrep",
  " where ftra = ?",
  " and npre not in (",
  " select npre",
  " from tmp_pvrep",
  " where ftra = ?",
  " and tipo = ?)",
  " and tipo = ?;"
  PREPARE p148_ins_temp_varp_02 FROM l_sql

  LET l_sql = "INSERT INTO tmp_varp",
  " select nvl(agen,pcmpcagen),pctdtnpre,nvl(cana,pcmpcrseg),sum(pctdtimpp)",
  " from pctdt,pcmpc,outer tmp_eaccc",
  " where pctdtnpre = pcmpcnpre",
  " and fcie = ?",
  " and pcmpcnpre = npre",
  " and pctdtftra between ? and ?",
  " and pctdtttrn = 2",
  " and pctdtstat = 7",
  " and not (pctdtpref=22 and pctdtccon=122)", #--SIN RENDONDEO
  " and not (pctdtpref=26 and pctdtccon=1)", #--ITF
  " and pctdtmrcb = 0",
  " group by 1,2,3"
  PREPARE p149_ins_temp_varp_03 FROM l_sql

  LET l_sql = "DELETE FROM tmp_ings"
  PREPARE p150_del_tmp_ings FROM l_sql

  LET l_sql = "select pcsgrnage,cana,sum(round(pcsgrtasa*pcsgrsalo/100,5))",
  " from pcsgr,tmp_eaccc",
  " where pcsgrfreg = fcie",
  " and pcsgrnpre = npre",
  " and pcsgrfreg = ?",
  " group by 1,2"
  PREPARE p151_sel_pcsgr FROM l_sql
  DECLARE q151_sel_pcsgr CURSOR FOR p151_sel_pcsgr

  LET l_sql = "INSERT INTO tmp_ings VALUES (?,?,?,?)"
  PREPARE p152_ins_tmp_ings FROM l_sql

  LET l_sql = "select pcdmtimpt,(select cast(pcprmdesc as float)",
  " from pcprm where pcprmflag = 555",
  " and pcprmdato = 17",
  " and pcprmvalo = pcdmtimpt),",
  " pcdmtagen,nvl(eaccccana,pcmpcrseg),sum(pcdmtimpt)",
  " from pchmt,pcdmt,pcmpc, outer eaccc",
  " where pchmtntra = pcdmtntra",
  " and pcdmtmodn = 18",
  " and pchmtfreg = ?",
  " and pchmtfreg = eacccfcie",
  " and pcdmtcodi = pcmpcnpre",
  " and pcmpcnpre = eacccnpre",
  " and pchmtmrcb = 0",
  " and pcdmtimpt > 0",
  " and pcmpcmrcb = 0",
  " group by 1,2,3,4"
  PREPARE p153_sel_pcdmt FROM l_sql
  DECLARE q153_sel_pcdmt CURSOR FOR p153_sel_pcdmt

  LET l_sql = "INSERT INTO tmp_ings",
  " select 3,nvl(agen,pcmpcagen),nvl(cana,pcmpcrseg),sum(cjtrnimpo)",
  " from pcmpc,pccna,cjtrn,outer tmp_eaccc",
  " where pcmpcnpre = pccnanpre",
  " and pccnantra = cjtrnntra",
  " and cjtrnftra between ? and ?",
  " and fcie = ?",
  " and pcmpcnpre = npre",
  " and pccnatpem = 2",
  " and pccnamrcb = 0",
  " and cjtrnstat = 0",
  " group by 1,2,3"
  PREPARE p154_ins_tmp_ings_pccna FROM l_sql

  LET l_sql = "SELECT nvl(SUM(a.impo),0)",
  " FROM tmp_ings a,tmp_rseg b",
  " WHERE a.agen = b.agen",
  " and a.cana = b.rseg",
  " and a.agen = ?"
  PREPARE p155_sel_tmp_ings_agen FROM l_sql

  LET l_sql = "SELECT SUM(impo)",
  " FROM tmp_ings",
  " WHERE agen = ?",
  " AND cana = ?"
  PREPARE p156_sel_tmp_ings_cana FROM l_sql

  LET l_sql = "drop index tmp_pctcn_01"
  PREPARE p158_drop_ind_pctcn FROM l_sql

  LET l_sql = "DELETE FROM tmp_gfin"
  PREPARE p159_del_tmp_gfin FROM l_sql

  LET l_sql = "INSERT INTO tmp_gfin",
  " select 1,nvl(agen,pcmpcagen),nvl(cana,pcmpcrseg),sum(pcdvicomi)",
  " from pcdvi,pcmpc,outer tmp_eaccc",
  " where pcdviftra between ? and ?",
  " and pcdvinpre = pcmpcnpre",
  " and fcie = ?",
  " and pcmpcnpre = npre",
  " and pcdvittrn = 2",
  " and pcdvicvia in (",
  " select TRIM(pcprmvalo)",
  " from PCPRM where pcprmflag = 555",
  " and pcprmdato = 21)",
  " and pcdvimrcb = 0",
  " group by 1,2,3"
  PREPARE p160_ins_tmp_gfin_1 FROM l_sql

  LET l_sql = "INSERT INTO tmp_gfin",
  " select 1,pctcnagen,nvl(cana,pcmpcrseg),sum(pctcnimpi)",
  " from pctcn,pcmpc,outer tmp_eaccc",
  " where pctcnftra between ? and ?",
  " and pctcnnpre = pcmpcnpre",
  " and fcie = ?",
  " and pcmpcnpre = npre",
  " and pctcnttrn = 2",
  " and pctcncctb in (",
  " select TRIM(pcprmvalo)",
  " from PCPRM where pcprmflag = 555",
  " and pcprmdato = 22)",
  " group by 1,2,3"
  PREPARE p161_ins_tmp_gfin_2 FROM l_sql

  LET l_sql = "INSERT INTO tmp_gfin",
  " SELECT 2,tfdcoagen,nvl(cana,pcmpcrseg),sum(tfdcocomr)",
  " FROM tfhco,tfdco,pchtr,pcmpc,outer tmp_eaccc",
  " WHERE tfhcontra=tfdcontra",
  " and tfdcotrac = pchtrntra",
  " and pchtrftra between ? and ?",
  " and pchtrnpre = pcmpcnpre",
  " and fcie = ?",
  " and pcmpcnpre = npre",
  " and pchtrttrn = 1",
  " and tfdcocomi = 0",
  " and tfdcocomr > 0",
  " AND tfhcomrcb = 0",
  " AND tfdcostat = 1",
  " AND tfdcomrcb = 0",
  " group by 1,2,3"
  PREPARE p162_ins_tmp_gfin_3 FROM l_sql

  LET l_sql = "SELECT nvl(SUM(a.impo),0)",
  " FROM tmp_gfin a,tmp_rseg b",
  " WHERE a.agen = b.agen",
  " and a.cana = b.rseg",
  " and a.agen = ?"
  PREPARE p163_sel_tmp_gfin_agen FROM l_sql

  LET l_sql = "SELECT SUM(impo)",
  " FROM tmp_gfin",
  " WHERE agen = ?",
  " AND cana = ?"
  PREPARE p164_sel_tmp_gfin_cana FROM l_sql

  LET l_sql = "select pcprmdato,pcprmvalo",
  " from pcprm",
  " where pcprmflag = ?"
  PREPARE p165_sel_pcprm FROM l_sql
  DECLARE q165_sel_pcprm CURSOR FOR p165_sel_pcprm

  LET l_sql = "SELECT DISTINCT CODI",
  " FROM TMP_DATOS",
  " UNION",
  " select CAST(pcprmvalo AS SMALLINT)",
  " from pcprm",
  " where pcprmflag = 555",
  " and pcprmdato = 6",
  " and pcprmvalo = 64",
  " ORDER BY 1"
  PREPARE p166_sel_codi_tmp_datos FROM l_sql
  DECLARE q166_sel_codi_tmp_datos CURSOR FOR p166_sel_codi_tmp_datos

  LET l_sql = "SELECT COUNT(DISTINCT CANA)",
  " FROM TMP_DATOS_CANA",
  " WHERE AGEN = ?",
  " AND CANA > -1" #@009
  PREPARE p167_sel_cont_cana FROM l_sql

  LET l_sql = "UPDATE TMP_DATOS_CANA",
  " SET SALT = SALT + ?",
  " WHERE CODI = ?",
  " AND AGEN = ?"
  PREPARE p168_upd_tmp_datos_cana FROM l_sql

  LET l_sql = "SELECT DISTINCT CANA",
  " FROM TMP_DATOS_CANA",
  " WHERE AGEN = ?",
  " AND CANA > -1" #@009
  PREPARE p169_sel_tmp_cana FROM l_sql
  DECLARE q169_sel_tmp_cana CURSOR FOR p169_sel_tmp_cana

  LET l_sql = "SELECT COUNT(DISTINCT CANA)",
  " FROM (",
  " SELECT CANA,SUM(SALT) SALX",
  " FROM TMP_DATOS_CANA",
  " WHERE CODI = ?",
  " AND AGEN = ?",
  " GROUP BY 1",
  " ) as p",
  " WHERE SALX > 0"
  PREPARE p170_sel_salt_cana FROM l_sql

  LET l_sql = "DELETE FROM TMP_DATOS_CANA WHERE CODI = ?"
  PREPARE p171_del_tmp_datos_cana_codi FROM l_sql

  LET l_sql = "DELETE FROM tmp_pctdt"
  PREPARE p172_del_tmp_pctdt FROM l_sql

  LET l_sql  = "DELETE FROM tmp_pcpfu"
  PREPARE p173_del_tmp_pcpfu FROM l_sql

  LET l_sql = "INSERT INTO tmp_pcpfu",
  " select pcpfunpre,pcpfunpfu,max(pchtrntra)",
  " from pcpfu,pcmpc,pchtr",
  " where pcpfunpre = pcmpcnpre",
  " and pcpfunpfu = pchtrnpre",
  " and pchtrftra between ? and ?",
  " and pcmpcstat >=3",
  " and pchtrttrn = 2",
  " and pchtrmrcb = 0",
  " group by 1,2;"
  PREPARE p174_ins_tmp_pcpfu FROM l_sql

  LET l_sql = "INSERT INTO tmp_pctdt",
  " select nvl(agen,pcmpcagen),nvl(cana,pcmpcrseg),sum(pctdtimpp)*-1",
  " from pctdt,pcmpc,outer tmp_eaccc",
  " where pctdtftra between ? and ?",
  " and pctdtnpre = pcmpcnpre",
  " and fcie = ?",
  " and pcmpcnpre = npre",
  " and pctdtntra not in (select ntra from tmp_pcpfu)",
  " and pctdtpref||'_'||pctdtccon in (",
  " select pcprmvalo from pcprm where pcprmflag = 555",
  " and pcprmdato = 24)",
  " and pctdtttrn = 2",
  " and pctdtmrcb = 0",
  " group by 1,2"
   PREPARE p173_ins_tmp_pctdt FROM l_sql

   LET l_sql = "select nvl(sum(impt),0)",
   " from tmp_pctdt",
   " where agen = ?",
   " and cana = ?"
   PREPARE p174_sel_tmp_pctdt_impt FROM l_sql

   LET l_sql = "select nvl(sum(pchidimpt),0)",
   " from pchid",
   " where pchidfcie = ?",
   " and pchidagen = ?",
   " and pchidcana = ?",
   #" and pchidstat between 3 and 6",
   " and pchidmrcb = 1" #@012
   PREPARE p175_sel_pchid_impt FROM l_sql

   LET l_sql = "INSERT INTO tmp_pctdt",
   " select nvl(agen,pcmpcagen),nvl(cana,pcmpcrseg),sum(pctcnimpi)",
   " from pctcn,pcmpc,outer tmp_eaccc",
   " where pctcnftra between ? and ?",
   " and pctcnnpre = pcmpcnpre",
   " and pctcncctb in (",
   " select trim(pcprmvalo)",
   " from pcprm where pcprmflag = 555",
   " and pcprmdato = 25)",
   " and fcie = ?",
   " and pcmpcnpre = npre",
   " and pctcnimpi < 0",
   " group by 1,2"
   PREPARE p176_ins_tmp_pctdt_rec FROM l_sql

   #@009 ini
   LET l_sql = "select nvl(sum(salx),0)",
   " from (SELECT cana,NVL(SUM(SALT),0) salx",
   " from TMP_DATOS_CANA p",
   " where codi = ? and agen = ?",
   " group by 1",
   " having NVL(SUM(SALT),0) > 0)"
   {LET l_sql = "SELECT NVL(SUM(SALT),0)",
   " FROM TMP_DATOS_CANA",
   " WHERE CODI = ?",
   " AND AGEN = ?",
   " AND SALT > 0"}
   #@009 fin
   PREPARE p171_sel_tmp_datos_salt_posi FROM l_sql

   LET l_sql = "DELETE FROM tmp_ajus"
   PREPARE p172_del_tmp_ajus FROM l_sql

   #@013 Inicio
   LET l_sql = "SELECT a.agen,a.cana,NVL(sum(a.ingm),0),NVL(sum(a.dife),0),",
   #LET l_sql = "SELECT a.agen,a.cana,sum(a.ingm),sum(a.dife),",#@012
   #" sum(a.dvg1),sum(a.dvg2),sum(a.ingm),",
   #" sum(a.dife),sum(a.ingf),nvl(sum(b.impt),0),sum(c.salt)",
   #" sum(a.dvg1),sum(a.dvg2),sum(a.post),", #@012
   #" sum(a.ingf),nvl(sum(b.impt),0),sum(c.salt)", #@012
   " NVL(sum(a.dvg1),0),NVL(sum(a.dvg2),0),NVL(sum(a.post),0), NVL(sum(a.mo19),0),",
   " NVL(sum(a.ingf),0),NVL(sum(b.impt),0),NVL(sum(c.salt),0)",
   #" FROM tmp_ingf a,tmp_datos_cana c,outer tmp_ajus b",
   " FROM tmp_ingf a, outer tmp_datos_cana c,outer tmp_ajus b",
   #@013 Fin
   " WHERE a.agen = c.agen",
   " AND a.cana = c.cana",
   " AND c.codi = 2",
   " AND a.agen = b.agen",
   " group by 1,2",
   " order by 1,2;"
   PREPARE p173_sel_detalle_codi_2 FROM l_sql
   DECLARE q173_sel_detalle_codi_2 CURSOR FOR p173_sel_detalle_codi_2

   LET l_sql = "select count(*)",
   " from adprf,adusr",
   " where adprfusrn = adusrusrn",
   " and adprfperf in (",
   " select trim(pcprmvalo) from pcprm where pcprmflag = 555",
   " and pcprmdato = 26)",
   " and adusrusrn = ?",
   " and adusrmrcb = 0;"
   PREPARE p174_sel_cont_usrn_codi_2 FROM l_sql
   #@008 fin

   #@009 ini
   LET l_sql = "select pctaorseg",
   " from pctao",
   " where pctaonpre = ?",
   " and pctaoagen = ?"
   PREPARE p175_sel_agen_orig FROM l_sql

   LET l_sql = "select agen,npre",
   " FROM tmp_varp",
   " WHERE cana in (9999,6666)",
   " ORDER BY 1,2" #@011
   PREPARE p176_sel_prov_cob FROM l_sql
   DECLARE q176_sel_prov_cob CURSOR FOR p176_sel_prov_cob

   LET l_sql = "update tmp_varp",
   " set cana = ?",
   " where npre = ?"
   PREPARE p177_upd_varp FROM l_sql

   LET l_sql = "DELETE FROM tmp_tasp"
   PREPARE p178_del_tmp_tasp FROM l_sql

   LET l_sql = "INSERT INTO tmp_tasp VALUES(?,?,?,?)"
   PREPARE p179_ins_tmp_tasp FROM l_sql

   LET l_sql = "SELECT DISTINCT agen",
   " FROM tmp_eaccc",
   " WHERE fcie = ?",
   " AND esta !='CASTIGADO'",
   " ORDER BY 1"
   PREPARE p180_sel_tmp_eaccc_agen FROM l_sql
   DECLARE q180_sel_temp_eacc_agen CURSOR FOR p180_sel_tmp_eaccc_agen

   LET l_sql = "SELECT DISTINCT cana",
   " FROM tmp_eaccc",
   " WHERE fcie = ?",
   " AND agen = ?",
   " AND esta !='CASTIGADO'",
   " ORDER BY 1"
   PREPARE p181_sel_tmp_eaccc_cana FROM l_sql
   DECLARE q181_sel_temp_eacc_cana CURSOR FOR p181_sel_tmp_eaccc_cana

   LET l_sql = "SELECT SUM(tea0*sals)",
   " FROM tmp_eaccc",
   " WHERE fcie = ?",
   " AND agen = ?",
   " AND cana = ?",
   " AND esta !='CASTIGADO'"
   PREPARE p182_sel_tea0_sals FROM l_sql

   LET l_sql = "SELECT SUM(tea0*sals)",
   " FROM tmp_eaccc",
   " WHERE fcie = ?",
   " AND agen = ?",
   " AND esta !='CASTIGADO'"
   PREPARE p183_sel_tea0_agen_sals FROM l_sql

   LET l_sql = "SELECT tea0",
   " FROM tmp_tasp",
   " WHERE tipo = ?",
   " and agen = ?",
   " AND cana = ?"
   PREPARE p184_sel_tpp FROM l_sql

   LET l_sql = "SELECT eacccdatr",
   " FROM eaccc",
   " WHERE eacccfcie = ?",
   " AND eacccnpre = ?"
   PREPARE p185_sel_eaccc_datr FROM l_sql

   LET l_sql = "DELETE FROM tmp_fecx"
   PREPARE p12_del_tmp_fecha FROM l_sql

   LET l_sql = "INSERT INTO tmp_fecx VALUES(?,?)"
   PREPARE p13_ins_tmp_fecha FROM l_sql

   LET l_sql = "select nvl(sum(cnegasals),0),count(distinct cnhegfreg)",
   " from cnheg,cnega",
   " where cnhegntra = cnegantra",
   " and cnhegfreg in (select fcie from tmp_fecx where tipo=2)",
   " and cnegatipo = 2", #Mensual
   " and cnegasubt = ?", #Subtipo -1.Sin cobranzas 2.Cobranzas
   " and cnegaagen = ?",
   " and cnegarseg = ?",
   " and cnegacodi = 1", #@011 interes totales
   " and cnhegmrcb = 0"
   PREPARE p14_sel_ings_12_sc FROM l_sql  #@011

   #@011 ini
   LET l_sql = "select nvl(sum(cnegasalc),0),count(distinct cnhegfreg)",
   " from cnheg,cnega",
   " where cnhegntra = cnegantra",
   " and cnhegfreg in (select fcie from tmp_fecx where tipo=2)",
   " and cnegatipo = 2", #Mensual
   " and cnegasubt = ?", #Subtipo -1.Sin cobranzas 2.Cobranzas
   " and cnegaagen = ?",
   " and cnegarseg = ?",
   " and cnegacodi = 1", #@011 interes totales
   " and cnhegmrcb = 0"
   PREPARE p14_sel_ings_12 FROM l_sql
   #@011 fin

   LET l_sql = "select nvl(sum(cnegssals),0),count(distinct cnhegfreg)",
   " from cnheg,cnegs",
   " where cnhegntra = cnegsntra",
   " and cnhegfreg in (select fcie from tmp_fecx where tipo=1)",
   " and cnegstipo = 2", #Mensual
   " and cnegsagen = ?",
   " and cnegscodi = 1", #@011 #interes por agencia
   " and cnhegmrcb = 0"
   PREPARE p14_sel_ings_12_sede_sc FROM l_sql #@011

   #@011 ini
   LET l_sql = "select nvl(sum(cnegssalc),0),count(distinct cnhegfreg)",
   " from cnheg,cnegs",
   " where cnhegntra = cnegsntra",
   " and cnhegfreg in (select fcie from tmp_fecx where tipo=1)",
   " and cnegstipo = 2", #Mensual
   " and cnegsagen = ?",
   " and cnegscodi = 1",
   " and cnhegmrcb = 0"
   PREPARE p14_sel_ings_12_sede FROM l_sql
   #@011 fin

   LET l_sql = "select nvl(sum(eacccsals),0)",
   " from eaccc",
   " where eacccfcie IN (select fcie from tmp_fecx where tipo=1)",
   " and eacccesta!='CASTIGADO'",
   " and eacccagen = ?"
   PREPARE p15_sel_sals_anual FROM l_sql

   LET l_sql = "select nvl(sum(eacccsals),0)",
   " from eaccc",
   " where eacccfcie IN (select fcie from tmp_fecx where tipo=2)",
   " and eacccesta!='CASTIGADO'",
   " and eacccagen = ?",
   " and eaccccana = ?"
   PREPARE p16_sel_sals_anual_cana FROM l_sql
   #@009 fin

   #@011 ini
   LET l_sql = "DELETE FROM tmp_cnega"
   PREPARE p17_del_tmp_cnega FROM l_sql

   LET l_sql = "DELETE FROM tmp_cnegs"
   PREPARE p18_del_tmp_cnegs FROM l_sql
   #@0114 fin

   #@013 inicio
   LET l_sql = "DELETE FROM tmp_pctcn_02"
   PREPARE p19_del_tmp_pctcn_02 FROM l_sql

   LET l_sql = "INSERT INTO tmp_pctcn_02 ",
   " Select pctcnftra,pctcnnpre, pctcnttrn,pctcnpref,pctcnccon,pctcncctb, pctcnimpi,pctcnagen,cana",
   " from pctcn, tmp_eaccc",
   " where pctcnftra between ? and ?",
   " and fcie = ?",
   " and pctcnnpre = npre",
   " and pctcnpost = 9"

   PREPARE p19_ins_tmp_pctcn_post_02_1 FROM l_sql

   LET l_sql = "INSERT INTO tmp_pctcn_02 ",
   " Select pctcnftra, pctcnnpre, pctcnttrn,pctcnpref,pctcnccon,pctcncctb, pctcnimpi,pctcnagen,pcmpcrseg",
   " from pctcn, pcmpc",
   " where pctcnftra between ? and ?",
   " and pctcnnpre = pcmpcnpre",
   " and pctcnpost = 9",
   " and pcmpcmrcb = 0",
   " and pctcnnpre not in (select pctcnnpre from tmp_pctcn_02 )"
   PREPARE p19_ins_tmp_pctcn_post_03 FROM l_sql

   LET l_sql =
   " INSERT INTO tmp_pctcn",
   " select * from (select 2,pctcncctb,pctcnagen,pctcnnpre,pcmpcrseg,SUM(pctcnimpi) * -1",
   " from tmp_pctcn_02",
	" where pctcnttrn = 2",
   " and pctcnpref = 20",
   " and pctcnccon = 2",
   #@018 " and pctcncctb not in (select cctb from tmp_pctcn where tipo=1)",
   " and pctcncctb[1,4] in (select pcprmvalo from pcprm where pcprmflag  = 555 and pcprmdato between 30 and 31)",
   " group by 1,2,3,4,5",
   " union all ",
   " select 2,pctcncctb,pctcnagen,pctcnnpre,pcmpcrseg,SUM(pctcnimpi) * -1",
   " from tmp_pctcn_02",
   " where pctcnttrn = 1",
   " and pctcnpref = 20",
   " and pctcnccon = 1",
   #@018" and pctcncctb not in (select cctb from tmp_pctcn where tipo=1)",
   " and pctcncctb[1,4] in (select pcprmvalo from pcprm where pcprmflag  = 555 and pcprmdato between 30 and 31)",
   " group by 1,2,3,4,5",
   " union all",
   " select 2,pctcncctb,pctcnagen,pctcnnpre,pcmpcrseg,SUM(pctcnimpi) * -1",
   " from tmp_pctcn_02",
   " where pctcnttrn = 2",
   " and pctcnpref = 22",
   " and pctcnccon = 188",
   #@018" and pctcncctb not in (select cctb from tmp_pctcn where tipo=1)",
   " and pctcncctb[1,4] in (select pcprmvalo from pcprm where pcprmflag  = 555 and pcprmdato = 31)",
   " group by 1,2,3,4,5",
   " union all",
   " select 2,pctcncctb,pctcnagen,pctcnnpre,pcmpcrseg,SUM(pctcnimpi) * -1",
   " from tmp_pctcn_02",
   " where pctcnttrn = 13",
   " and pctcnpref = 20",
   " and pctcnccon = 2",
   #@018" and pctcncctb not in (select cctb from tmp_pctcn where tipo=1)",
   " and pctcncctb[1,4] in (select pcprmvalo from pcprm where pcprmflag  = 555 and pcprmdato = 30)",
   "  group by 1,2,3,4,5",
   " union all",
   " select 2,pctcncctb,pctcnagen,pctcnnpre,pcmpcrseg,SUM(pctcnimpi) * -1",
   " from tmp_pctcn_02",
   " where pctcnttrn = 12",
   " and pctcnpref = 20",
   " and pctcnccon = 2",
   #@018" and pctcncctb not in (select cctb from tmp_pctcn where tipo=1)",
   " and pctcncctb[1,4] in (select pcprmvalo from pcprm where pcprmflag  = 555 and pcprmdato = 30)",
   " group by 1,2,3,4,5)"
   PREPARE p19_ins_tmp_pctcn_post FROM l_sql

   #@012 ini
   {LET l_sql = "INSERT INTO tmp_pctcn",
   " select 2,pctcncctb,pctcnagen,pctcnnpre,pcmpcrseg,SUM(pctcnimpi) * -1",
   " from pctcn,pcmpc",
   " where pctcnftra between ? and ?",
   " and pctcnnpre = pcmpcnpre",
   " and pctcnttrn = 2",
   " and pctcnpref = 20",
   " and pctcnccon = 2",
   " and pctcncctb not in (select cctb from tmp_pctcn where tipo=1)",
   " and pctcnntra not in (select ntra from tmp_pcpfu)", #--Sin refinanciados--#
   " and pctcnpost = 9",
   " and pcmpcstat !=7", #--Sin castigados--#
   " and pcmpcmrcb = 0",
   " group by 1,2,3,4,5"
   PREPARE p19_ins_tmp_pctcn_post FROM l_sql}
   #@012 fin
   #@013 fin

   #@016 Inicio
   LET l_sql =  "insert into tmp_nprx ",
   " SELECT distinct a.pcmpcnpre, pchtxrsef ",
   " FROM pcmpc a, pchtx",
   " WHERE a.pcmpcmrcb = 0",
   " AND a.pcmpcstat = 9 ",
   " AND a.pcmpcfsta BETWEEN ? AND ? ",
   " AND a.pcmpcrseg not in (Select distinct eacdccana from tmp_eacdc where eacdcfreg = ?) ",
   " AND a.pcmpcagen in (Select agen from tmp_rseg_ini)",
   " AND pchtxttrx = 6 ", -- transferencia de cartera
   --" AND pchtxrsei = a.pcmpcrseg" ,--analista inicial  #@021
   " and a.pcmpcnpre = pchtxnpre", #@021
   " AND pchtxfpro BETWEEN ? AND ? " ,---Maximo registro de transferencia de cartera
   " AND a.pcmpcrseg not in (select pcmpcrseg from pcmpc where pcmpcmrcb = 0 and pcmpcstat between 3 and 7 )"; -- No tiene cartera vigente
   PREPARE p119_ins_tmp_nprx FROM l_sql

   LET l_sql =  "update tmp_eacdc b",
   " set b.eacdccana =  (SELECT a.cana FROM tmp_nprx a WHERE a.npre = b.eacdcnpre)",
   " where b.eacdcnpre in (select npre from tmp_nprx)"
   PREPARE p120_upd_tmp_eacdc_03 FROM l_sql

   LET l_sql = "  UPDATE tmp_pctcn b",
   " SET b.cana = (SELECT distinct a.cana",
   " FROM tmp_nprx a",
   " WHERE a.npre = b.npre)",
   " WHERE b.tipo = 1",
   " AND b.npre in (select npre from tmp_nprx)"
   PREPARE p19_ins_tmp_pctcn_tpo_1_06 FROM l_sql

   LET l_sql = "  UPDATE tmp_pctcn b",
   " SET b.cana = (SELECT distinct a.cana",
   " FROM tmp_nprx a",
   " WHERE a.npre = b.npre)",
   " WHERE b.tipo = 2",
   " AND b.npre in (select npre from tmp_nprx)"
   PREPARE p19_ins_tmp_pctcn_tpo_1_07 FROM l_sql
   #@016 Fin

END FUNCTION
#@004 fin

FUNCTION f0300_proceso_ea017()
DEFINE   l_hora   CHAR(8)

   #@003 ini
   LET g_des1 = NULL
   select gbcondesc INTO g_des1
   from gbcon
   where gbconpfij=10
   and gbconcorr=1
   #@003 fin

   LET p1.vouc = "S"
   #@004 ini
   LET p1.deta = "N"
   LET p1.det1 = "N"
   LET p1.det2 = "N"
   LET p1.unif = "S" #@006
   LET p1.tipo = 1
   #DISPLAY BY NAME p1.deta,p1.det1,p1.det2,p1.tipo
   DISPLAY BY NAME p1.deta,p1.det1,p1.det2,p1.unif,p1.tipo
   #@004 fin
   DISPLAY BY NAME p1.vouc

   OPTIONS INPUT WRAP
   LET  g_spool  = "ea017.r"

   WHILE TRUE
      CALL f6000_limpiar_campos_ea017()
      INPUT BY NAME m1.* WITHOUT DEFAULTS

         ON KEY (CONTROL-C,INTERRUPT)
            LET int_flag = TRUE
            EXIT INPUT

         ON KEY (CONTROL-M)
            IF INFIELD(o1) THEN
               IF f0400_pedir_datos_ea017() THEN
                  #CALL f1000_impreso_ea017()
                  MESSAGE "Termine de Procesar" #@004
                  #CALL f0100_imprimir_gb001(g_spool)
                  #IF p1.ambi = 3 THEN
                  CALL imprimir_excel()
                  CALL f0100_imprimir_gb001(g_spool)
                  #END IF
                  #DISPLAY "TERMINE AHHH"
               END IF
               NEXT FIELD o1
            END IF

            IF INFIELD(o2) THEN
               #IF f0400_pedir_datos_ea017() THEN	#@001
               ERROR "OPCION NO HABILITADA"
               #END IF	#@001
               NEXT FIELD o2
            END IF

            IF INFIELD(o3) THEN
               CALL f0100_imprimir_gb001(g_spool)
               NEXT FIELD o3
            END IF

            IF INFIELD(o4) THEN
               EXIT WHILE
            END IF

        BEFORE FIELD o1
                DISPLAY m1.d1 TO d1 ATTRIBUTE(REVERSE)
                LET m1.o1 = "*"
        AFTER FIELD o1
                INITIALIZE m1.o1 TO NULL
                DISPLAY m1.d1 TO d1 ATTRIBUTE(NORMAL)
                DISPLAY m1.o1 TO o1
        BEFORE FIELD o2
                DISPLAY m1.d2 TO d2 ATTRIBUTE(REVERSE)
                LET m1.o2 ="*"
        AFTER  FIELD o2
                INITIALIZE m1.o2 TO NULL
                DISPLAY m1.d2 TO d2 ATTRIBUTE(NORMAL)
                DISPLAY m1.o2 TO o2
        BEFORE FIELD o3
                DISPLAY m1.d3 TO d3 ATTRIBUTE(REVERSE)
                LET m1.o3 = "*"
        AFTER FIELD o3
                INITIALIZE m1.o3 TO NULL
                DISPLAY m1.d3 TO d3 ATTRIBUTE(NORMAL)
                DISPLAY m1.o3 TO o3
        BEFORE FIELD o4
                DISPLAY m1.d4 TO d4 ATTRIBUTE(REVERSE)
                LET m1.o4 = "*"
        AFTER FIELD o4
                INITIALIZE m1.o4 TO NULL
                DISPLAY m1.d4 TO d4 ATTRIBUTE(NORMAL)
                DISPLAY m1.o4 TO o4
        END INPUT
        IF int_flag THEN
                LET int_flag = FALSE
                CONTINUE WHILE
        END IF
        END WHILE
END FUNCTION

FUNCTION f0400_pedir_datos_ea017()
DEFINE   l_contador INTEGER,
         #@004 ini
         l_fant   DATE,
         l_cade   CHAR(6),
         l_cad1   CHAR(6),
         l_1fec   DATE, #@013
         l_hora   CHAR(8)
         #@004 fin

   #@004 ini
    LET p1.deta = "N"
    LET p1.tipo = 1
    LET p1.det1 = "N"
    LET p1.det2 = "N"
    LET p1.unif = "S"
    DISPLAY BY NAME p1.deta,p1.tipo,p1.det1,p1.det2,p1.unif
    #@004 fin
    #@008 ini
    LET p1.agei = 1
    LET p1.agef = 99
    #@008 fin
    DISPLAY BY NAME p1.agei,p1.agef

    LET p1.vouc = "S"
    DISPLAY BY NAME p1.vouc

    OPTIONS INPUT NO WRAP
    INPUT BY NAME p1.* WITHOUT DEFAULTS
      ON KEY (CONTROL-C,INTERRUPT)
         LET int_flag = TRUE
         EXIT INPUT

      AFTER FIELD fech
         IF p1.fech IS NULL THEN
            ERROR "INGRESE FECHA INICIAL"
            NEXT FIELD fech
         END IF

       #@008 ini
        AFTER FIELD agei
            IF p1.agei IS NULL THEN
                ERROR "INGRESE SEDE INICIAL"
                NEXT FIELD agei
            END IF

        AFTER FIELD agef
            IF p1.agef IS NULL THEN
                ERROR "INGRESE SEDE FINAL"
                NEXT FIELD agef
            END IF

            IF p1.agei > p1.agef THEN
                ERROR "SEDE FINAL DEBE SER MENOR A SEDE INICIAL"
                SLEEP 1
                NEXT FIELD agef
            END IF
       #@008 fin

         #@004 ini
         AFTER FIELD deta
            IF p1.deta IS NULL THEN
               LET p1.deta = "N"
            END IF

         BEFORE FIELD tipo
            IF p1.deta  = "S" THEN
               LET p1.tipo = 2
               DISPLAY BY NAME p1.tipo
            END IF

         AFTER FIELD tipo
            IF p1.tipo IS NULL THEN
               LET p1.tipo = 1
            END IF

            IF p1.deta = "N" THEN
               #EXIT INPUT
               NEXT FIELD unif  #@006
            END IF

         AFTER FIELD det1
            IF p1.det1 IS NULL THEN
               LET p1.det1 = "N"
            END IF

         AFTER FIELD det2
            IF p1.det2 IS NULL THEN
               LET p1.det2 = "N"
            END IF

         #@004 fin

         #@005 ini
         AFTER FIELD vouc
            IF p1.vouc IS NULL THEN
               LET p1.vouc = "S"
            END IF
         #@005 fin

         #@006 ini
         AFTER FIELD unif
            IF p1.unif IS NULL THEN
               LET p1.unif = "S"
            END IF
         #@006 fin

                {AFTER FIELD ambi
                       IF p1.ambi IS NULL THEN
                          ERROR "Ingresar ambito del reporte"
                          NEXT FIELD ambi
                       ELSE
                          CASE p1.ambi
                            WHEN 1 LET p1.damb = "CONSOLIDADO"
                            WHEN 2 LET p1.damb = "POR PLAZA"
                            #WHEN 3 LET p1.damb = "POR AGENCIA"
							WHEN 3 LET p1.damb = "POR AGENCIA",UPSHIFT(g_sede) CLIPPED	#@001
                          END CASE
                          DISPLAY BY NAME p1.damb
                       END IF
                AFTER FIELD agei
                        IF p1.agei IS NULL THEN
                           LET p1.agei = 1
                           DISPLAY BY NAME p1.agei
                        END IF
                AFTER FIELD agef
                        IF p1.agef IS NULL THEN
                           LET p1.agef = 999
                           DISPLAY BY NAME p1.agef
                        END IF
                    {AFTER FIELD vouc
                        IF p1.vouc IS NULL THEN
                           NEXT FIELD vouc
                        END IF        }
        END INPUT
        OPTIONS INPUT WRAP
        IF int_flag THEN
                LET int_flag = FALSE
                RETURN FALSE
        END IF
        ERROR "Procesando ... Un momento por favor .."
        #@004 ini
        LET l_hora = TIME
        DISPLAY "Inicio:",l_hora
        #IF (p1.deta = 'S') THEN
           EXECUTE p11_del_tmp_eaccc
           EXECUTE p27_del_tmp_codia

           LET l_cade = MONTH(p1.fech) using "&&",YEAR(p1.fech)
           EXECUTE p28_ins_tmp_codia_1 USING l_cade
           EXECUTE p12_ins_tmp_eaccc USING p1.fech,p1.agei,p1.agef #@008

           LET l_fant = p1.fech - DAY(p1.fech)
           LET l_1fec= l_fant +1 #@013
           LET l_cad1 = MONTH(l_fant) using "&&",YEAR(l_fant)
           EXECUTE p28_ins_tmp_codia_0 USING l_cad1
           EXECUTE p12_ins_tmp_eaccc USING l_fant,p1.agei,p1.agef #@008

           #@013 Inicio
           EXECUTE p00_del_tmp_nprx
           EXECUTE p00_del_tmp_eacdc

           EXECUTE p12_ins_tmp_nprx USING p1.fech, l_fant
           EXECUTE p12_upd_tmp_eaccc USING l_fant

           EXECUTE p16_ins_tmp_eacdc USING p1.fech
           EXECUTE p16_ins_tmp_eacdc USING l_fant

           EXECUTE p00_del_tmp_nprx
           EXECUTE p12_ins_tmp_eacdc_nprx USING p1.fech, l_fant
           EXECUTE p12_upd_tmp_eacdc_01 USING  l_fant
           EXECUTE p00_del_tmp_nprx
           EXECUTE p12_ins_tmp_nprx_pcmpc USING l_1fec, p1.fech, l_fant
           EXECUTE p12_upd_tmp_eacdc
           #@013 Fin

           EXECUTE p15_indx01_tmp_eaccc
           EXECUTE p16_indx02_tmp_eaccc

           #@009 ini
           EXECUTE p12_del_tmp_fecha
           CALL f032_buscar_fechas_pc913(1,p1.fech)
           CALL f032_buscar_fechas_pc913(2,p1.fech)

           CALL f030_calcular_tasa_ponderada_cn559()
           #@009 fin

           #@007 ini
           EXECUTE p117_del_tmp_rseg_ini
           EXECUTE p118_ins_tmp_rseg_ini USING p1.fech
           EXECUTE p118_ins_tmp_rseg_ini USING l_fant
           #@007 fin

            #@016 Inicio
            EXECUTE p00_del_tmp_nprx
            EXECUTE p119_ins_tmp_nprx USING l_1fec, p1.fech, p1.fech, l_1fec, p1.fech
            EXECUTE p120_upd_tmp_eacdc_03
            #@016 Fin

           #@011 ini
           EXECUTE p71_del_tmp_cobr
           EXECUTE p72_ins_tmp_cobr USING p1.fech
           #@011 fin
        #END IF

        #CALL f3000_crea_saldo_ea017(p1.tipo)
        CALL f3000_crea_saldo_ea017_v2(p1.tipo)

    #@006 fin
#        CALL f000_declare_cursor()
         #@004 fin
RETURN TRUE
END FUNCTION

#@001 ini
FUNCTION f0310_nombre_sede_ea017(l_valo)
DEFINE	l_valo	SMALLINT,
		l_desc	CHAR(100)

	LET l_desc=NULL

	SELECT pcprmdesc INTO l_desc
	FROM pcprm
	WHERE pcprmflag=92
	AND pcprmdato=0
	AND pcprmvalo=l_valo

RETURN l_desc
END FUNCTION
#@001 fin

#################
# LISTADO IMPRESO
#################
FUNCTION buscar_descripcion(l_agen , l_ambi)
	DEFINE l_agen    INTEGER,
	       l_ambi    SMALLINT,
	       l_desc    CHAR(50)

	IF l_agen = 0 THEN
	   LET l_desc = "CONSOLIDADO"
	ELSE
	   CASE l_ambi
	      WHEN 2
	      SELECT GBCONDESC INTO l_desc
	        FROM GBCON
	       WHERE GBCONPFIJ = 71
	         AND GBCONCORR = l_agen
	      WHEN 3
	      SELECT GBOFIDESC INTO l_desc
	        FROM GBOFI
	       WHERE GBOFINOFI = l_agen
	   END CASE
	   IF STATUS = NOTFOUND THEN
	      LET l_desc = "NO EXISTE"
	   END IF
	END IF

	RETURN l_desc
END FUNCTION





###################
# RUTINAS GENERALES
###################

FUNCTION f6000_limpiar_campos_ea017()
         INITIALIZE p1.* TO  NULL
         INITIALIZE m1.o1, m1.o2, m1.o3, m1.o4 TO NULL
         LET g_raya = "---------------------------------------",
                      "---------------------------------------"
         LET g_raya1 = "---------------------------------------",
                       "---------------------------------------",
                       "---------------------------------------",
                       "--------------------"
         LET g_act = 0
         LET g_pas = 0
         LET g_ing = 0
         LET g_egr = 0
         LET g_res = 0
END FUNCTION

FUNCTION f6050_usuario_ea017()
        SELECT * INTO t0.* FROM gbpmt
        IF status = NOTFOUND OR status < 0 THEN
            RETURN FALSE
        END IF
        IF STATUS = NOTFOUND THEN
            RETURN FALSE
        END IF
        RETURN TRUE
END FUNCTION

FUNCTION f6100_cabecera_ea017()
       DEFINE   l_string CHAR(33),
                l_empres CHAR(33),
                l_sistem CHAR(16),
                l_opcion CHAR(33),
                l_col    SMALLINT


# DISPLAY DEL SISTEMA (16 caracteres)
        LET     l_string = "CONTABILIDAD"
        LET     l_col = ((16 - length(l_string)) / 2)
        LET     l_sistem = " "
        LET     l_sistem[l_col+1,16-l_col] = l_string
        DISPLAY l_sistem AT 4,2

# DISPLAY DEL NOMBRE DE LA EMPRESA (33 caracteres)
        LET     l_string = t0.gbpmtnemp CLIPPED
        LET     l_col = ((33 - length(l_string)) / 2)
        LET     l_empres = " "
        LET     l_empres[l_col+1,33-l_col] = l_string
        DISPLAY l_empres AT 4,24

# DISPLAY DE LA FECHA
        DISPLAY t0.gbpmtfdia USING "dd/mm/yyyy" AT 4,66

# DISPLAY DE LA OPCION (33 caracteres)
        LET     l_string = "EGP CON DISTRIBUCION"
        LET     l_col = ((33 - length(l_string)) / 2)
        LET     l_opcion = " "
        LET     l_opcion[l_col+1,33-l_col] = l_string
        DISPLAY l_opcion AT 5,24
        DISPLAY version AT 22,70

END FUNCTION

FUNCTION f6200_carga_menu_ea017()
   LET m1.d1 = "Generar e Imprimir"
   LET m1.d2 = "Ver en Pantalla"
   LET m1.d3 = "Repetir Impresion"
   LET m1.d4 = "Volver al Menu Anterior"
END FUNCTION

################################################################
# GENERA DATOS PARA EL BALANCE DIRECTAMENTE DE LA CONTABILIDAD #
################################################################
FUNCTION f3000_crea_saldo_ea017()
DEFINE 	l_cnta  CHAR(16),
       	l1	RECORD LIKE EAHRU.*,
       	l2      RECORD LIKE EADRU.*,
       	l_agen  INTEGER,
       	l3	RECORD
		TITU     CHAR(100),
		AGE1     DECIMAL(14,2),
		AGE2     DECIMAL(14,2),
		AGE3     DECIMAL(14,2),
		AGE4     DECIMAL(14,2),
		AGE5     DECIMAL(14,2),
		AGE6     DECIMAL(14,2),
		AGE7     DECIMAL(14,2),
		AGE8     DECIMAL(14,2),
		AGE9     DECIMAL(14,2),
		AG10     DECIMAL(14,2),
		AG11     DECIMAL(14,2),
		AG12     DECIMAL(14,2),
		AG13     DECIMAL(14,2),
		AG14     DECIMAL(14,2),
		AG15     DECIMAL(14,2),
		AG16     DECIMAL(14,2)
		END RECORD,
	l_mope  DECIMAL(14,2),
	l_gadm  DECIMAL(14,2),
	l_valu  DECIMAL(14,2),
	l_oing  DECIMAL(14,2),
	l_SQL   CHAR(300),
	l_cad   CHAR(50)

       DELETE FROM TMP_DATOS
       DELETE FROM TMP_SALDOS
       ##################################
       # CARGA DATOS DE LA CONTABILIDAD #
       ##################################

     { IF p1.vouc = "S" THEN
       INSERT INTO TMP_SALDOS
       SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)
         FROM CNDTR
        WHERE CNDTRFDOC <= p1.fech
          AND CNDTRTDOC NOT IN(96)
          AND CNDTRMRCB = 0
        GROUP BY 1,2
       ELSE
       	 INSERT INTO TMP_SALDOS
       SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)
         FROM CNDTR
        WHERE CNDTRFDOC <= p1.fech
          AND CNDTRTDOC NOT IN(96,55,56)
          AND CNDTRMRCB = 0
        GROUP BY 1,2
       END IF}

     {IF p1.vouc = "S" THEN
        INSERT INTO TMP_SALDOS
       SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)
         FROM CNDTR
        WHERE CNDTRFDOC <= p1.fech
          AND CNDTRTDOC NOT IN(96)
          AND CNDTRMRCB = 0
        GROUP BY 1,2
    else      }
    	 INSERT INTO TMP_SALDOS
       SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)
         FROM CNDTR
        WHERE CNDTRFDOC <= p1.fech
          AND CNDTRTDOC NOT IN(96,55,56)
          AND CNDTRAGEN NOT IN(41,99)
          AND CNDTRMRCB = 0
        GROUP BY 1,2
    #end if
    {IF p1.vouc = "S" THEN
        {INSERT INTO TMP_SALDOS_CENT
         SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)
         FROM CNDTR
        WHERE CNDTRFDOC <= p1.fech
          AND CNDTRTDOC IN(55,56)
          AND CNDTRAGEN NOT IN(41,99)
          AND CNDTRMRCB = 0
        GROUP BY 1,2  }
    #else
    	  INSERT INTO TMP_SALDOS_CENT
         SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)
         FROM CNDTR
        WHERE CNDTRFDOC <= p1.fech
          AND CNDTRTDOC IN(55)
          AND CNDTRAGEN NOT IN(41,99)
          AND CNDTRMRCB = 0
        GROUP BY 1,2
	#end if
	# IF p1.vouc = "S" THEN
        INSERT INTO TMP_SALDOS_AG
         SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)
         FROM CNDTR
        WHERE CNDTRFDOC <= p1.fech
          #AND CNDTRCNTA[1,2]="45"
          #AND CNDTRAGEN <>99
          #AND CNDTRAGEN NOT IN(41,99)
          AND CNDTRTDOC NOT IN(96)
          AND CNDTRMRCB = 0
        GROUP BY 1,2

        INSERT INTO TMP_SALDOS_COBR
         SELECT CNDTRCNTA, CNDTRAGEN ,NVL(SUM(CNDTRIMPI),0)
         FROM CNDTR
        WHERE CNDTRFDOC <= p1.fech
          #AND CNDTRCNTA[1,2]="45"
          #AND CNDTRAGEN <>99
          AND CNDTRTDOC IN(56)
          AND CNDTRAGEN NOT IN(41,99)
          AND CNDTRTDOC NOT IN(96)
          AND CNDTRMRCB = 0
        GROUP BY 1,2
   # else
   # end if
	UPDATE TMP_SALDOS
	SET IMPI = 0
	WHERE IMPI IS NULL

	UPDATE TMP_SALDOS_CENT
	SET IMPI = 0
	WHERE IMPI IS NULL

	UPDATE TMP_SALDOS_AG
	SET IMPI = 0
	WHERE IMPI IS NULL

	#CALL f003_distribucion_cartera_ea017() !!!

	# Se determina la diferencia de cambio total compania para poder activar el flag


        #IF p1.ambi = 1 THEN
           	CALL f000_construir_egp()
        #ELSE
		#LET p1.ambi = 1
		LET g_dcto = (s('510801') + s('51080409') + s('510809')) - (s('410801') + s('41080409') + s('410809'))
		#LET p1.ambi = 3
	        DECLARE Q_NOFI CURSOR FOR
	        SELECT GBOFINOFI
	          FROM GBOFI
	         WHERE GBOFINOFI BETWEEN 1 AND 99

		FOREACH Q_NOFI INTO g_agen
		   CALL f000_construir_egp()
		END FOREACH
	#END IF

	#CALL f004_distribuir_ROF_ea017a(39)
	#CALL f004_distribuir_ROF_ea017a(47)
	#CALL f004_distribuir_ROF_ea017a(48)

	{FOREACH Q_NOFI INTO g_agen
	    SELECT SUM(SALT) INTO l_mope
		   FROM TMP_DATOS
		  WHERE CODI IN (38,39)
		    AND AGEN = g_agen

		UPDATE TMP_DATOS
		   SET SALT = l_mope
		 WHERE AGEN = g_agen
		   AND CODI = 49

		SELECT SUM(SALT) INTO l_gadm
		   FROM TMP_DATOS
		  WHERE CODI IN (50,54,58)
		    AND AGEN = g_agen

		UPDATE TMP_DATOS
		   SET SALT = l_mope - l_gadm
		 WHERE AGEN = g_agen
		   AND CODI = 59

		SELECT SUM(SALT) INTO l_valu
		  FROM TMP_DATOS
		 WHERE CODI IN (60)
		   AND AGEN = g_agen

		UPDATE TMP_DATOS
		   SET SALT = l_mope - l_gadm - l_valu
		 WHERE AGEN = g_agen
		   AND CODI = 70

		SELECT SUM(SALT) INTO l_oing
		  FROM TMP_DATOS
		 WHERE CODI IN (71)
		   AND AGEN = g_agen

		UPDATE TMP_DATOS
		   SET SALT = l_mope - l_gadm - l_valu + l_oing
		 WHERE AGEN = g_agen
		   AND CODI = 73
	END FOREACH
	 }

END FUNCTION

#@004 ini
FUNCTION f3000_crea_saldo_ea017_v2(p_tipo)
DEFINE   p_tipo   SMALLINT,
         l_fant   DATE

   EXECUTE p00_del_tmp_datos
   EXECUTE p01_del_tmp_saldos
   EXECUTE p29_del_tb_agenc

   EXECUTE p63_del_tmp_saldos_cent
   EXECUTE p64_del_tmp_saldos_ag
   EXECUTE p65_del_tmp_saldos_cobr

   EXECUTE p66_del_tmp_cndtr

   IF p_tipo = 1 THEN
      EXECUTE p67_ins_tmp_cndtr_acum USING p1.fech,p1.agei,p1.agef #@008
   ELSE
      LET l_fant = p1.fech - DAY(p1.fech) + 1
      EXECUTE p68_ins_tmp_cndtr_mens USING l_fant,p1.fech,p1.agei,p1.agef #@008
   END IF

   EXECUTE p69_ind_tmp_cndtr
   EXECUTE p02_ins_tmp_saldos
   EXECUTE p04_ins_tmp_saldos_cent
   EXECUTE p06_ins_tmp_saldos_ag
   EXECUTE p08_ins_tmp_saldos_cobr

	UPDATE TMP_SALDOS
	SET IMPI = 0
	WHERE IMPI IS NULL

	UPDATE TMP_SALDOS_CENT
	SET IMPI = 0
	WHERE IMPI IS NULL

	UPDATE TMP_SALDOS_AG
	SET IMPI = 0
	WHERE IMPI IS NULL

   EXECUTE p30_indx_tmp_saldos
   EXECUTE p31_indx_tmp_saldos_cent
   EXECUTE p32_indx_tmp_saldos_ag
   EXECUTE p33_indx_tmp_saldos_cobr
	#CALL f003_distribucion_cartera_ea017()

   #@005 ini
   IF (p1.vouc = 'N') THEN #---No considerar datos de central
      EXECUTE p63_del_tmp_saldos_cent
   END IF
   #@005 fin

   IF (g_tipo IS NULL) THEN #@005
      MESSAGE " Construyendo EGP ..!"
   END IF #@005
	# Se determina la diferencia de cambio total compania para poder activar el flag
   #CALL f000_construir_egp()
   #LET g_dcto = (s('510801') + s('51080409') + s('510809')) - (s('410801') + s('41080409') + s('410809'))

	FOREACH q10_sel_gbofi USING p1.agei,p1.agef INTO g_agen #@008
      CALL f000_construir_egp()
	END FOREACH

	{UNLOAD TO "TMP_DATOS.TXT"
	SELECT * FROM TMP_DATOS}

END FUNCTION
#@004 fin

FUNCTION f3000_crea_temporal_ea017()

   CREATE TEMP TABLE tmp_eaccc(
   fcie  date,
   agen  integer,
   npre  integer,
   sals  decimal(14,2),
   cana  integer,
   esta  char(20),
   pdvg  decimal(14,2),
   pros  DECIMAL(14,2),
   pcls  DECIMAL(14,2),
   fcas  DATE,
   tea0  FLOAT, #@009
   datr  INTEGER #@009
   ) WITH NO LOG;

   CREATE TEMP TABLE TMP_DATOS
	(
	AGEN     INTEGER,
	CODI     SMALLINT,
	DRUB     CHAR(100),
	SALS     DECIMAL(14,2),
	SALD     DECIMAL(14,2),
	SALT     DECIMAL(14,2)
	)WITH NO LOG

	CREATE TEMP TABLE TMP_SALDOS
	(
	CNTA     CHAR(16),
	AGEN     SMALLINT,
	IMPI     DECIMAL(14,2)
	) WITH NO LOG

	#CREATE INDEX TMP_SALDOS_01 ON TMP_SALDOS(CNTA,AGEN)
	#TMP_SALDOS_CENT
	CREATE TEMP TABLE TMP_SALDOS_CENT
	(
	CNTA     CHAR(16),
	AGEN     SMALLINT,
	IMPI     DECIMAL(14,2)
	) WITH NO LOG
	#CREATE INDEX TMP_SALDOS_CEN_01 ON TMP_SALDOS_CENT(CNTA,AGEN)

	CREATE TEMP TABLE TMP_SALDOS_AG
	(
	CNTA     CHAR(16),
	AGEN     SMALLINT,
	IMPI     DECIMAL(14,2)
	) WITH NO LOG
	#CREATE INDEX TMP_SALDOS_AG_01 ON TMP_SALDOS_AG(CNTA,AGEN)

   CREATE TEMP TABLE TMP_SALDOS_COBR(
   CNTA     CHAR(16),
	AGEN     SMALLINT,
	IMPI     DECIMAL(14,2)
	) WITH NO LOG
   #CREATE INDEX TMP_SALDOS_CB_01 ON TMP_SALDOS_COBR(CNTA,AGEN)

    #@013 inicio
   CREATE TEMP TABLE tmp_pctcn_02(
   pctcnftra   DATE,--
   pctcnnpre  INTEGER,        -- numero de prestamo
   pctcnttrn  SMALLINT,       -- tipo de transaccion
   pctcnpref  SMALLINT,       -- prefijo
   pctcnccon  SMALLINT,       -- concepto
   pctcncctb  CHAR(16),       -- cuenta
   pctcnimpi  DECIMAL(14,2),  -- importe
   pctcnagen  INTEGER,        -- agencia
   pcmpcrseg 	INTEGER        -- analista
   )WITH NO LOG;

   CREATE TEMP TABLE tmp_nprx(-- para actualizar tabla tmp_eaccc
   npre  integer,             -- numero de prestamo
   cana  integer              -- numero del analista
   ) WITH NO LOG;

   CREATE TEMP TABLE tmp_eacdc(
   eacdcfreg DATE,         -- fecha de registro
   eacdcagen SMALLINT,     -- numero de agencia
   eacdccana INTEGER,      -- codigo de canalista
   eacdcnpre INTEGER,      -- numero de prestamo
   eacdcpdvg DECIMAL(14,2) -- devengado
   )WITH NO LOG;
   #@013 fin

    #@004 ini
    CREATE TEMP TABLE TMP_DATOS_CANA(
    CODI     SMALLINT,
    AGEN     SMALLINT,
    CANA     INTEGER,
    SALT     DECIMAL(14,2)
    )WITH NO LOG;

   CREATE TEMP TABLE tmp_pctcn(
   tipo  SMALLINT, #@012 1:Ingreos 2:Posteos
   cctb  CHAR(16), #@012 --Cuentas
   agen  SMALLINT,
   npre  INTEGER,
   cana  INTEGER,
   impi  DECIMAL(14,2)
   )WITH NO LOG;

   CREATE TEMP TABLE tmp_sudli(
   cemp  INTEGER,
   uneg  SMALLINT,
   rseg  INTEGER,
   impt  DECIMAL(14,2)
   )WITH NO LOG;

   CREATE TEMP TABLE tmp_codia(
   tipo  SMALLINT, #--0 Mes Anterior 1 Mes Actual
   rseg  INTEGER,
   cemp  INTEGER
   )WITH NO LOG;

   CREATE TEMP TABLE tb_agenc(
   agen  SMALLINT
   )WITH NO LOG

   #@008 ini
   {CREATE TEMP TABLE tmp_varp(
   fcie    DATE,
   agen    SMALLINT,
   npre    INTEGER,
   cana    INTEGER,
   tpov    DECIMAL(14,2),
   texp    SMALLINT,
   vari    DECIMAL(14,2)
   ) with no LOG;}

   CREATE TEMP TABLE tmp_varp(
   agen    SMALLINT,
   npre    INTEGER,
   cana    INTEGER,
   impo    DECIMAL(14,2)
   ) with no LOG;
   #@008 fin

   CREATE TEMP TABLE tmp_vari(
   npre  INTEGER,
   vari  DECIMAL(14,2)
   ) with no log;

   CREATE TEMP TABLE tmp_cndtr(
   CNDTRCNTA   CHAR(16),
   CNDTRAGEN   SMALLINT,
   CNDTRTDOC   SMALLINT,
   CNDTRIMPI   DECIMAL(14,2)
   ) WITH NO LOG;

   CREATE TEMP TABLE tmp_cobr(
   agen  SMALLINT,
   sals  DECIMAL(14,2)
   )WITH NO LOG;

   CREATE TEMP TABLE tmp_rseg(
   agen  SMALLINT,
   rseg  INTEGER
   ) WITH NO LOG

   CREATE TEMP TABLE tmp_ingf(
   agen  SMALLINT,
   cana  INTEGER,
   dvg1  DECIMAL(14,2),
   dvg2  DECIMAL(14,2),
   ingm  DECIMAL(14,2),
   dife  DECIMAL(14,2), #@008
   post  DECIMAL(14,2), #@012
   ingf  DECIMAL(14,2)
   ,mo19  DECIMAL(14,2) #@013
   ) WITH NO LOG;
   #@004 fin

   CREATE TEMP TABLE tmp_ajus(
   agen SMALLINT,
   cana INTEGER,
   impt DECIMAL(14,2)
   ) WITH NO LOG;

   #@005 ini
   create TEMP table tmp_cnegs(
   ntra integer,
   tipo smallint, --1.Acumulado 2.Mensual
   age1 smallint,
   cod1 smallint,
   sal1 decimal(14,2), --Saldo central
   sal2 decimal(14,2) --Saldo sin central
   ) WITH NO LOG;

   create TEMP table tmp_cnega(
   ntra integer,
   tipo smallint, --1.Acumulado 2.Mensual
   subt smallint, --1.Sin cobranzas 2.Cobranzas
   age1 smallint,
   rse1 integer,
   cod1 smallint,
   sal1 decimal(14,2), --Saldo central
   sal2 decimal(14,2) --Saldo sin central
   ) WITH NO LOG;
   #@005 fin

   #@006 ini
   create TEMP table tmp_sedes(
   nofi SMALLINT,
   desc CHAR(40),
   valo char(150)
   ) WITH NO LOG;
   #@006 fin

   #@007 ini
   CREATE TEMP TABLE tmp_adeu(
   acre INTEGER,
   tea0 FLOAT,
   sald DECIMAL(14,2)
   ) WITH NO LOG;

   CREATE TEMP TABLE tmp_rseg_ini(
   agen SMALLINT,
   cana INTEGER
   ) WITH NO LOG;

   CREATE TEMP TABLE tmp_area(
   area SMALLINT,
   agen SMALLINT,
   nume SMALLINT,
   item SMALLINT
   ) WITH NO LOG;

   CREATE TEMP TABLE tmp_subt(
   codi SMALLINT,
   subt SMALLINT,
   porc FLOAT,
   impt DECIMAL(14,2),
   drub CHAR(100)
   ) WITH NO LOG;
   #@007 fin

   #@008 ini
   CREATE TEMP TABLE tmp_rseo(
    rseo    INTEGER,
    ageo    SMALLINT,
    sals    DECIMAL(14,2)
   ) WITH NO LOG;

   CREATE TEMP TABLE tmp_pvrep(
   ftra DATE,
   tipo SMALLINT,
   agen SMALLINT,
   cana INTEGER,
   npre INTEGER,
   impo DECIMAL(14,2)
   ) WITH NO LOG;

   CREATE TEMP TABLE tmp_ings(
   tipo    SMALLINT,#1:Desgrav 2:Microseguro
   agen    SMALLINT,
   cana    INTEGER,
   impo    DECIMAL(14,2)
   ) with no LOG;

   CREATE TEMP TABLE tmp_gfin(
   tipo     SMALLINT, #1:Corresponsalia
   agen     SMALLINT,
   cana     INTEGER,
   impo     DECIMAL(14,2)
   ) WITH NO LOG;

   CREATE TEMP TABLE tmp_pcpfu(
    npre    INTEGER,
    npfu    INTEGER,
    ntra    INTEGER
    ) WITH NO LOG;

   CREATE TEMP TABLE tmp_pctdt(
    agen    SMALLINT,
    cana    INTEGER,
    impt    DECIMAL(14,2)
    )WITH NO LOG;
   #@008 fin

   #@009 ini
   CREATE TEMP TABLE tmp_tasp(
     tipo   SMALLINT, #-1:TPP 2:TR
     agen   SMALLINT,
     cana   INTEGER,
     tea0   FLOAT
    )WITH NO LOG;

    CREATE TEMP TABLE tmp_fecx(
      tipo  SMALLINT, #1:Sede 2:Analista
      fcie  DATE
     )WITH NO LOG;
   #@009 fin

   #@015 ini
   CREATE TEMP TABLE tmp_pctao(
     agen SMALLINT,
     rseg INTEGER,
     fdes DATE,
     fila INTEGER
    ) WITH NO LOG;
   #@015 fin

END FUNCTION

FUNCTION f000_construir_egp()
DEFINE l1  RECORD
	   AGEN     INTEGER,
	   CODI     SMALLINT,
	   DRUB     CHAR(100),
	   SALS     DECIMAL(14,2),
	   SALD     DECIMAL(14,2),
	   SALT     DECIMAL(14,2)
	   END RECORD,
       l_sald   DECIMAL(14,2),
       l_agru1  DECIMAL(14,2),
       l_agru2  DECIMAL(14,2),
       l_agru3  DECIMAL(14,2),
       l_agru4  DECIMAL(14,2),
       l_agru5  DECIMAL(14,2),
       l_agru6  DECIMAL(14,2),
       l_agru7  DECIMAL(14,2),
       i	SMALLINT,
       l_infs   DECIMAL(14,2),  -- ingreso financiero en soles
       l_infd   DECIMAL(14,2),  -- ingreso financiero en dolares
       l_inft   DECIMAL(14,2),  -- ingreso financiero total
       l_gafs   DECIMAL(14,2),  -- gasto financiero en soles
       l_gafd   DECIMAL(14,2),  -- gasto financiero en dolares
       l_gaft   DECIMAL(14,2),  -- gasto financiero total
       l_mabs   DECIMAL(14,2),  -- margen bruto en soles
       l_mabd   DECIMAL(14,2),  -- margen bruto en dolares
       l_mabt   DECIMAL(14,2),  -- margen bruto total
       l_mans   DECIMAL(14,2),  -- margen neto en soles
       l_mand   DECIMAL(14,2),  -- margen neto en dolares
       l_mant   DECIMAL(14,2),  -- margen neto total
       l_mons   DECIMAL(14,2),  -- margen operacional neto en soles
       l_mond   DECIMAL(14,2),  -- margen operacional neto en dolares
       l_mont   DECIMAL(14,2),  -- margen operacional neto total
       l_maos   DECIMAL(14,2),  -- margen operacional en soles
       l_maod   DECIMAL(14,2),  -- margen operacional en dolares
       l_maot   DECIMAL(14,2),  -- margen operacional total
       l_dtct   DECIMAL(14,2),  -- diferencia de tipo de cambio total
       l_dtcs   DECIMAL(14,2),  -- diferencia de tipo de cambio soles
       l_dtcd   DECIMAL(14,2),  -- diferencia de tipo de cambio dolares

       l_gxfs   DECIMAL(14,2),
       l_gxfd   DECIMAL(14,2),
       l_gxft   DECIMAL(14,2),
       l_prot   DECIMAL(14,2),
       l_pros   DECIMAL(14,2),
       l_prod   DECIMAL(14,2),
       l_ixfs   DECIMAL(14,2),
       l_ixfd   DECIMAL(14,2),
       l_ixft   DECIMAL(14,2),

       l_roft   DECIMAL(14,2),
	   l_rofs   DECIMAL(14,2),
	   l_rofd   DECIMAL(14,2),

       l_SQL    CHAR(300),
       l_cad    CHAR(80)

#DIFERENCIA DE CAMBIO


# Diferencia de Cambio de Operaciones Varias
	LET l1.AGEN = g_agen
	LET l1.DRUB = "Diferencia de Cambio de Operaciones Varias"
	LET g_cmon = 0
	LET l_dtct = (s('510801') + s('51080409') + s('510809')) - (s('410801') + s('41080409') + s('410809'))
        LET g_cmon = 1
	LET l_dtcs = (s('510801') + s('51080409') + s('510809')) - (s('410801') + s('41080409') + s('410809'))
        LET g_cmon = 2
	LET l_dtcd = (s('510801') + s('51080409') + s('510809')) - (s('410801') + s('41080409') + s('410809'))

# INGRESOS FINANCIEROS
	LET l1.AGEN = g_agen
	LET l1.CODI = 2
	#LET l1.DRUB = "INGRESOS FINANCIEROS"
	LET l1.DRUB = "INGRESOS POR INTERESES AGENCIAS"
	FOR i = 0 TO 2
	  LET g_cmon = i
		#LET l_agru1 = s('510904') - s('410904')
		LET l_agru1 = s('5109170102') + s('5109170202') + s('51091703') + s('5109240101')

		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF

    LET l_sald = s('5101') + s('5102') + s('510302') + s('510304') + s('510305') + s('5104') + s('5107') + s('510901') + l_agru1 -( s('4109170102')+s('4109170202')+s('41091703')+s('4109240101')+s('41092403'))
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
		LET l_agru1 = 0
		LET l_agru2 = 0
		LET l_agru3 = 0
		LET l_agru4 = 0
		LET l_agru5 = 0
		LET l_agru6 = 0
		LET l_agru7 = 0
	END FOR
	##############################################
	# SE GUARDA LOS INGRRESOS FINANCIEROS EN LAS #
	# VARIABLES CORRESPONDIENTES                 #
	##############################################
	LET l_infs = l1.SALS
	LET l_infd = l1.SALD
	LET l_inft = l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	############################################ INGRESOS POR INTERES CENTRAL #############################################
	# INGRESOS FINANCIEROS
	LET l1.AGEN = g_agen
	#LET l1.CODI = 76
	LET l1.CODI = 12

	#LET l1.DRUB = "INGRESOS FINANCIEROS"
	LET l1.DRUB = "INGRESOS POR INTERESES CENTRAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		#LET l_agru1 = s('510904') - s('410904')
		LET l_agru1 = sd('5109170102') + sd('5109170202') + sd('51091703') + sd('5109240101')

		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF

		LET l_sald = sd('5101') + sd('5102') + sd('510302') + sd('510304') + sd('510305') + sd('5104') + sd('5107') + sd('510901') + l_agru1 -( sd('4109170102')+sd('4109170202')+sd('41091703')+sd('4109240101')+sd('41092403'))
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
		LET l_agru1 = 0
		LET l_agru2 = 0
		LET l_agru3 = 0
		LET l_agru4 = 0
		LET l_agru5 = 0
		LET l_agru6 = 0
		LET l_agru7 = 0
	END FOR
	##############################################
	# SE GUARDA LOS INGRRESOS FINANCIEROS EN LAS #
	# VARIABLES CORRESPONDIENTES                 #
	##############################################
	#LET l_infs = l1.SALS
	#LET l_infd = l1.SALD
	#LET l_inft = l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

    ############################################ INGRESOS POR INTERES TOTAL #############################################
	# INGRESOS FINANCIEROS
	LET l1.AGEN = g_agen
	#LET l1.CODI = 77
	LET l1.CODI = 1
	#LET l1.DRUB = "INGRESOS FINANCIEROS"
	LET l1.DRUB = "INGRESOS POR INTERESES TOTAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		#LET l_agru1 = s('510904') - s('410904')
		LET l_agru1 = sg('5109170102') + sg('5109170202') + sg('51091703') + sg('5109240101')

		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF

		LET l_sald = sg('5101') + sg('5102') + sg('510302') + sg('510304') + sg('510305') + sg('5104') + sg('5107') + sg('510901') + l_agru1 -( sg('4109170102')+sg('4109170202')+sg('41091703')+sg('4109240101')+sg('41092403'))
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
		LET l_agru1 = 0
		LET l_agru2 = 0
		LET l_agru3 = 0
		LET l_agru4 = 0
		LET l_agru5 = 0
		LET l_agru6 = 0
		LET l_agru7 = 0
	END FOR
	##############################################
	# SE GUARDA LOS INGRRESOS FINANCIEROS EN LAS #
	# VARIABLES CORRESPONDIENTES                 #
	##############################################
	LET l_ixfs = l1.SALS
	LET l_ixfd = l1.SALD
	LET l_ixft = l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	############################################ INGRESOS POR INTERES TOTAL #############################################

# INTERESES POR DISPONIBLES
	LET l1.AGEN = g_agen
	LET l1.CODI = 3
	LET l1.DRUB = "Disponible"
	FOR i = 0 TO 2
	    LET g_cmon = i
		LET l_sald = s('5101')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
# "INTERESES Y COMISIONES POR FONDOS INTERBANCARIOS"
	LET l1.AGEN = g_agen
	LET l1.CODI = 4
	#LET l1.DRUB = "INTERESES Y COMISIONES POR FONDOS INTERBANCARIOS"
	LET l1.DRUB = "Fondos interbancarios"
	FOR i = 0 TO 2
	        LET g_cmon = i
		#LET l_sald = s('5102') + s('510702')
		LET l_sald = s('5102')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

# "INGRESOS POR INVERSIONES NEGOCIABLES Y A VENCIMIENTO"
	LET l1.AGEN = g_agen
	LET l1.CODI = 5
	LET l1.DRUB = "Inversiones a valor razonable con cambios en resultados"
	#LET l1.DRUB = "INGRESOS POR INVERSIONES NEGOCIABLES Y A VENCIMIENTO"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510302')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

# "INGRESOS POR VALORIZACION DE INVERSIONES NEGOCIABLES Y A VENCIMIENTO"
	LET l1.AGEN = g_agen
	LET l1.CODI = 6
	LET l1.DRUB = "Inversiones disponibles para las ventas"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510304')
		IF l_sald > 0 THEN
		   LET l_sald = 0
		END IF
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

# "Intereses y Comisiones por Cartera de Créditos"
	LET l1.AGEN = g_agen
	LET l1.CODI = 7
	LET l1.DRUB = "Inversiones a vencimientos"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510305')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

# "Ingresos de Cuentas por Cobrar"
	LET l1.AGEN = g_agen
	LET l1.CODI = 8
	LET l1.DRUB = "Cartera de Créditos directos"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('5104') + s('5107')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

# "Ganancias por Inversiones en Subsidiarias y Asociadas"
	LET l1.AGEN = g_agen
	LET l1.CODI = 9
	LET l1.DRUB = "Resultado por operaciones de cobertura"
	FOR i = 0 TO 2
	    LET g_cmon = i
		LET l_agru1 = s('5109170102') + s('5109170202') + s('51091703') + s('5109240101')

		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF
		LET l_sald =  l_agru1 -( s('4109170102')+s('4109170202')+s('41091703')+s('4109240101')+s('41092403'))
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
		LET l_agru1 = 0
		LET l_agru2 = 0
		LET l_agru3 = 0
		LET l_agru4 = 0
		LET l_agru5 = 0
		LET l_agru6 = 0
		LET l_agru7 = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

# "Otras Comisiones"
	LET l1.AGEN = g_agen
	LET l1.CODI = 10
	LET l1.DRUB = "Cuentas por cobrar"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510504')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

# Reajuste por indexaciÓn
	LET l1.AGEN = g_agen
	LET l1.CODI = 11
	LET l1.DRUB = "Otros ingresos financieros"

	FOR i = 0 TO 2

	    LET g_cmon = i
	    LET l_agru1 = s('510904') - s('410904')
		IF l_agru1 < 0 THEN
		   LET l_agru1 = 0
		END IF
		LET l_sald = s('510901') +s('510908')  + l_agru1
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004


# GASTOS FINANCIEROS
	LET l1.AGEN = g_agen
	LET l1.CODI = 14
	LET l1.DRUB = "GASTOS POR INTERESES AGENCIAS"
	FOR i = 0 TO 2
	    LET g_cmon = i

		LET l_agru1 = s('5109170102')+ s('5109170102')+ s('51091703')+s('51092403')
		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF

		LET l_sald = s('4101') + s('4102') + s('4103') + s('410401') + s('410402') + s('410403')+ s('410404')+ s('410405')+ s('410406')+ s('410407')+ s('4107')+ s('4106')+ s('4105')+ l_agru1 + s('4109170102') + s('4109170202') + s('41091703') + s('4109240101') + s('41092403') + s('410925') #@002
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
		LET l_agru1 = 0
		LET l_agru2 = 0
		LET l_agru3 = 0
		LET l_agru4 = 0
		LET l_agru5 = 0
		LET l_agru6 = 0
		LET l_agru7 = 0
	END FOR
	###########################################
	# SE GUARDA LOS GASTOS FINANCIEROS EN LAS #
	# VARIABLES CORRESPONDIENTES              #
	###########################################
	LET l_gafs = l1.SALS
	LET l_gafd = l1.SALD
	LET l_gaft = l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004


	############################################ GASTOS FINANCIEROS CENTRAL ###########################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 29
	LET l1.DRUB = "GASTOS POR INTERESES CENTRAL"
	FOR i = 0 TO 2
	    LET g_cmon = i

		LET l_agru1 = sd('5109170102')+ sd('5109170102')+ sd('51091703')+sd('51092403')
		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF

		LET l_sald = sd('4101') + sd('4102') + sd('4103') + sd('410401') + sd('410402') + sd('410403')+ sd('410404')+ sd('410405')+ sd('410406')+ sd('410407')+ sd('4107')+ sd('4106')+ sd('4105')+ l_agru1 + sd('4109170102') + sd('4109170202') + sd('41091703') + sd('4109240101') + sd('41092403') + sd('410925') #@002
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
		LET l_agru1 = 0
		LET l_agru2 = 0
		LET l_agru3 = 0
		LET l_agru4 = 0
		LET l_agru5 = 0
		LET l_agru6 = 0
		LET l_agru7 = 0
	END FOR
	###########################################
	# SE GUARDA LOS GASTOS FINANCIEROS EN LAS #
	# VARIABLES CORRESPONDIENTES              #
	###########################################
	#LET l_gafs = l1.SALS
	#LET l_gafd = l1.SALD
	#LET l_gaft = l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	############################################ GASTOS FINANCIEROS CENTRAL ###########################################

	############################################ GASTOS FINANCIEROS TOTAL ###########################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 13
	LET l1.DRUB = "GASTOS POR INTERESES TOTAL"
	FOR i = 0 TO 2
	    LET g_cmon = i

		LET l_agru1 = sg('5109170102')+ sg('5109170102')+ sg('51091703')+sg('51092403')
		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF

		LET l_sald = sg('4101') + sg('4102') + sg('4103') + sg('410401') + sg('410402') + sg('410403')+ sg('410404')+ sg('410405')+ sg('410406')+ sg('410407')+ sg('4107')+ sg('4106')+ sg('4105')+ l_agru1 + sg('4109170102') + sg('4109170202') + sg('41091703') + sg('4109240101') + sg('41092403') + sg('410925') #@002
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
		LET l_agru1 = 0
		LET l_agru2 = 0
		LET l_agru3 = 0
		LET l_agru4 = 0
		LET l_agru5 = 0
		LET l_agru6 = 0
		LET l_agru7 = 0
	END FOR
	###########################################
	# SE GUARDA LOS GASTOS FINANCIEROS EN LAS #
	# VARIABLES CORRESPONDIENTES              #
	###########################################
	LET l_gxfs = l1.SALS
	LET l_gxfd = l1.SALD
	LET l_gxft = l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	############################################ GASTOS FINANCIEROS CENTRAL ###########################################

# Intereses y Comisiones por Obligaciones con el Público
	LET l1.AGEN = g_agen
	LET l1.CODI = 15
	LET l1.DRUB = "Obligaciones con el Público"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4101')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
# Intereses y Comisiones por Fondos Interbancarios
	LET l1.AGEN = g_agen
	LET l1.CODI = 16
	LET l1.DRUB = "Fondos Interbancarios"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4102')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	# Intereses y Comisiones por Fondos Interbancarios
	LET l1.AGEN = g_agen
	LET l1.CODI = 17
	LET l1.DRUB = "Depósitos de Empresas del Sistema Financiero y organismos Financieros"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4103')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Intereses por Adeudos y Obligaciones del Sistema Financiero del País
	LET l1.AGEN = g_agen
	LET l1.CODI = 18
	LET l1.DRUB = "Adeudos y Obligaciones del Sistema Financiero"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410401') + s('410402') + s('410403')+ s('410404')+ s('410405') + s('410406') + s('410407')+ s('4107')+ s('4106')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Intereses por Adeudos y Obligaciones con Instituciones Financieras del Exter. y Organ. Financ. Internac.
	LET l1.AGEN = g_agen
	LET l1.CODI = 19
	LET l1.DRUB = "ADEUDOS Y OBLIGACIONES CON EL BANCO CENTRAL DE RESERVA DEL PERU"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410401')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Intereses de Otros Adeudos y Obligaciones del País y del Exterior.
	LET l1.AGEN = g_agen
	LET l1.CODI = 20
	LET l1.DRUB = "ADEUDOS Y OBLIGACION DEL SISTEMA FINANCIERO DEL PAIS"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410402') + s('410403')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Intereses, Comisiones y otros cargos de Cuentas por Pagar
	LET l1.AGEN = g_agen
	LET l1.CODI = 21
	LET l1.DRUB = "ADEUDOS Y OBLIGACION CON INSTITUCIONES FINANCIERAS DEL EXT. Y ORGAN. "
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410404') + s('410405')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Intereses por Valores, Títulos y Obligaciones en CirculaciÓn
	LET l1.AGEN = g_agen
	LET l1.CODI = 22
	LET l1.DRUB = "Otros Adeudos y Obligaciones del  País y del Exterior."
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410406') +  s('410407')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Comisiones y Otros Cargos por Obligaciones Financieras
	LET l1.AGEN = g_agen
	LET l1.CODI = 23
	LET l1.DRUB = "COMISIONES Y OTROS CARGOS POR ADEUDOS Y OBLIGACIONES FINANCIERAS"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4107')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Reajuste por Indexación
	LET l1.AGEN = g_agen
	LET l1.CODI = 24

	LET l1.DRUB = "VALORES, TITULOS Y OBLIGACIONES EN CIRCULACION "
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4106')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Pérdida de operaciones de cobertura
	LET l1.AGEN = g_agen
	LET l1.CODI = 25
	LET l1.DRUB = "Cuentas por pagar"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4105')
		IF l_sald < 0 THEN
		   LET l_sald = 0
		END IF
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Primas para el Fondo de Seguro de Depósitos
	LET l1.AGEN = g_agen
	LET l1.CODI = 26
	LET l1.DRUB = "Intereses de cuentas por pagar"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4105')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Ganancia de operaciones de cobertura
	LET l1.AGEN = g_agen
	LET l1.CODI = 27
	LET l1.DRUB = "Resultado por operaciones de cobertura"
	FOR i = 0 TO 2
	        LET g_cmon = i
		#LET l_agru1 = s('510904') - s('410904')
		LET l_agru1 = s('5109170102') + s('5109170202') + s('51901703') + s('51902403')

		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF
		LET l_sald = l_agru1 -( s('4109170102')+s('4109170202')+s('41091703')+s('4109240101')+s('41092403'))
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
		LET l_agru1 = 0
		LET l_agru2 = 0
		LET l_agru3 = 0
		LET l_agru4 = 0
		LET l_agru5 = 0
		LET l_agru6 = 0
		LET l_agru7 = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Otros Gastos Financieros
	LET l1.AGEN = g_agen
	LET l1.CODI = 28
	LET l1.DRUB = "Otros Gastos Financieros"
	LET l_agru1 = s('510925') - s('410925')
		IF l_agru1 >  0 THEN
		   LET l_agru1 = 0
		END IF
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410901') + s('410908') + s('410909') + s('410922') + s('410923') + s('410925') + l_agru1	#@002
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    #Margen Financiero Bruto
	LET l1.AGEN = g_agen
	LET l1.CODI = 30
	LET l1.DRUB = "MARGEN FINANCIERO BRUTO"
	LET l1.SALT = l_inft - l_gaft
	LET l1.SALS = l_infs - l_gafs
	LET l1.SALD = l_infd - l_gafd
	##########################################
	# SE GUARDA EL MARGEN BRUTO EN VARIABLES #
	##########################################
	LET l_mabs = l1.SALS
	LET l_mabd = l1.SALD
	LET l_mabt = l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)

	###################################### Margen Financiero Bruto Final ######################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 31
	LET l1.DRUB = "MARGEN FINANCIERO BRUTO FINAL"
	LET l1.SALT = l_ixft - l_gxft
	LET l1.SALS = l_ixfs - l_gxfs
	LET l1.SALD = l_ixfd - l_gxfd
	LET l_maxt = l1.SALT
	LET l_maxs = l1.SALS
	LET l_maxd = l1.SALD

	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	###################################### Margen Financiero Bruto Final ######################################

    #Provisiones para Incobrabilidad de Créditos del Ejercicio
	LET l1.AGEN = g_agen
	LET l1.CODI = 33
	LET l1.DRUB = "Provisiones para Créditos directos Agencias"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4302') +s('4305050401') -  s('510927') - s('540401')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_mant = l_mabt - l1.SALT
	LET l_mans = l_mabs - l1.SALS
	LET l_mand = l_mabd - l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    #Margen Financiero Neto
	LET l1.AGEN = g_agen
	LET l1.CODI = 35
	LET l1.DRUB = "MARGEN FINANCIERO NETO"
	LET l1.SALT = l_mant
	LET l1.SALS = l_mans
	LET l1.SALD = l_mand
	#INSERT INTO TMP_DATOS VALUES(l1.*)
    #INGRESOS POR SERVICIOS FINANCIEROS
	LET l1.AGEN = g_agen
	LET l1.CODI = 38
	LET l1.DRUB = "INGRESOS POR SERVICIOS FINANCIEROS AGENCIAS"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('5201') + s('520204') + s('520205') + s('5202') - s('520204') - s('520205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_maos = l_mans + l1.SALS
	LET l_maod = l_mand + l1.SALD
	LET l_maot = l_mant + l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    ################################# INGRESOS POR SERVICIOS FINANCIEROS CENTRAL ###################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 42
	LET l1.DRUB = "INGRESOS POR SERVICIOS FINANCIEROS CENTRAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('5201') + sd('520204') + sd('520205') + sd('5202') - sd('520204') - sd('520205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	################################# INGRESOS POR SERVICIOS FINANCIEROS CENTRAL ###################################



    ################################### Provisiones de Créditos directos Central ###################################
    LET l1.AGEN = g_agen
	LET l1.CODI = 34
	LET l1.DRUB = "Provisiones para Créditos directos Central"
	FOR i = 0 TO 2
	    LET g_cmon = i
		LET l_sald = sd('4302') +sd('4305050401') -  sd('510927') - sd('540401')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#LET l_mant = l_mabt - l1.SALT
	#LET l_mans = l_mabs - l1.SALS
	#LET l_mand = l_mabd - l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    ################################### Provisiones de Créditos directos Central ###################################

    ################################### Provisiones de Créditos directos Total ###################################
    LET l1.AGEN = g_agen
	LET l1.CODI = 32
	LET l1.DRUB = "Provisiones para Créditos directos Total"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sg('4302') +sg('4305050401') -  sg('510927') - sg('540401')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_prot = l1.SALT
	LET l_pros = l1.SALS
	LET l_prod = l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    ################################### Provisiones de Créditos directos Central ###################################

    ################################### Margen Financiero Neto ###################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 36
	LET l1.DRUB = "MARGEN FINANCIERO NETO FINAL"

	LET l1.SALT = l_ixft - l_gxft - l_prot
	LET l1.SALS = l_ixfs - l_gxfs - l_pros
	LET l1.SALD = l_ixfd - l_gxfd - l_prod
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	LET l_maxt = l1.SALT
	LET l_maxs = l1.SALS
	LET l_maxd = l1.SALD

    ################################### Margen Financiero Neto ###################################

    ################################# INGRESOS POR SERVICIOS FINANCIEROS TOTAL ###################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 37
	LET l1.DRUB = "INGRESOS POR SERVICIOS FINANCIEROS TOTAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sg('5201') + sg('520204') + sg('520205') + sg('5202') - sg('520204') - sg('520205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_maxs = l_maxs + l1.SALS
	LET l_maxd = l_maxd + l1.SALD
	LET l_maxt = l_maxt + l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	################################# INGRESOS POR SERVICIOS FINANCIEROS CENTRAL ###################################

#Ingresos por Operaciones Contingentes
	LET l1.AGEN = g_agen
	LET l1.CODI = 39
	LET l1.DRUB = "Ingresos por créditos indirectos"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('5201')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Ingresos por Fideicomisos y Comisiones de Confianza
	LET l1.AGEN = g_agen
	LET l1.CODI = 40
	LET l1.DRUB = "Gastos por Fideicomisos y Comisiones de Confianza"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('520204') + s('520205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Ingresos Diversos
	LET l1.AGEN = g_agen
	LET l1.CODI = 41
	LET l1.DRUB = "Ingresos Diversos"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('5202') - (s('520204') + s('520205'))
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#GASTOS POR SERVICIOS FINANCIEROS
	LET l1.AGEN = g_agen
	LET l1.CODI = 44
	LET l1.DRUB = "GASTOS POR SERVICIOS FINANCIEROS AGENCIA"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4201') + s('410907') + s('420204') + s('420205')+ s('410905')+ s('4202')- s('420204')- s('420205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR

	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	LET l_maos = l_maos - l1.SALS
	LET l_maod = l_maod - l1.SALD
	LET l_maot = l_maot - l1.SALT

    ################################ GASTOS POR SERVICIOS FINANCIEROS CENTRAL ################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 49
	LET l1.DRUB = "GASTOS POR SERVICIOS FINANCIEROS CENTRAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('4201') + sd('410907') + sd('420204') + sd('420205')+ sd('410905')+ sd('4202')- sd('420204')- sd('420205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR

	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#LET l_maos = l_maos - l1.SALS
	#LET l_maod = l_maod - l1.SALD
	#LET l_maot = l_maot - l1.SALT

    ################################ GASTOS POR SERVICIOS FINANCIEROS TOTAL ################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 43
	LET l1.DRUB = "GASTOS POR SERVICIOS FINANCIEROS TOTAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sg('4201') + sg('410907') + sg('420204') + sg('420205')+ sg('410905')+ sg('4202')- sg('420204')- sg('420205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR

	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	LET l_maxs = l_maxs - l1.SALS
    LET l_maxd = l_maxd - l1.SALD
    LET l_maxt = l_maxt - l1.SALT
    ################################ GASTOS POR SERVICIOS FINANCIEROS TOTAL ################################

#Gastos por Operaciones Contingentes
	LET l1.AGEN = g_agen
	LET l1.CODI = 45
	LET l1.DRUB = "Gastos por créditos indirectos"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4201') + s('410907')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

#Gastos por Fideicomisos y Comisiones de Confianza
	LET l1.AGEN = g_agen
	LET l1.CODI = 46
	LET l1.DRUB = "Gastos por Fideicomisos y Comisiones de Confianza"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('420204') + s('420205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Gastos por Fideicomisos y Comisiones de Confianza
	LET l1.AGEN = g_agen
	LET l1.CODI = 47
	LET l1.DRUB = "Primas del fondo seguro de deposito"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410905')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

#Gastos Diversos
	LET l1.AGEN = g_agen
	LET l1.CODI = 48
	LET l1.DRUB = "Gastos Diversos"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4202') - s('420204') - s('420205')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

#Margen Fiannciero neto
	LET l1.AGEN = g_agen
	LET l1.CODI = 50
	LET l1.DRUB = "MARGEN FINANCIERO NETO DE INGRESOS Y GASTOS POR SERVICIOS FINANCIEROSs"
	LET l1.SALT = l_maot
	LET l1.SALS = l_maos
	LET l1.SALD = l_maod
	#INSERT INTO TMP_DATOS VALUES(l1.*)

    ############################# Margen Fiannciero neto central #############################
	LET l1.AGEN = g_agen
	LET l1.CODI = 51
	LET l1.DRUB = "MARGEN FINANCIERO NETO CENTRAL DE INGRESOS Y GASTOS POR SERVICIOS FINANCIEROS"
	LET l1.SALT = l_maxt
	LET l1.SALS = l_maxs
	LET l1.SALD = l_maxd
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	############################# Margen Fiannciero neto central #############################


	#RESULTADOS POR OPERACIONES FINANCIERAS (ROF)
	LET l1.AGEN = g_agen
	LET l1.CODI = 53
	LET l1.DRUB = "RESULTADOS POR OPERACIONES FINANCIERAS (ROF) AGENCIAS"
	FOR i = 0 TO 2
	        LET g_cmon = i
    		 LET l_sald = s('410911') +s('410912') +s('510301') +s('510911') +s('510912') +s('510306') +s('510915')
                -s('410915')+s('410913')+s('410914')+s('510303')+s('510913')+s('510914')+s('510916')-s('410916')
                +s('510917')-s('5109170102')-s('5109170202')-s('51091703')+s('5109240102')-s('410917')
                -s('4109170102')-s('4109170202')-s('41091703')+s('4109240102')+s('510904')-s('410904')+s('5108')-s('4108')+s('510918')
                +s('510921')-s('410921')+s('510919')-s('410919')-s('410920')+s('510926')-s('410926')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_mont = l1.SALT
	LET l_mons = l1.SALS
	LET l_mond = l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	############################# RESULTADOS POR OPERACIONES FINANCIERAS (ROF) COBRANZAS ##############################
	LET l1.AGEN = g_agen
	LET l1.CODI =105
	LET l1.DRUB = "RESULTADOS POR OPERACIONES FINANCIERAS (ROF) CENTRAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sc('410911') +sc('410912') +sc('510301') +sc('510911') +sc('510912') +sc('510306') +sc('510915')
                -sc('410915')+sc('410913')+sc('410914')+sc('510303')+sc('510913')+sc('510914')+sc('510916')-sc('410916')
                +sc('510917')-sc('5109170102')-sc('5109170202')-sc('51091703')+sc('5109240102')-sc('410917')
                -sc('4109170102')-sc('4109170202')-sc('41091703')+sc('4109240102')+sc('510904')-sc('410904')+sc('5108')-sc('4108')+sc('510918')
                +sc('510921')-sc('410921')+sc('510919')-sc('410919')-sc('410920')+sc('510926')-sc('410926')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_roft = l1.SALT
	LET l_rofs = l1.SALS
	LET l_rofd = l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
	############################# RESULTADOS POR OPERACIONES FINANCIERAS (ROF) COBRANZAS ##############################

	############################# RESULTADOS POR OPERACIONES FINANCIERAS (ROF) CENTRAL ##############################
	LET l1.AGEN = g_agen
	LET l1.CODI =63
	LET l1.DRUB = "RESULTADOS POR OPERACIONES FINANCIERAS (ROF) CENTRAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('410911') +sd('410912') +sd('510301') +sd('510911') +sd('510912') +sd('510306') +sd('510915')
                -sd('410915')+sd('410913')+sd('410914')+sd('510303')+sd('510913')+sd('510914')+sd('510916')-sd('410916')
                +sd('510917')-sd('5109170102')-sd('5109170202')-sd('51091703')+sd('5109240102')-sd('410917')
                -sd('4109170102')-sd('4109170202')-sd('41091703')+sd('4109240102')+sd('510904')-sd('410904')+sd('5108')-sd('4108')+sd('510918')
                +sd('510921')-sd('410921')+sd('510919')-sd('410919')-sd('410920')+sd('510926')-sd('410926')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald + l_roft
		   WHEN 1  LET l1.SALS = l_sald + l_rofs
		   WHEN 2  LET l1.SALD = l_sald + l_rofd
		END CASE
		LET l_sald = 0
	END FOR
	#LET l_mont = l1.SALT
	#LET l_mons = l1.SALS
	#LET l_mond = l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	############################# RESULTADOS POR OPERACIONES FINANCIERAS (ROF) CENTRAL ##############################



	############################# RESULTADOS POR OPERACIONES FINANCIERAS (ROF) FINAL ##############################
	LET l1.AGEN = g_agen
	LET l1.CODI = 52
	LET l1.DRUB = "RESULTADOS POR OPERACIONES FINANCIERAS (ROF) TOTAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sg('410911') +sg('410912') +sg('510301') +sg('510911') +sg('510912') +sg('510306') +sg('510915')
		-sg('410915')+sg('410913')+sg('410914')+sg('510303')+sg('510913')+sg('510914')+sg('510916')-sg('410916')
		+sg('510917')-sg('5109170102')-sg('5109170202')-sg('51091703')+sg('5109240102')-sg('410917')
		-sg('4109170102')-sd('4109170202')-sg('41091703')+sg('4109240102')+sg('510904')-sg('410904')+sg('5108')-sg('4108')+sg('510918')
		+sg('510921')-sg('410921')+sg('510919')-sg('410919')-sd('410920')+sg('510926')-sg('410926')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_maxt = l_maxt + l1.SALT
	LET l_maxs = l_maxs + l1.SALS
	LET l_maxd = l_maxd + l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	############################# RESULTADOS POR OPERACIONES FINANCIERAS (ROF) FINAL ##############################

	#CALL f004_distribuir_ROF_ea017a()


#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 54
	LET l1.DRUB = "Inversiones a valor razonable de cambios en resultados"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410911') + s('410912') + s('510301') + s('510911')+ s('510912')
		+ s('510306')+ s('510915')- s('410915')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 55
	LET l1.DRUB = "Inversiones a valor razonable con cambios en resultados"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410911') + s('410912') + s('510301') + s('510911')+ s('510912')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 56
	LET l1.DRUB = "Inversiones en commodities"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510306') + s('510915') - s('410915')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
		#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 57
	LET l1.DRUB = "Inversiones Disponibles para la Venta"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('410913') + s('410914') + s('510303') + s('510913')+ s('510914')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 58
	LET l1.DRUB = "Derivados de Negociacion"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510916') - s('410916')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 59
	LET l1.DRUB = "Resultado por Operaciones de Cobertura"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510917') - s('5109170102')- s('5109170202')- s('51091703')+ s('5109240102')
		- s('410917')- s('4109170102')- s('4109170202')- s('41091703')+ s('4109240102')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 60
	LET l1.DRUB = "Ganancias (Perdida) en Participaciones"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510904') - s('410904')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 61
	LET l1.DRUB = "Utilidad-Perdida en Diferencia de Cambio"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('5108') - s('4108')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 62
	LET l1.DRUB = "Otros"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('510918') - s('410918') + s('510921') - s('410921')  + s('510919') - s('410919')
		- s('410920') + s('510926') - s('410926')

		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Margen operacional

	LET l1.AGEN = g_agen
	LET l1.CODI = 64
	LET l1.DRUB = "MARGEN OPERACIONAL"

	LET l1.SALT = l_maot + l_mont
	LET l1.SALS = l_maos + l_mons
	LET l1.SALD = l_maod + l_mond
	##########################################
	# SE GUARDA EL MARGEN BRUTO EN VARIABLES #
	##########################################

	#INSERT INTO TMP_DATOS VALUES(l1.*)
	LET l_maot = l1.SALT
	LET l_maos = l1.SALS
	LET l_maod = l1.SALD

    ##################################### Margen operacional total #####################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 65
	LET l1.DRUB = "MARGEN OPERACIONAL"

	LET l1.SALT = l_maxt
	LET l1.SALS = l_maxs
	LET l1.SALD = l_maxd
	##########################################
	# SE GUARDA EL MARGEN BRUTO EN VARIABLES #
	##########################################

	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#LET l_maot = l1.SALT
	#LET l_maos = l1.SALS
	#LET l_maod = l1.SALD

	############################## Gastos de Personal y Directorio Total ##############################
	LET l1.AGEN = g_agen
	LET l1.CODI = 66
	LET l1.DRUB = "GASTOS DE ADMINISTRACIÓN TOTAL"
	FOR i = 0 TO 2
	    LET g_cmon = i
		LET l_sald = sg('4501') + sg('4502') + sg('4503') + sg('4504')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	LET l_maxt = l_maxt - l1.SALT
	LET l_maxs = l_maxs - l1.SALS
	LET l_maxd = l_maxd - l1.SALD
	#display l_maxt,"-",l_maxs,"-",l_maxd

	############################## Gastos de Personal y Directorio Agencias ##############################
	LET l1.AGEN = g_agen
	LET l1.CODI = 67
	LET l1.DRUB = "GASTOS DE  ADMINISTRACIÓN AGENCIAS"
	FOR i = 0 TO 2
	    LET g_cmon = i
		LET l_sald = s('4501') + s('4502') + s('4503') + s('4504')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	LET l_maot = l_maot - l1.SALT
	LET l_maos = l_maos -  l1.SALS
	LET l_maod = l_maod -  l1.SALD

	#Gastos de Personal y Directorio
	LET l1.AGEN = g_agen
	LET l1.CODI = 68
	LET l1.DRUB = "Gastos de Personal y Directorio Agencias"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4501') + s('4502')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Gastos por Servicios Recibidos de Terceros
	LET l1.AGEN = g_agen
	LET l1.CODI = 69
	LET l1.DRUB = "Gastos por Servicios Recibidos de Terceros Agencias"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4503')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Impuestos y Contribuciones
	LET l1.AGEN = g_agen
	LET l1.CODI = 70
	LET l1.DRUB = "Impuestos y Contribuciones Agencias"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4504')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	############################# GASTOS INDIRECTOS DE LA CCENTRAL #################################


	######################################## Gastos de Personal y Directorio Central ########################################
	LET l1.AGEN = g_agen
	LET l1.CODI = 71
	LET l1.DRUB = "GASTOS DE  ADMINISTRACIÓN CENTRAL"
	FOR i = 0 TO 2
	    LET g_cmon = i
		LET l_sald = sd('4501') + sd('4502') + sd('4503') + sd('4504')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#LET l_maot = l_maot -  l1.SALT
	#LET l_maos = l_maos -  l1.SALS
	#LET l_maod = l_maod -  l1.SALD

	#Gastos de Personal y Directorio INDIRECTOS
	LET l1.AGEN = g_agen
	LET l1.CODI = 72
	LET l1.DRUB = "Gastos de Personal y Directorio Central"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('4501') + sd('4502')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Gastos por Servicios Recibidos de Terceros INDIRECTOS
	LET l1.AGEN = g_agen
	LET l1.CODI = 73
	LET l1.DRUB = "Gastos por Servicios Recibidos de Terceros Central"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('4503')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	#Impuestos y Contribuciones INDIRECTOS
	LET l1.AGEN = g_agen
	LET l1.CODI = 74
	LET l1.DRUB = "Impuestos y Contribuciones Central"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('4504')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

	LET l1.AGEN = g_agen
	LET l1.CODI = 75
	LET l1.DRUB = "Gastos Administracion Cobranzas"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sc('4501') + sc('4502') + sc('4503') + sc('4504')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    #PROVISIONES, DEPRECIACIÓN Y AMORTIZACIÓN (mal)
	LET l1.AGEN = g_agen
	LET l1.CODI = 76
	LET l1.DRUB = "DEPRECIACIÓN Y AMORTIZACIÓN TOTAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sg('4401') + sg('4403')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#LET l_maot = l_maot -  l1.SALT
	#LET l_maos = l_maos -  l1.SALS
	#LET l_maod = l_maod -  l1.SALD
	LET l_maxt = l_maxt -  l1.SALT
	LET l_maxs = l_maxs -  l1.SALS
	LET l_maxd = l_maxd -  l1.SALD
	#display l_maxt,"-",l_maxs,"-",l_maxd
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#PROVISIONES, DEPRECIACIÓN Y AMORTIZACIÓN (mal)
	LET l1.AGEN = g_agen
	LET l1.CODI = 77
	LET l1.DRUB = "DEPRECIACIÓN Y AMORTIZACIÓN AGENCIAS"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4401') + s('4403')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_maot = l_maot -  l1.SALT
	LET l_maos = l_maos -  l1.SALS
	LET l_maod = l_maod -  l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    #PROVISIONES, DEPRECIACIÓN Y AMORTIZACIÓN (mal)
	LET l1.AGEN = g_agen
	LET l1.CODI = 78
	LET l1.DRUB = "DEPRECIACIÓN Y AMORTIZACIÓN CENTRAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('4401') + sd('4403')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    #PROVISIONES, DEPRECIACIÓN Y AMORTIZACIÓN (mal)
	LET l1.AGEN = g_agen
	LET l1.CODI = 79
	LET l1.DRUB = "DEPRECIACIÓN Y AMORTIZACIÓN COBRANZAS"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sc('4401') + sc('4403')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004

    #MARGEN OPERACIONAL NETO (mal)
	LET l1.AGEN = g_agen
	LET l1.CODI = 80
	LET l1.DRUB = "MARGEN OPERACIONAL NETO"
	LET l1.SALT = l_maot
	LET l1.SALS = l_maos
	LET l1.SALD = l_maod
	#INSERT INTO TMP_DATOS VALUES(l1.*)
	LET l_maot =   l1.SALT
	LET l_maos =   l1.SALS
	LET l_maod =   l1.SALD

	#MARGEN OPERACIONAL FINAL (mal)
	LET l1.AGEN = g_agen
	LET l1.CODI = 81
	LET l1.DRUB = "MARGEN OPERACIONAL NETO FINAL"
	LET l1.SALT = l_maxt
	LET l1.SALS = l_maxs
	LET l1.SALD = l_maxd
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
   CALL f007_creacion_egp_ea017(l_maxt,l_maxs,l_maxd)


END FUNCTION

FUNCTION f007_creacion_egp_ea017(l_maxt,l_maxs,l_maxd)
DEFINE l1  RECORD
	   AGEN     INTEGER,
	   CODI     SMALLINT,
	   DRUB     CHAR(100),
	   SALS     DECIMAL(14,2),
	   SALD     DECIMAL(14,2),
	   SALT     DECIMAL(14,2)
	   END RECORD,
	   l_maxt,l_maxs,l_maxd    DECIMAL(14,2),
	   l_cad  CHAR(300),
	   l_maot DECIMAL(14,2),
	   l_maos DECIMAL(14,2),
	   l_maod DECIMAL(14,2),
	   l_sald DECIMAL(14,2),
	   i     SMALLINT,
	   l_SQL CHAR(500)

#Provisiones para Incobrabilidad de Cuentas por Cobrar
	LET l1.AGEN = g_agen
	LET l1.CODI = 82
	LET l1.DRUB = "VALUACIÓN DE ACTIVOS Y PROVISIONES TOTAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sg('430501')+ sg('430502')+ sg('4305050402')+ sg('430506')+ sg('540402')+ sg('4303')+ sg('43050505')
		- sg('5405')+ sg('430401')+ sg('430402')+ sg('43050506')- sg('5406')+ sg('430403')+ sg('4301')+ sg('43050503')+ sg('43050507')
		- sg('5301')+sg('4404')-sg('5302')+sg('4405')-sg('5303')+sg('430504')+sg('430503')+sg('4305050')+sg('430509')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_maxt =   l_maxt - l1.SALT
	LET l_maxs =   l_maxs - l1.SALS
	LET l_maxd =   l_maxd - l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Provisiones para Incobrabilidad de Cuentas por Cobrar
	LET l1.AGEN = g_agen
	LET l1.CODI = 83
	LET l1.DRUB = "VALUACIÓN DE ACTIVOS Y PROVISIONES AGENCIAS"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('430501')+ s('430502')+ s('4305050402')+ s('430506')+ s('540402')+ s('4303')+ s('43050505')
		- s('5405')+ s('430401')+ s('430402')+ s('43050506')- s('5406')+ s('430403')+ s('4301')+ s('43050503')+ s('43050507')
		- s('5301')+s('4404')-s('5302')+s('4405')-s('5303')+s('430504')+s('430503')+s('4305050')+s('430509')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_maot =   l_maot - l1.SALT
	LET l_maos =   l_maos - l1.SALS
	LET l_maod =   l_maod - l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Provisiones para Incobrabilidad de Cuentas por Cobrar
	LET l1.AGEN = g_agen
	LET l1.CODI = 84
	LET l1.DRUB = "Provisiones para Créditos Indirectos"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('430501')+ s('430502')+ s('4305050402')+ s('430506')+ s('540402')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Provisiones para Incobrabilidad de Cuentas por Cobrar
	LET l1.AGEN = g_agen
	LET l1.CODI = 85
	LET l1.DRUB = "Provisiones para Incobrabilidad de Cuentas por Cobrar"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4303') + s('43050505') -  s('5405')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Provisiones para Bienes fuera de Uso, DesvalorizaciÓn y DepreciaciÓn de Bienes en Capitalizacion Inmobiliaria, y Otros
	LET l1.AGEN = g_agen
	LET l1.CODI = 86
	LET l1.DRUB = "Provisiones para  Bienes Realizables, Recibidos en Pago ,Recuperados y Adjudicados y Otros"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('430401') + s('430402') + s('43050506') - s('5406')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    #Provisiones para Contingencias y Otras
	LET l1.AGEN = g_agen
	LET l1.CODI = 87
	LET l1.DRUB = "Provisiones para Activos no corrientes mantenidos para la venta"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('430403')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004


	#Depreciación de Inmuebles, Mobiliario y Equipo
	LET l1.AGEN = g_agen
	LET l1.CODI = 88
	LET l1.DRUB = "Deterioro de inversiones"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4301')+ s('3050503')+ s('43050507')- s('5301')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Deterioro activo fijo
	LET l1.AGEN = g_agen
	LET l1.CODI = 89
	LET l1.DRUB = "Deterioro de Activo Fijo"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4404') - s('5302')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#Deterioro activo fijo
	LET l1.AGEN = g_agen
	LET l1.CODI = 90
	LET l1.DRUB = "Deterioro de Activos Intangibles"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('4405') - s('5303')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Deterioro activo fijo
	LET l1.AGEN = g_agen
	LET l1.CODI = 91
	LET l1.DRUB = "Provisiones por Litigios y Demandas"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('430504')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Deterioro activo fijo
	LET l1.AGEN = g_agen
	LET l1.CODI = 92
	LET l1.DRUB = "Otras Provisiones"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('430503')+ s('43050501')+ s('430509')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
    #Provisiones para Incobrabilidad de Cuentas por Cobrar
	LET l1.AGEN = g_agen
	LET l1.CODI = 93
	LET l1.DRUB = "VALUACIÓN DE ACTIVOS Y PROVISIONES CENTRAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('430501')+ sd('430502')+ sd('4305050402')+ sd('430506')+ sd('540402')+ sd('4303')+ sd('43050505')
		- sd('5405')+ sd('430401')+ sd('430402')+ sd('43050506')- sd('5406')+ sd('430403')+ sd('4301')+ sd('43050503')+ sd('43050507')
		- sd('5301')+sd('4404')-sd('5302')+sd('4405')-sd('5303')+sd('430504')+sd('430503')+sd('4305050')+sd('430509')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Provisiones para Incobrabilidad de Cuentas por Cobrar
	LET l1.AGEN = g_agen
	LET l1.CODI = 94
	LET l1.DRUB = "VALUACIÓN DE ACTIVOS Y PROVISIONES COBRANZAS"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sc('430501')+ sc('430502')+ sc('4305050402')+ sc('430506')+ sc('540402')+ sc('4303')+ sc('43050505')
		- sc('5405')+ sc('430401')+ sc('430402')+ sc('43050506')- sc('5406')+ sc('430403')+ sc('4301')+ sc('43050503')+ sc('43050507')
		- sc('5301')+sc('4404')-sc('5302')+sc('4405')-sc('5303')+sc('430504')+sc('430503')+sc('4305050')+sc('430509')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#RESULTADO DE OPERACION OTROS INGRESOS Y GASTOS
	LET l1.AGEN = g_agen
	LET l1.CODI = 95
	LET l1.DRUB = "RESULTADO DE OPERACION FINAL"
	LET l1.SALT = l_maxt
	LET l1.SALS = l_maxs
	LET l1.SALD = l_maxd
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	LET l_maot= l1.SALT
	LET l_maos= l1.SALS
	LET l_maod= l1.SALD

#Ingresos Netos (Gastos Netos) por Recuperación de Créditos
	LET l1.AGEN = g_agen
	LET l1.CODI = 96
	LET l1.DRUB = "OTROS INGRESOS Y GASTOS TOTAL"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sg('56')- sg('46')+ sg('57')-sg('49')+ sg('510507')+sg('510910')-sg('410910') + sg('5203')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	LET l_maxt= l_maxt + l1.SALT
	LET l_maxs= l_maxs + l1.SALS
	LET l_maxd= l_maxd + l1.SALD
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Ingresos Netos (Gastos Netos) por Recuperación de Créditos
	LET l1.AGEN = g_agen
	LET l1.CODI = 97
	LET l1.DRUB = "Otros ingresos y gastos "
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = s('56')- s('46')+ s('57')-s('49')+ s('510507')+s('510910')-s('410910') + s('5203')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Ingresos Netos (Gastos Netos) por Recuperación de Créditos
	LET l1.AGEN = g_agen
	LET l1.CODI = 98
	LET l1.DRUB = "Otros ingresos y gastos Central"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sd('56')- sd('46')+ sd('57')-sd('49')+ sd('510507')+sd('510910')-sd('410910') + sd('5203')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
	#Ingresos Netos (Gastos Netos) por Recuperación de Créditos
	LET l1.AGEN = g_agen
	LET l1.CODI = 99
	LET l1.DRUB = "Otros ingresos y gastos Cobranzas"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sc('56')- sc('46')+ sc('57')-sc('49')+ sc('510507')+sc('510910')-sc('410910') + sc('5203')
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#RESULTADOS DEL EJERCICIO ANTES DE PARTICIPACIONES E IMPUESTO A LA RENTA distribución LEGAL DE LA RENTA NETA
	LET l1.AGEN = g_agen
	LET l1.CODI = 100
	LET l1.DRUB = "RESULTADOS DEL EJERCICIO ANTES DEL IMPUESTO A LA RENTA"
	LET l1.SALT = l_maxt
	LET l1.SALS = l_maxs
	LET l1.SALD = l_maxd
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#IMPUESTO A LA RENTA
	LET l1.AGEN = g_agen
	LET l1.CODI = 101
	LET l1.DRUB = "IMPUESTO A LA RENTA"
	FOR i = 0 TO 2
	        LET g_cmon = i
		LET l_sald = sg('68')
		IF l_sald > 0 THEN
		   LET l_sald = l_sald * (-1)
		END IF
		CASE g_cmon
		   WHEN 0  LET l1.SALT = l_sald
		   WHEN 1  LET l1.SALS = l_sald
		   WHEN 2  LET l1.SALD = l_sald
		END CASE
		LET l_sald = 0
	END FOR
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
#RESULTADO NETO DEL EJERCICIO
	LET l1.AGEN = g_agen
	LET l1.CODI = 102
	LET l1.DRUB = "RESULTADO NETO DEL EJERCICIO"
	{IF p1.ambi = 1 THEN
	   LET l_cad = NULL
	ELSE}
	   LET l_cad = " AND AGEN =", g_agen
	#END IF
	FOR i = 0 TO 2
	        LET g_cmon = i
		CASE g_cmon
		   WHEN 0
		   LET l_SQL = " SELECT NVL(SUM(IMPI),0)",
		               " FROM TMP_SALDOS_AG ",
		               " WHERE CNTA[1,1] in(4,5,6) ", l_cad CLIPPED

		   PREPARE s_salt FROM l_SQL
		   EXECUTE s_salt INTO l1.SALT
	      LET l1.SALT = l1.SALT * (-1)

		   WHEN 1
 	           LET l_SQL = " SELECT NVL(SUM(IMPI),0)",
		               " FROM TMP_SALDOS_AG ",
		    	       " WHERE CNTA[1,1] in(4,5,6)  ",
		               " AND CNTA[3,3] = 1 ",l_cad  CLIPPED

		   PREPARE s_sals FROM l_SQL
		   EXECUTE s_sals INTO l1.SALS
         LET l1.SALS = l1.SALS * (-1)
		   WHEN 2
	 	   LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
		               " FROM TMP_SALDOS_AG ",
		               " WHERE CNTA[1,1] in(4,5,6) ",
		               " AND CNTA[3,3] = 2 ", l_cad CLIPPED

		   PREPARE s_sald FROM l_SQL
		   EXECUTE s_sald INTO l1.SALD
		   LET l1.SALD = l1.SALD * (-1)
		END CASE
		LET l_sald = 0
	END FOR
	#display g_agen,l1.SALT
	#INSERT INTO TMP_DATOS VALUES(l1.*)
   EXECUTE p80_ins_tmp_datos USING l1.* #@004
END FUNCTION


FUNCTION s(l_cnta)
DEFINE l_cnta CHAR(16),
       l_sald DECIMAL(14,2),
       l_SQL  CHAR(200),
       l_newc CHAR(16),
       l_cmax SMALLINT,
       l_cade CHAR(30),
       l_natu SMALLINT
       LET l_cmax = LENGTH(l_cnta)
       LET l_natu = l_cnta[1,1]


       IF l_cmax = 3 OR l_cmax = 2 THEN
	       CASE g_cmon
	          WHEN 0  LET l_newc = l_cnta[1,2],'?','*'
	          WHEN 1  LET l_newc = l_cnta[1,2],'1','*'
	          WHEN 2  LET l_newc = l_cnta[1,2],'2','*'
	       END CASE
       ELSE
	       CASE g_cmon
	          WHEN 0  LET l_newc = l_cnta[1,2],'?',l_cnta[4,l_cmax],'*'
	          WHEN 1  LET l_newc = l_cnta[1,2],'1',l_cnta[4,l_cmax],'*'
	          WHEN 2  LET l_newc = l_cnta[1,2],'2',l_cnta[4,l_cmax],'*'
	       END CASE
       END IF

       {IF p1.ambi = 1 THEN
          LET l_cade = ""
       ELSE}
          #LET l_cade =  " AND AGEN = ", g_agen
       #END IF

      {LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
                   " FROM TMP_SALDOS ",
        	   " WHERE CNTA MATCHES '",l_newc CLIPPED , "' ",
        	   l_cade CLIPPED
      PREPARE s_saldo FROM l_SQL
      EXECUTE s_saldo INTO l_sald}

      EXECUTE s_saldo USING l_newc,g_agen INTO l_sald #@004

      IF l_sald IS NULL THEN
         LET l_sald = 0
      END IF

      IF g_codi = 58 THEN
         IF l_cnta[1,2] = '62' THEN
            LET l_sald = l_sald * (-1)
         ELSE
             IF l_sald < 0 THEN
                LET l_sald = l_sald * (-1)
             END IF
         END IF
      ELSE
         IF l_natu = 5 OR l_natu = 6 THEN
            LET l_sald = l_sald * (-1)
         END IF
      END IF
      RETURN l_sald
END FUNCTION

FUNCTION sd(l_cnta)
DEFINE l_cnta CHAR(16),
       l_sald DECIMAL(14,2),
       l_SQL  CHAR(200),
       l_newc CHAR(16),
       l_cmax SMALLINT,
       l_cade CHAR(30),
       l_natu SMALLINT
       LET l_cmax = LENGTH(l_cnta)
       LET l_natu = l_cnta[1,1]


       IF l_cmax = 3 OR l_cmax = 2 THEN
	       CASE g_cmon
	          WHEN 0  LET l_newc = l_cnta[1,2],'?','*'
	          WHEN 1  LET l_newc = l_cnta[1,2],'1','*'
	          WHEN 2  LET l_newc = l_cnta[1,2],'2','*'
	       END CASE
       ELSE
	       CASE g_cmon
	          WHEN 0  LET l_newc = l_cnta[1,2],'?',l_cnta[4,l_cmax],'*'
	          WHEN 1  LET l_newc = l_cnta[1,2],'1',l_cnta[4,l_cmax],'*'
	          WHEN 2  LET l_newc = l_cnta[1,2],'2',l_cnta[4,l_cmax],'*'
	       END CASE
       END IF

       {IF p1.ambi = 1 THEN
          LET l_cade = ""
       ELSE}
          #LET l_cade =  " AND AGEN = ", g_agen
       #END IF

       {LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
                   " FROM TMP_SALDOS_CENT ",
        	   " WHERE CNTA MATCHES '",l_newc CLIPPED , "' ",
        	   l_cade CLIPPED
      PREPARE s_saldo_d FROM l_SQL
      EXECUTE s_saldo_d INTO l_sald}

      EXECUTE s_saldo_d USING l_newc,g_agen INTO l_sald #@004

      IF l_sald IS NULL THEN
         LET l_sald = 0
      END IF

      IF g_codi = 58 THEN
         IF l_cnta[1,2] = '62' THEN
            LET l_sald = l_sald * (-1)
         ELSE
             IF l_sald < 0 THEN
                LET l_sald = l_sald * (-1)
             END IF
         END IF
      ELSE
      	IF p1.vouc = "S" THEN
	         IF l_natu = 5 OR l_natu = 6 THEN
	            LET l_sald = l_sald * (-1)
	         END IF
        else
        	IF l_natu = 5 OR l_natu = 6 OR l_natu = 4  THEN
	            LET l_sald = l_sald * (-1)
	         END IF
        end if
      END IF
      RETURN l_sald
END FUNCTION

FUNCTION sc(l_cnta)
DEFINE l_cnta CHAR(16),
       l_sald DECIMAL(14,2),
       l_SQL  CHAR(200),
       l_newc CHAR(16),
       l_cmax SMALLINT,
       l_cade CHAR(30),
       l_natu SMALLINT
       LET l_cmax = LENGTH(l_cnta)
       LET l_natu = l_cnta[1,1]


       IF l_cmax = 3 OR l_cmax = 2 THEN
	       CASE g_cmon
	          WHEN 0  LET l_newc = l_cnta[1,2],'?','*'
	          WHEN 1  LET l_newc = l_cnta[1,2],'1','*'
	          WHEN 2  LET l_newc = l_cnta[1,2],'2','*'
	       END CASE
       ELSE
	       CASE g_cmon
	          WHEN 0  LET l_newc = l_cnta[1,2],'?',l_cnta[4,l_cmax],'*'
	          WHEN 1  LET l_newc = l_cnta[1,2],'1',l_cnta[4,l_cmax],'*'
	          WHEN 2  LET l_newc = l_cnta[1,2],'2',l_cnta[4,l_cmax],'*'
	       END CASE
       END IF

       {IF p1.ambi = 1 THEN
          LET l_cade = ""
       ELSE}
          #LET l_cade =  " AND AGEN = ", g_agen
       #END IF

       {LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
                   " FROM TMP_SALDOS_COBR ",
        	   " WHERE CNTA MATCHES '",l_newc CLIPPED , "' ",
        	   l_cade CLIPPED
      PREPARE s_saldo_c FROM l_SQL
      EXECUTE s_saldo_c INTO l_sald}

      EXECUTE s_saldo_c USING l_newc,g_agen INTO l_sald #@004

      IF l_sald IS NULL THEN
         LET l_sald = 0
      END IF

      IF g_codi = 58 THEN
         IF l_cnta[1,2] = '62' THEN
            LET l_sald = l_sald * (-1)
         ELSE
             IF l_sald < 0 THEN
                LET l_sald = l_sald * (-1)
             END IF
         END IF
      ELSE
      	IF p1.vouc = "S" THEN
	         IF l_natu = 5 OR l_natu = 6 THEN
	            LET l_sald = l_sald * (-1)
	         END IF
        else
        	IF l_natu = 5 OR l_natu = 6 OR l_natu = 4  THEN
	            LET l_sald = l_sald * (-1)
	         END IF
        end if
      END IF
      RETURN l_sald
END FUNCTION

FUNCTION sg(l_cnta)
DEFINE l_cnta CHAR(16),
       l_sald DECIMAL(14,2),
       l_SQL  CHAR(200),
       l_newc CHAR(16),
       l_cmax SMALLINT,
       l_cade CHAR(30),
       l_natu SMALLINT
       LET l_cmax = LENGTH(l_cnta)
       LET l_natu = l_cnta[1,1]


       IF l_cmax = 3 OR l_cmax = 2 THEN
	       CASE g_cmon
	          WHEN 0  LET l_newc = l_cnta[1,2],'?','*'
	          WHEN 1  LET l_newc = l_cnta[1,2],'1','*'
	          WHEN 2  LET l_newc = l_cnta[1,2],'2','*'
	       END CASE
       ELSE
	       CASE g_cmon
	          WHEN 0  LET l_newc = l_cnta[1,2],'?',l_cnta[4,l_cmax],'*'
	          WHEN 1  LET l_newc = l_cnta[1,2],'1',l_cnta[4,l_cmax],'*'
	          WHEN 2  LET l_newc = l_cnta[1,2],'2',l_cnta[4,l_cmax],'*'
	       END CASE
       END IF

       {IF p1.ambi = 1 THEN
          LET l_cade = ""
       ELSE}
          #LET l_cade =  " AND AGEN = ", g_agen
       #END IF

       {LET l_SQL = " SELECT NVL(SUM(IMPI),0) ",
                   " FROM TMP_SALDOS_AG ",
        	   " WHERE CNTA MATCHES '",l_newc CLIPPED , "' ",
        	   l_cade CLIPPED
      PREPARE s_saldo_g FROM l_SQL
      EXECUTE s_saldo_g INTO l_sald}

      EXECUTE s_saldo_g USING l_newc,g_agen INTO l_sald

      IF l_sald IS NULL THEN
         LET l_sald = 0
      END IF

      IF g_codi = 58 THEN
         IF l_cnta[1,2] = '62' THEN
            LET l_sald = l_sald * (-1)
         ELSE
             IF l_sald < 0 THEN
                LET l_sald = l_sald * (-1)
             END IF
         END IF
      ELSE
         IF l_natu = 5 OR l_natu = 6 THEN
            LET l_sald = l_sald * (-1)
         END IF
      END IF
      RETURN l_sald
END FUNCTION

FUNCTION imprimir_excel()
DEFINE l1      RECORD
               codi    SMALLINT,
               drub    CHAR(100)
               END RECORD,
       l_sald  DECIMAL(14,2),
       t_sald  DECIMAL(14,2),
       l_desc  CHAR(30),
       l_nofi  INTEGER,
       l_nume   SMALLINT,
       l_perf  CHAR(3),
       l_cmd   CHAR(10000),
       #@004 ini
       l2   RECORD
         agei  SMALLINT,
         agef  SMALLINT
       END RECORD,
       l_cana  INTEGER,
       l_cont  SMALLINT,
       i       SMALLINT,
       l_fant  DATE,
       l_tipo  SMALLINT,
       l_tip1  SMALLINT,
       #@005 fin
       l3   RECORD
         sal1  DECIMAL(14,2), #--Saldo sede
         sal2  DECIMAL(14,2)  #--Saldo analista
       END RECORD,
       #@005 fin
       l_sals  DECIMAL(14,2),
       #@006 inicio
       l3_temp RECORD
         sal1  DECIMAL(14,2),
         sal2  DECIMAL(14,2)
       END RECORD,
       l_agen SMALLINT,
       l_agen_temp SMALLINT,
       l_sald_temp DECIMAL(14,2),
       l_sald_ult  DECIMAL(14,2),
       l_posi SMALLINT,
       l_cant SMALLINT,
       l_valo_upd CHAR(150), #@006
       l_nofi_upd SMALLINT, #@006
       l_valo CHAR(150),
       #@009 ini
       l_ximp   SMALLINT,
       l_tipx   SMALLINT
       #@009 fin

       #CREATE TEMP TABLE tb_agenc
       #(
       # agen  SMALLINT
       #)WITH NO LOG
       DELETE FROM tb_agenc #@004

       SELECT adprfperf         INTO l_perf
         FROM adprf
        WHERE adprfusrn = g_user

       SELECT COUNT(*) INTO l_nume
         FROM pcprm
        WHERE pcprmflag = 372
          AND pcprmdato = l_perf
       IF l_nume > 0 THEN
            IF l_perf = "PJT" THEN
                INSERT INTO tb_agenc
                SELECT pcofinofi
                FROM pcofi WHERE pcofiprm6 IN(
                SELECT gbconcorr FROM gbcon WHERE gbconpfij=46 AND gbconabre=g_user)
            ELSE
                INSERT INTO tb_agenc
                SELECT adusragen
               FROM adusr
                 WHERE adusrusrn = g_user
            END IF
       ELSE
            INSERT INTO tb_agenc
            SELECT gbofinofi FROM gbofi
       END IF

      #@004 ini
      INITIALIZE l2.* TO NULL
      FOREACH q34_sel_pcprm INTO l2.agei,l2.agef
         EXECUTE p35_upd_agen_tmp_datos USING l2.agef,l2.agei
      END FOREACH

      LET l_tipo = 5
      LET l_tip1 = 6
      IF p1.det1 = 'S' THEN
         LET l_tipo = 2
         LET l_tip1 = 0
      END IF
      #@004 fin

      #@007 ini
      CALL f019_ajustar_tmp_datos_cn559()
      CALL f021_actualizar_calculo_cn559()
      #@007 fin

	{UPDATE  TMP_DATOS SET AGEN = 1 WHERE AGEN= 9
	UPDATE  TMP_DATOS SET AGEN = 1 WHERE AGEN= 11
	UPDATE  TMP_DATOS SET AGEN = 17 WHERE AGEN= 18
	UPDATE  TMP_DATOS SET AGEN = 1  WHERE AGEN= 13
	UPDATE  TMP_DATOS SET AGEN = 1  WHERE AGEN= 19
	UPDATE  TMP_DATOS SET AGEN = 1  WHERE AGEN= 98

	UPDATE  TMP_DATOS SET AGEN = 29 WHERE AGEN= 10
	UPDATE  TMP_DATOS SET AGEN = 30 WHERE AGEN= 16
	UPDATE  TMP_DATOS SET AGEN = 31 WHERE AGEN= 20
	UPDATE  TMP_DATOS SET AGEN = 33 WHERE AGEN= 21
	UPDATE  TMP_DATOS SET AGEN = 34 WHERE AGEN= 22
	UPDATE  TMP_DATOS SET AGEN = 32 WHERE AGEN= 23
	UPDATE  TMP_DATOS SET AGEN = 28	WHERE AGEN= 4

	UPDATE  TMP_DATOS SET AGEN = 42	WHERE AGEN= 26  #JAYAN
	UPDATE  TMP_DATOS SET AGEN = 47	WHERE AGEN= 27  #MOTUE
	UPDATE  TMP_DATOS SET AGEN = 44	WHERE AGEN= 24  #FERRE
	UPDATE  TMP_DATOS SET AGEN = 43	WHERE AGEN= 39  #CHOTA
	UPDATE  TMP_DATOS SET AGEN = 48	WHERE AGEN= 35  #BAGUA
	UPDATE  TMP_DATOS SET AGEN = 49	WHERE AGEN= 36  #PATAP
	UPDATE  TMP_DATOS SET AGEN = 50	WHERE AGEN= 38  #QUERE
	UPDATE  TMP_DATOS SET AGEN = 51	WHERE AGEN= 46  #NUEVA

	UPDATE  TMP_DATOS SET AGEN = 56	WHERE AGEN= 54  #NUEVA

	UPDATE  TMP_DATOS SET AGEN = 3	WHERE AGEN= 45
	UPDATE  TMP_DATOS SET AGEN = 3	WHERE AGEN= 55
	UPDATE  TMP_DATOS SET AGEN = 12	WHERE AGEN= 40
}
	CALL f003_distribuir_impuestos_ea017a()

   #@004 ini
   IF p1.deta = 'S' THEN
      CALL f005_llenar_datos_analist_cn559(p1.fech)
   END IF
   LET l_fant = p1.fech -  DAY(p1.fech)
   #@004 fin

 DECLARE q_rubros CURSOR FOR
 SELECT CODI, DRUB
 FROM TMP_DATOS
 WHERE CODI NOT IN ( #@004 ini
    SELECT pcprmvalo
    FROM pcprm
    WHERE pcprmflag = 555
    AND pcprmdato = 6
    AND pcprmvalo = 64 #@005
    ) #@004 fin
 GROUP BY 1,2
 ORDER BY 1

 INITIALIZE l3.* TO NULL #@005

 #@006 ini
 IF p1.unif = "S" THEN
     CALL crear_temp_sedes()
 END IF
 #@006 fin
  {DECLARE q_oficina CURSOR FOR
      SELECT GBOFINOFI, GBOFIDESC FROM GBOFI
        #WHERE GBOFINOFI NOT IN (6,8,9,11,18)
       WHERE GBOFINOFI NOT IN (6,8,9,11,18,19,10,16,20,21,22,23,4,26,27,24,39,35,36,38,46,54,55,45,40,13,98)
        #WHERE GBOFINOFI IN (SELECT DISTINCT AGEN FROM TMP_DATOS WHERE SALS>0 AND SALD>0 AND SALT>0)
       AND GBOFINOFI IN (SELECT agen FROM tb_agenc)
      ORDER BY GBOFINOFI}

  LET g_sHtml = "<html><head><title>EGP POR AGENCIA</title></head>"
  LET g_sHtml = g_sHtml CLIPPED, "<body>"
  LET g_sHtml = g_sHtml CLIPPED, "<h3><font face='Arial' color = '#000066'>EDPYME ALTERNATIVA SA</font></h3>"
  #LET g_sHtml = g_sHtml CLIPPED, "<h2>EGP POR AGENCIA -FORMA B - EN NUEVOS SOLES  A FECHA ", p1.fech USING "DD/MM/YYYY","</h2>"
  LET g_sHtml = g_sHtml CLIPPED, "<h2>ESTADO DE RESULTADO EN ",g_des1 CLIPPED,"  AL ", p1.fech USING "DD/MM/YYYY","</h2>"	#@001
  LET g_sHtml = g_sHtml CLIPPED, "<table border = 1>"
  LET l_cmd  = "echo '", g_sHtml CLIPPED, "' > ", g_spool #@005
  RUN l_cmd #@005

  #----TPP--y--TR--#
  #@009 ini
  IF p1.deta = 'S' THEN
    FOR l_tipx = 1 TO 2 #@011
        LET g_sHtml = "<tr>"
        IF l_tipx = 1 THEN
            LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>TPP</center></b></td>"
        ELSE
            LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>TR</center></b></td>"
        END IF
        LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
        RUN l_cmd

        IF p1.unif = 'N' THEN
            FOREACH q_oficina USING p1.agei,p1.agef INTO l_nofi,l_desc
                LET g_sHtml = "<td bgcolor=#E1E1F0>",f031_buscar_tpp_cn559(l_nofi,0,l_tipx) using "---,--&.-&" ,"</td>"
                LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                RUN l_cmd
            FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_cana
                LET g_sHtml = "<td>",f031_buscar_tpp_cn559(l_nofi,l_cana,l_tipx) using "---,--&.-&" ,"</td>"
                LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                RUN l_cmd
            END FOREACH
        END FOREACH
    ELSE
        FOREACH q_agen USING p1.agei,p1.agef INTO l_agen
            LET g_sHtml = "<td bgcolor=#E1E1F0>",f031_buscar_tpp_cn559(l_agen,0,l_tipx) using "---,--&.-&" ,"</td>"
            LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
            RUN l_cmd
            LET l_ximp = 0
            FOREACH q_sede USING l_agen INTO l_nofi, l_desc
                FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_cana
                    IF l_cana = -1 THEN
                        LET l_ximp = l_ximp + 1
                        IF l_ximp = 2 THEN
                            CONTINUE FOREACH
                        END IF
                    END IF
                    LET g_sHtml = NULL
                    LET g_sHtml = "<td>",f031_buscar_tpp_cn559(l_nofi,l_cana,l_tipx) using "---,--&.-&" ,"</td>"
                    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                    RUN l_cmd
                END FOREACH
            END FOREACH
        END FOREACH
    END IF
    LET g_sHtml = "</tr>"
    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@005
    RUN l_cmd
    END FOR
  END IF
  #@009 fin

  #@005 ini
  IF p1.deta = 'S' THEN
    LET g_sHtml = "<tr>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>SALDO EN SOLES</center></b></td>"
    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@005
    RUN l_cmd
  END IF

    LET g_sHtml = NULL

  #@005 fin

    IF p1.deta = 'S' THEN
        IF p1.unif = 'N' THEN #@006
            FOREACH q_oficina USING p1.agei,p1.agef INTO l_nofi, l_desc #@008
                LET l3.sal1 = NULL
                EXECUTE p19_sel_sals_agen USING p1.fech,l_nofi INTO l3.sal1
                LET g_sHtml = NULL
                LET g_sHtml = "<td bgcolor=#E1E1F0>",l3.sal1 using "--,---,---,--&.-&" ,"</td>"
                LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                RUN l_cmd

                FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_cana
                    LET l3.sal2 = NULL
                    EXECUTE p20_sel_sals_cana USING p1.fech,l_nofi,l_cana INTO l3.sal2
                    LET g_sHtml = NULL
                    LET g_sHtml = "<td>",l3.sal2 using "--,---,---,--&.-&" ,"</td>"
                    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                    RUN l_cmd
                END FOREACH

            END FOREACH
        #@006 inicio
        ELSE
            FOREACH q_agen USING p1.agei,p1.agef INTO l_agen #@008
                LET l3.sal1 = 0
                FOREACH q_sede USING l_agen INTO l_nofi, l_desc
                    EXECUTE p19_sel_sals_agen USING p1.fech,l_nofi INTO l3_temp.sal1
                    LET l3.sal1 = l3.sal1 + l3_temp.sal1
                END FOREACH
                LET g_sHtml = NULL
                LET g_sHtml = "<td bgcolor=#E1E1F0>",l3.sal1 using "--,---,---,--&.-&" ,"</td>"
                LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                RUN l_cmd

                LET l_ximp = 0 #@009
                FOREACH q_sede USING l_agen INTO l_nofi, l_desc
                    FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_cana
                        #@009 ini
                        IF l_cana = -1 THEN
                            LET l_ximp = l_ximp + 1
                            IF l_ximp = 2 THEN
                                CONTINUE FOREACH
                            END IF
                        END IF
                        #@009 fin
                        LET l3.sal2 = NULL
                        EXECUTE p20_sel_sals_cana USING p1.fech,l_nofi,l_cana INTO l3.sal2
                        LET g_sHtml = NULL
                        LET g_sHtml = "<td>",l3.sal2 using "--,---,---,--&.-&" ,"</td>"
                        LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                        RUN l_cmd
                    END FOREACH
                END FOREACH
            END FOREACH
        END IF
        #@006 fin
        LET g_sHtml = "</tr>"
    END IF
    #@005 fin

    LET g_sHtml = g_sHtml CLIPPED, "<tr>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>RUBROS</center></b></td>"
    #@004 ini
    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@005
    RUN l_cmd
    #@004 fin

    IF p1.unif = 'N' THEN #@006
        FOREACH q_oficina INTO l_nofi, l_desc
            #@004 ini
            IF p1.deta = 'S' THEN
                LET g_sHtml = "<td><b><center>",l_desc CLIPPED, "</center></b></td>"
                LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@004
                RUN l_cmd
                FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_cana
                    LET g_sHtml = "<td>",f004_buscar_analista_cn559(l_cana) CLIPPED,"</td>"
                    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@004
                    RUN l_cmd #@004
                END FOREACH
            ELSE #@004 ini
                LET g_sHtml = "<td><b><center>",l_desc CLIPPED, "</center></b></td>"
                LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@004
                RUN l_cmd #@004
            END IF
        END FOREACH
    #@016 inicio
    ELSE
        FOREACH q_agen USING p1.agei,p1.agef INTO l_agen #@008
            IF p1.deta = 'S' THEN
                LET g_sHtml = "<td><b><center>",l_agen,"-",f006_buscar_descripcion_pc2009(l_agen) CLIPPED, "</center></b></td>"
                LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@004
                RUN l_cmd
                #### DUDA: SE MOSTRARA EL ANALISTA SOLO DE LA AGENCIA O DE LAS OFICINAS? #######
                LET l_ximp = 0 #@009
                FOREACH q_sede USING l_agen INTO l_nofi, l_desc
                    FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_cana
                        #@009 ini
                        IF l_cana = -1 THEN
                            LET l_ximp = l_ximp + 1
                            IF l_ximp = 2 THEN
                                CONTINUE FOREACH
                            END IF
                        END IF
                        #@009 fin
                        LET g_sHtml = "<td>",f004_buscar_analista_cn559(l_cana) CLIPPED,"</td>"
                        LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@004
                        RUN l_cmd #@004
                    END FOREACH
                END FOREACH
            ELSE #@004 ini
                LET g_sHtml = "<td><b><center>",f006_buscar_descripcion_pc2009(l_agen) CLIPPED, "</center></b></td>"
                LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@004
                RUN l_cmd #@004
            END IF
        END FOREACH
    END IF
    #@016 fin

    LET g_sHtml = "<td><b><center>TOTAL</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "</tr>"
    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool #@004
    RUN l_cmd

    IF p1.deta = 'S' THEN #@004
        FOREACH q14_sel_rubr_det USING l_tipo,l_tip1 INTO l1.*
            LET t_sald = 0
            LET g_sHtml = "<tr>"
            IF l1.codi= 1 OR l1.codi= 13  OR l1.codi= 30 OR l1.codi= 31 OR l1.codi= 32 OR l1.codi= 35 OR  l1.codi= 36 OR l1.codi= 37
                OR l1.codi= 43 OR  l1.codi= 51 OR  l1.codi= 52 OR  l1.codi= 64 OR l1.codi= 65 OR l1.codi=66  OR l1.codi=67 OR l1.codi=71
                OR l1.codi=75 OR l1.codi=76 OR l1.codi=79  OR l1.codi=81  OR l1.codi=82 OR l1.codi=95 OR l1.codi=96 OR l1.codi=100
                OR l1.codi=102 THEN
                LET g_sHtml = g_sHtml CLIPPED, "<td bgcolor=#E1E1F0><font face='Arial'><b>",l1.drub CLIPPED, "</b></font></td>"
            ELSE
                LET g_sHtml = g_sHtml CLIPPED, "<td><font face='Arial' size=1>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",l1.drub CLIPPED, "</font></td>"
            END IF

            LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
            RUN l_cmd

            IF p1.unif = 'N' THEN #@006
                FOREACH q_oficina INTO l_nofi, l_desc
                    IF l1.codi = 64 THEN
                        LET l_sald = f006_buscar_saldo_agencia_cn559(l_nofi,l1.codi)
                    ELSE
                        LET l_sald = buscar_saldo_a(l1.codi,l_nofi)
                    END IF

                    IF l1.codi= 1 OR l1.codi= 13  OR l1.codi= 30 OR
                        l1.codi= 31 OR l1.codi= 32 OR l1.codi= 35 OR  l1.codi= 36 OR l1.codi= 37 OR
                        l1.codi= 43 OR  l1.codi= 51 OR  l1.codi= 52 OR  l1.codi= 64 OR l1.codi= 65  OR l1.codi=66  OR l1.codi=67 OR l1.codi=71
                        OR l1.codi=75 OR l1.codi=76 OR l1.codi=79  OR l1.codi=81 OR l1.codi=82 OR l1.codi=95 OR l1.codi=96 OR l1.codi=100 OR l1.codi=102
                    THEN
                        LET g_sHtml = "<td bgcolor=#E1E1F0><b>",l_sald using "--,---,---,--&.-&" ,"</b></td>"
                    ELSE
                        LET g_sHtml = "<td><font face='Arial' size=1>",l_sald using "--,---,---,--&.-&" ,"</td>"
                    END IF
                    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                    RUN l_cmd

                    FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_cana
                        LET g_sHtml = "<td>",f006_buscar_saldo_analist_cn559(l_nofi,l_cana,l1.codi) USING "--,---,---,--&.-&","</td>"
                        LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                        RUN l_cmd
                    END FOREACH

                    LET t_sald = t_sald + l_sald
                END FOREACH
            #@006 inicio
            ELSE
                FOREACH q_agen USING p1.agei,p1.agef INTO l_agen #@008
                    LET l_sald = 0
                    FOREACH q_sede USING l_agen INTO l_nofi, l_desc
                        IF l1.codi = 64 THEN
                            LET l_sald_temp = f006_buscar_saldo_agencia_cn559(l_nofi,l1.codi)
                        ELSE
                            LET l_sald_temp = buscar_saldo_a(l1.codi,l_nofi)
                        END IF
                        LET l_sald = l_sald + l_sald_temp
                    END FOREACH
                    IF l1.codi= 1 OR l1.codi= 13  OR l1.codi= 30 OR
                        l1.codi= 31 OR l1.codi= 32 OR l1.codi= 35 OR  l1.codi= 36 OR l1.codi= 37 OR
                        l1.codi= 43 OR  l1.codi= 51 OR  l1.codi= 52 OR  l1.codi= 64 OR l1.codi= 65  OR l1.codi=66  OR l1.codi=67 OR l1.codi=71
                        OR l1.codi=75 OR l1.codi=76 OR l1.codi=79  OR l1.codi=81 OR l1.codi=82 OR l1.codi=95 OR l1.codi=96 OR l1.codi=100 OR l1.codi=102
                    THEN
                        LET g_sHtml = "<td bgcolor=#E1E1F0><b>",l_sald using "--,---,---,--&.-&" ,"</b></td>"
                    ELSE
                        LET g_sHtml = "<td><font face='Arial' size=1>",l_sald using "--,---,---,--&.-&" ,"</td>"
                    END IF
                    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                    RUN l_cmd

                    LET l_ximp = 0 #@009
                    FOREACH q_sede USING l_agen INTO l_nofi, l_desc
                        FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_cana
                        #@009 ini
                        IF l_cana = -1 THEN
                            LET l_ximp = l_ximp + 1
                            IF l_ximp = 2 THEN
                                CONTINUE FOREACH
                            END IF
                        END IF
                        #@009 fin
                            LET g_sHtml = "<td>",f006_buscar_saldo_analist_cn559(l_nofi,l_cana,l1.codi) USING "--,---,---,--&.-&","</td>"
                            LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
                            RUN l_cmd
                        END FOREACH
                    END FOREACH

                    LET t_sald = t_sald + l_sald
                END FOREACH
            END IF
            #@006 inicio

            LET g_sHtml = "<td bgcolor = '#4682B4'><b>",t_sald using "---,---,---,--&.-&", "</b></td>"
   	        LET g_sHtml = g_sHtml CLIPPED,"</tr>"
   	        LET l_cmd = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
            RUN l_cmd
        END FOREACH

    ELSE #@004 ini
        FOREACH q_rubros INTO l1.*
            LET t_sald = 0
            LET g_sHtml = "<tr>"
            IF l1.codi= 1 OR l1.codi= 13  OR l1.codi= 30 OR l1.codi= 31 OR l1.codi= 32 OR l1.codi= 35 OR  l1.codi= 36 OR l1.codi= 37
                OR l1.codi= 43 OR  l1.codi= 51 OR  l1.codi= 52 OR  l1.codi= 64 OR l1.codi= 65 OR l1.codi=66  OR l1.codi=67 OR l1.codi=71
                OR l1.codi=75 OR l1.codi=76 OR l1.codi=79  OR l1.codi=81  OR l1.codi=82 OR l1.codi=95 OR l1.codi=96 OR l1.codi=100
                OR l1.codi=102
            THEN
                LET g_sHtml = g_sHtml CLIPPED, "<td bgcolor=#E1E1F0><font face='Arial'><b>",l1.drub CLIPPED, "</b></font></td>"
	        ELSE
	   	        LET g_sHtml = g_sHtml CLIPPED, "<td><font face='Arial' size=1>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",l1.drub CLIPPED, "</font></td>"
	        END IF

	        IF p1.unif = 'N' THEN #@006
            #LET g_sHtml = g_sHtml CLIPPED,"<td bgcolor=#E1E1F0><b>",l1.codi using "--,---,---,--&.-&" ,"</b></td>"
                FOREACH q_oficina INTO l_nofi, l_desc
                    LET l_sald = buscar_saldo_a(l1.codi,l_nofi)

                    IF l1.codi= 1 OR l1.codi= 13  OR l1.codi= 30 OR
                        l1.codi= 31 OR l1.codi= 32 OR l1.codi= 35 OR  l1.codi= 36 OR l1.codi= 37 OR
    		            l1.codi= 43 OR  l1.codi= 51 OR  l1.codi= 52 OR  l1.codi= 64 OR l1.codi= 65  OR l1.codi=66  OR l1.codi=67 OR l1.codi=71
                        OR l1.codi=75 OR l1.codi=76 OR l1.codi=79  OR l1.codi=81 OR l1.codi=82 OR l1.codi=95 OR l1.codi=96 OR l1.codi=100 OR l1.codi=102
     			    THEN
                        LET g_sHtml = g_sHtml CLIPPED,"<td bgcolor=#E1E1F0><b>",l_sald using "--,---,---,--&.-&" ,"</b></td>"
                    ELSE
                        LET g_sHtml = g_sHtml CLIPPED,"<td><font face='Arial' size=1>",l_sald using "--,---,---,--&.-&" ,"</td>"
                  	END IF

                    LET t_sald = t_sald + l_sald
                 END FOREACH
            #@006 ini
            ELSE
                FOREACH q_agen USING p1.agei,p1.agef INTO l_agen #@008
                    LET l_sald = 0
                    FOREACH q_sede USING l_agen INTO l_nofi, l_desc
                        LET l_sald_temp = buscar_saldo_a(l1.codi,l_nofi)
                        LET l_sald = l_sald + l_sald_temp
                    END FOREACH
                    IF l1.codi= 1 OR l1.codi= 13  OR l1.codi= 30 OR
                        l1.codi= 31 OR l1.codi= 32 OR l1.codi= 35 OR  l1.codi= 36 OR l1.codi= 37 OR
    		            l1.codi= 43 OR  l1.codi= 51 OR  l1.codi= 52 OR  l1.codi= 64 OR l1.codi= 65  OR l1.codi=66  OR l1.codi=67 OR l1.codi=71
                        OR l1.codi=75 OR l1.codi=76 OR l1.codi=79  OR l1.codi=81 OR l1.codi=82 OR l1.codi=95 OR l1.codi=96 OR l1.codi=100 OR l1.codi=102
     			    THEN
                        LET g_sHtml = g_sHtml CLIPPED,"<td bgcolor=#E1E1F0><b>",l_sald using "--,---,---,--&.-&" ,"</b></td>"
                    ELSE
                        LET g_sHtml = g_sHtml CLIPPED,"<td><font face='Arial' size=1>",l_sald using "--,---,---,--&.-&" ,"</td>"
              	    END IF

                    LET t_sald = t_sald + l_sald
                END FOREACH
            END IF

            LET g_sHtml = g_sHtml CLIPPED, "<td bgcolor = '#4682B4'><b>",t_sald using "---,---,---,--&.-&", "</b></td>"
            LET g_sHtml = g_sHtml CLIPPED,"</tr>"
            LET l_cmd = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
            RUN l_cmd
        END FOREACH
    END IF #@004

    LET g_sHtml ="</table></body></html>"
    LET l_cmd = "echo '", g_sHtml CLIPPED, "' >> ", g_spool
    RUN l_cmd

    CALL f009_eliminar_indices_cn559(p1.deta) #@004

    #@008 ini
    IF p1.deta = 'S' THEN
        CALL f029_mostrar_dif_cod2_cn559()
    END IF
    #@008 fin
    #EXECUTE p103_del_tmp_sedes
    #drop table tb_agenc
END FUNCTION

FUNCTION buscar_saldo_a(l_codi, l_agen)
DEFINE l_codi   SMALLINT,
       l_agen   SMALLINT,
       l_sald   DECIMAL(14,2)

   LET l_sald = NULL #@004
   {SELECT NVL(SUM(SALT),0)
     INTO l_sald
     FROM TMP_DATOS
    WHERE CODI = l_codi
      AND AGEN = l_agen
   }
   EXECUTE p81_sel_tmp_datos_salt USING l_codi,l_agen INTO l_sald #@004

   IF l_sald IS NULL THEN
      LET l_sald = 0
   END IF

RETURN l_sald
END FUNCTION

FUNCTION f003_distribuir_impuestos_ea017a()
DEFINE
	l_agen  INTEGER,
	l_nume  SMALLINT,
	l_salt	DECIMAL(14,4),
	l_tres	DECIMAL(14,4),
	l_timp	DECIMAL(14,4),
	l_resu	DECIMAL(14,4),
	l_impr	DECIMAL(14,4),
	l_porc	DECIMAL(14,10)

	SELECT agen,nvl(SUM(SALT),0) as salt
	  FROM TMP_DATOS
	 #WHERE codi = 73
	 WHERE codi = 100
	 GROUP BY 1
	   #AND SALT > 0
	  # AND agen NOT IN (6,8,9,11,18,13,19)
	INTO TEMP tab_ag_aux WITH NO LOG

	SELECT nvl(SUM(salt),0) INTO l_tres
	  FROM tab_ag_aux
	 WHERE salt > 0

	SELECT nvl(SUM(SALT),0)		INTO l_timp
	  FROM TMP_DATOS
	 #WHERE codi = 74
	 WHERE codi = 101

	  #DISPLAY l_tres,l_timp
	DECLARE q_cur_agen CURSOR FOR
		#SELECT DISTINCT agen FROM TMP_DATOS
		SELECT agen,SUM(salt) FROM TMP_DATOS WHERE codi = 100
		#SELECT agen,SUM(salt) FROM TMP_DATOS WHERE codi = 73
		#AND salt>0
		#AND agen NOT IN (6,8,9,11,18,13,19)
		GROUP BY 1
        ORDER BY 1;

	FOREACH q_cur_agen INTO l_agen,l_salt
		#display l_agen,l_salt,l_nume

		#SELECT COUNT(*) INTO l_nume FROM TMP_DATOS WHERE agen = l_agen AND codi = 74
		SELECT COUNT(*) INTO l_nume FROM TMP_DATOS WHERE agen = l_agen AND codi = 101

		IF l_salt > 0 THEN

			IF l_nume = 0 THEN
				LET l_nume = 1
			END IF

			LET l_porc = l_salt / l_tres
			LET l_impr = (l_porc * l_timp)/l_nume
			LET l_resu = (l_salt/l_nume) + l_impr
			#display l_agen,l_porc,l_impr,"-",l_resu

			#UPDATE TMP_DATOS SET salt = l_impr WHERE codi = 74 AND agen = l_agen
			#UPDATE TMP_DATOS SET salt = l_resu WHERE codi = 75 AND agen = l_agen
			UPDATE TMP_DATOS SET salt = l_impr WHERE codi = 101 AND agen = l_agen
			UPDATE TMP_DATOS SET salt = l_resu WHERE codi = 102 AND agen = l_agen
		ELSE
			#UPDATE TMP_DATOS SET salt = 0 WHERE codi = 74 AND agen = l_agen
			#UPDATE TMP_DATOS SET salt = l_salt/l_nume WHERE codi = 75 AND agen = l_agen
			UPDATE TMP_DATOS SET salt = 0 WHERE codi = 101 AND agen = l_agen
            #@007 ini
            IF l_nume = 0 THEN
				LET l_nume = 1
			END IF
            #@007 fin
			UPDATE TMP_DATOS SET salt = l_salt/l_nume WHERE codi = 102 AND agen = l_agen
		END IF
	END FOREACH

   DROP TABLE tab_ag_aux

END FUNCTION

FUNCTION f003_distribucion_cartera_ea017()
DEFINE
	l_tota DECIMAL(14,2)

	SELECT SUM(eacccsals) 		INTO l_tota
	FROM eaccc
	WHERE eacccfcie=p1.fech
	 #AND eaccccana=9999
	 AND eacccagen NOT IN(13,98,17,19,98)
	 AND eacccesta<>'CASTIGADO'


	SELECT eacccagen AS agen,SUM(eacccsals)/l_tota AS porc
	FROM eaccc
	WHERE eacccfcie=p1.fech
	 #AND eaccccana=9999
	 AND eacccagen NOT IN(13,98,17,19,98)
	 AND eacccesta<>'CASTIGADO'
	GROUP BY 1
	INTO TEMP tab_agen WITH NO LOG;

END FUNCTION

#@004 ini
FUNCTION f004_buscar_analista_cn559(p_cfun)
DEFINE   p_cfun   INTEGER,
         l_nomb   CHAR(40)

   LET l_nomb = NULL
   EXECUTE p14_sel_gbfir USING p_cfun INTO l_nomb

   IF p_cfun = - 1 THEN
    LET l_nomb = "AJUSTE ANALISTA ORIGEN"
   END IF

RETURN l_nomb
END FUNCTION
#@004 fin

#@004 ini
FUNCTION f005_llenar_datos_analist_cn559(p_fcie)
DEFINE   p_fcie   DATE,
         l1 RECORD
            codi  SMALLINT,
            agen  SMALLINT,
            cana  INTEGER,
            salt  DECIMAL(14,2)
         END RECORD,
         l2 RECORD
            dvg1  DECIMAL(14,2), #---Devengado cierre actual
            dvg2  DECIMAL(14,2), #---Devengado cierre mes anterior
            ingm  DECIMAL(14,2),  #---Ingresos mensuales
            dife  DECIMAL(14,2), #@008 --Diferido
            ingf  DECIMAL(14,2),  #--Ingresos finales
            sals  DECIMAL(14,2), #--Saldo Soles
            sven  DECIMAL(14,2), #--Saldo Vencido
            post  DECIMAL(14,2) #@012 --Ingresos posteados
            ,mo19 DECIMAL(14,2) #@013 ---- modulo 19
         END RECORD,
         l3 RECORD
            s002  DECIMAL(14,2),
            nume  INTEGER,
            s012  DECIMAL(14,2),
            sals  DECIMAL(14,2),
            sven  DECIMAL(14,2),
            s013  DECIMAL(14,2), #@007
            s014  DECIMAL(14,2),
            s029  DECIMAL(14,2),
            s032  DECIMAL(14,2),
            s033  DECIMAL(14,2), #@008
            s034  DECIMAL(14,2), #@008
            s038  DECIMAL(14,2),
            s042  DECIMAL(14,2),
            s043  DECIMAL(14,2),
            s044  DECIMAL(14,2),
            s049  DECIMAL(14,2),
            s053  DECIMAL(14,2),
            s063  DECIMAL(14,2),
            s068  DECIMAL(14,2),
            s069  DECIMAL(14,2),
            s070  DECIMAL(14,2),
            s072  DECIMAL(14,2),
            s073  DECIMAL(14,2),
            s074  DECIMAL(14,2),
            s075  DECIMAL(14,2),
            s077  DECIMAL(14,2),
            s078  DECIMAL(14,2),
            s079  DECIMAL(14,2),
            s083  DECIMAL(14,2),
            s093  DECIMAL(14,2),
            s094  DECIMAL(14,2),
            s097  DECIMAL(14,2),
            s098  DECIMAL(14,2),
            s099  DECIMAL(14,2),
            s100  DECIMAL(14,2), #@008
            s101  DECIMAL(14,2)
         END RECORD,
         l4 RECORD
            s012  DECIMAL(14,2),
            s014  DECIMAL(14,2),
            s029  DECIMAL(14,2),
            s031  DECIMAL(14,2),
            s032  DECIMAL(14,2),
            s033  DECIMAL(14,2), #@008
            s034  DECIMAL(14,2), #@008
            s036  DECIMAL(14,2),
            s037  DECIMAL(14,2),
            s038  DECIMAL(14,2),
            s042  DECIMAL(14,2),
            s043  DECIMAL(14,2),
            s044  DECIMAL(14,2),
            s049  DECIMAL(14,2),
            s051  DECIMAL(14,2),
            s052  DECIMAL(14,2),
            s053  DECIMAL(14,2),
            s063  DECIMAL(14,2),
            s064  DECIMAL(14,2),
            s065  DECIMAL(14,2),
            s066  DECIMAL(14,2),
            s067  DECIMAL(14,2),
            s068  DECIMAL(14,2),
            s069  DECIMAL(14,2),
            s070  DECIMAL(14,2),
            s071  DECIMAL(14,2),
            s072  DECIMAL(14,2),
            s073  DECIMAL(14,2),
            s074  DECIMAL(14,2),
            s075  DECIMAL(14,2),
            s076  DECIMAL(14,2),
            s077  DECIMAL(14,2),
            s078  DECIMAL(14,2),
            s079  DECIMAL(14,2),
            s081  DECIMAL(14,2),
            s082  DECIMAL(14,2),
            s083  DECIMAL(14,2),
            s093  DECIMAL(14,2),
            s094  DECIMAL(14,2),
            s095  DECIMAL(14,2),
            s096  DECIMAL(14,2),
            s097  DECIMAL(14,2),
            s098  DECIMAL(14,2),
            s099  DECIMAL(14,2),
            s100  DECIMAL(14,2),
            s101  DECIMAL(14,2),
            s102  DECIMAL(14,2)
         END RECORD,
         l_fant   DATE,
         #l_desc   CHAR(50),
         l_fini   DATE,
         l_npre   INTEGER,
         l_cana   INTEGER,
         l_para   DECIMAL(14,2),
         l_suel   DECIMAL(14,2),
         l_dife   DECIMAL(14,2),
         l_dif1   DECIMAL(14,2),
         l_dif2   DECIMAL(14,2),
         l_difp   DECIMAL(14,2),
         l_difa   DECIMAL(14,2),
         l_difi   DECIMAL(14,2),
         l_cade   CHAR(6),
         l_cad1   CHAR(6),
         l_ting   DECIMAL(14,2),
         l_ning   SMALLINT,
         l_ingf   DECIMAL(14,2),
         l_ing2   DECIMAL(14,2),
         l_stot   DECIMAL(14,2),
         l_sto1   DECIMAL(14,2),
         #@008 ini
         l_dinf   DECIMAL(14,2),
         l_degf   DECIMAL(14,2)
         #@008 fin

   LET l_npre = NULL
   EXECUTE p15_sel_tmp_datos_cana
   LET l_fant = p_fcie - DAY(p_fcie)
   LET l_fini = p_fcie - DAY(p_fcie) + 1

   EXECUTE p172_del_tmp_ajus #@008

   IF (g_tipo IS NULL) THEN #@005
      ERROR ""
      MESSAGE " Buscando provisiones ..!"
   END IF #@005
   CALL f007_llenar_datos_prov_cn559(p_fcie,l_fant)

   #@008 ini
   EXECUTE p173_del_tmp_pcpfu
   EXECUTE p172_del_tmp_pctdt

   EXECUTE p174_ins_tmp_pcpfu using l_fini,p_fcie

   #EXECUTE p173_ins_tmp_pctdt using l_fini,p_fcie,p_fcie
   #EXECUTE p176_ins_tmp_pctdt_rec using l_fini,p_fcie,p_fcie
   #@008 fin

   #@012 ini
   EXECUTE p18_del_tmp_pctcn
   EXECUTE p19_ins_tmp_pctcn USING l_fini,p_fcie
   #@013 inicio
   EXECUTE p19_ins_tmp_pctcn_pref USING l_fini,p_fcie #@018


   #EXECUTE p19_ins_tmp_pctcn_post USING l_fini,p_fcie
   EXECUTE p00_del_tmp_nprx
   EXECUTE p19_ins_tmp_pctcn_nprx
   EXECUTE p19_ins_tmp_pctcn_tpo_1
   EXECUTE p00_del_tmp_nprx
   EXECUTE p19_ins_tmp_pctcn_nprx_02
   EXECUTE p19_ins_tmp_pctcn_tpo_1_02
   EXECUTE p00_del_tmp_nprx

   EXECUTE p19_del_tmp_pctcn_02
   EXECUTE p19_ins_tmp_pctcn_post_02_1 USING l_fini, p_fcie, p_fcie
   EXECUTE p19_ins_tmp_pctcn_post_03 USING l_fini,p_fcie
   EXECUTE p19_ins_tmp_pctcn_post
   #@013 fin

   #@012 fin

   {
   EXECUTE p18_del_tmp_pctcn
   EXECUTE p19_ins_tmp_pctcn USING l_fini,p_fcie

   #---Buscando Analista a su cargo-----#
   EXECUTE p17_upt_tmp_pctcn USING p_fcie
   EXECUTE p18_upt_tmp_pctcn
   EXECUTE p17_upt_tmp_pctcn USING l_fant
    }

   #---Buscando datos de sueldo-----#
   LET l_para = NULL
   EXECUTE p55_sel_param_sueld INTO l_para

   EXECUTE p21_del_tmp_sudli
   LET l_cade = MONTH(p_fcie) USING "&&",YEAR(p_fcie)
   LET l_cad1 = MONTH(l_fant) USING "&&",YEAR(l_fant)

   EXECUTE p22_ins_tmp_sudli USING l_para,l_cade

   #---Buscando ingresos finales---#
   #EXECUTE p73_sel_pdvg
   EXECUTE p73_del_tmp_ingf #@007
   INITIALIZE l1.* TO NULL
   #FOREACH q74_sel_tot_rseg USING p_fcie,l_fant INTO l1.agen,l1.cana
   {
   FOREACH q74_sel_tot_rseg INTO l1.agen,l1.cana #@007
      INITIALIZE l2.* TO NULL
      EXECUTE p17_sel_pdvg USING p_fcie,l1.agen,l1.cana INTO l2.dvg1
      EXECUTE p17_sel_pdvg USING l_fant,l1.agen,l1.cana INTO l2.dvg2
      EXECUTE p13_sel_impi_tmp_pctcn USING l1.agen,l1.cana INTO l2.ingm
      LET l2.ingf = l2.dvg1 + l2.ingm - l2.dvg2
      EXECUTE p75_ins_tmp_ingf USING l1.agen,l1.cana,l2.dvg1,l2.dvg2,l2.ingm,l2.ingf
   END FOREACH
   }
   #@008 ini --Nueva formula

   #@013 Inicio
   EXECUTE p00_del_tmp_nprx
   EXECUTE p19_ins_tmp_pctcn_nprx_03
   EXECUTE p19_ins_tmp_pctcn_tpo_1_03
   EXECUTE p19_ins_tmp_pctcn_tpo_1_04 USING p_fcie
   EXECUTE p19_ins_tmp_pctcn_tpo_1_05
   #@013 Fin

   #@016 Inicio
   EXECUTE p00_del_tmp_nprx

   EXECUTE p119_ins_tmp_nprx USING l_fini, p_fcie, p_fcie, l_fini, p_fcie
   
   EXECUTE p19_ins_tmp_pctcn_tpo_1_06
   EXECUTE p19_ins_tmp_pctcn_tpo_1_07
   EXECUTE p00_del_tmp_nprx
   #@016 Fin


   INITIALIZE l1.* TO NULL
   FOREACH q74_sel_tot_rseg INTO l1.agen,l1.cana
    INITIALIZE l2.* TO NULL
    EXECUTE p17_sel_eacdc_pdvg USING p_fcie,l1.agen,l1.cana INTO l2.dvg1
    EXECUTE p17_sel_eacdc_pdvg USING l_fant,l1.agen,l1.cana INTO l2.dvg2
    #EXECUTE p174_sel_tmp_pctdt_impt USING l1.agen,l1.cana INTO l2.ingm
    EXECUTE p13_sel_impi_tmp_pctcn USING l1.agen,l1.cana,"1" INTO l2.ingm #@012
    EXECUTE p13_sel_impi_tmp_pctcn USING l1.agen,l1.cana,"2" INTO l2.post #@012
    EXECUTE p175_sel_pchid_impt USING p_fcie,l1.agen,l1.cana INTO l2.dife #--Ajustes manuales
    LET l2.ingf = (l2.dvg1 - l2.dvg2) + l2.ingm + l2.dife + l2.post #@012
    LET l2.mo19 = (l2.dvg1 - l2.dvg2) + l2.post #@013
    EXECUTE p75_ins_tmp_ingf
    #USING l1.agen,l1.cana,l2.dvg1,l2.dvg2,l2.ingm,l2.dife,l2.post,l2.ingf #@013 --@012
    USING l1.agen,l1.cana,l2.dvg1,l2.dvg2,l2.ingm,l2.dife,l2.post,l2.ingf, l2.mo19 #@013
   END FOREACH
   #@008 fin


   #CALL f007_quitar_admin_cn559(p_fcie,l_cade)
   #CALL f007_quitar_admin_cn559(l_fant,l_cad1)

   {IF p1.det2 = 'N' THEN
      EXECUTE p71_del_tmp_cobr
      EXECUTE p72_ins_tmp_cobr USING p_fcie

      EXECUTE p62_del_rseg_eaccc USING p_fcie,"6666"
      EXECUTE p62_del_rseg_eaccc USING p_fcie,"9999"
      EXECUTE p62_del_rseg_eaccc USING l_fant,"6666"
      EXECUTE p62_del_rseg_eaccc USING l_fant,"9999"
   END IF

   #---Buscando analistas
   EXECUTE p14_del_tmp_rseg
   EXECUTE p15_ins_tmp_rseg using p_fcie
   EXECUTE p15_ins_tmp_rseg using l_fant
}

   LET l_stot = NULL
   EXECUTE p78_sel_tot_cob INTO l_stot

   IF (g_tipo IS NULL) THEN #@005
      MESSAGE " Buscando datos por Analista ..!"
   END IF #@005
   INITIALIZE l1.* TO NULL

   CALL f022_gastos_adm_cobr_cn559(75) #@008
   FOREACH q10_sel_gbofi USING p1.agei,p1.agef INTO l1.agen #@008 #@007 ,l_desc

      INITIALIZE l3.*,l4.* TO NULL
      EXECUTE p13_sel_cont_tmp_eaccc USING p_fcie,l1.agen INTO l3.nume

      LET l_difi = 0
      IF p1.det2 = 'N' THEN
         FOREACH q77_sel_ingf_no_cons USING l1.agen,l1.agen INTO l_cana,l_ingf
            LET l_ing2 = 0 #@011
            IF l_cana = 9999 THEN
               LET l_sto1 = NULL
               EXECUTE p79_sel_tot_agen USING l1.agen INTO l_sto1
               IF l_stot !=0 THEN #@011
                    LET l_ing2 = l_ingf / l3.nume #@014
                    --LET l_ing2 = ((l_sto1/l_stot) * l_ingf) / l3.nume #@014
               END IF #@011
            ELSE
               LET l_ing2 = l_ingf / l3.nume
            END IF
            LET l_difi = l_difi + l_ing2
         END FOREACH

         #@011 ini
         IF l_difi IS NULL THEN
            LET l_difi = 0
         END IF
         #@011 fin

         #@008 ini
         IF (l_difi !=0) THEN
            INSERT INTO tmp_ajus VALUES(l1.agen,0,l_difi)
        END IF
        #@008 fin
      END IF

      LET l_ting = 0
      LET l_ning = 0
      EXECUTE p82_sel_ingf_tot USING l1.agen,l1.agen INTO l_ting,l_ning
      LET l_ting = l_ting + (l_ning * l_difi)

      EXECUTE p19_sel_sals_agen USING p_fcie,l1.agen INTO l3.sals
      EXECUTE p25_sel_sven_agen USING p_fcie,l1.agen INTO l3.sven

      LET l3.s002 = buscar_saldo_a(2,l1.agen)
      LET l_dif2 = l3.s002 - l_ting

      LET l3.s012 = buscar_saldo_a(12,l1.agen)
      LET l3.s014 = buscar_saldo_a(14,l1.agen)
      LET l3.s029 = buscar_saldo_a(29,l1.agen)

      LET l3.s013 = l3.s014 + l3.s029 #@007

      #LET l3.s032 = buscar_saldo_a(32,l1.agen)
      LET l3.s033 = buscar_saldo_a(33,l1.agen) #@008
      LET l3.s034 = buscar_saldo_a(34,l1.agen) #@008

      LET l3.s038 = buscar_saldo_a(38,l1.agen)
      LET l3.s042 = buscar_saldo_a(42,l1.agen)

      LET l3.s044 = buscar_saldo_a(44,l1.agen)
      LET l3.s049 = buscar_saldo_a(49,l1.agen)

      LET l3.s053 = buscar_saldo_a(53,l1.agen)
      LET l3.s063 = buscar_saldo_a(63,l1.agen)

      LET l3.s069 = buscar_saldo_a(69,l1.agen)
      LET l3.s070 = buscar_saldo_a(70,l1.agen)

      LET l3.s072 = buscar_saldo_a(72,l1.agen)
      LET l3.s073 = buscar_saldo_a(73,l1.agen)
      LET l3.s074 = buscar_saldo_a(74,l1.agen)

      #LET l3.s075 = buscar_saldo_a(75,l1.agen) #@008

      LET l3.s077 = buscar_saldo_a(77,l1.agen)
      LET l3.s078 = buscar_saldo_a(78,l1.agen)
      LET l3.s079 = buscar_saldo_a(79,l1.agen)

      LET l3.s083 = buscar_saldo_a(83,l1.agen)
      LET l3.s093 = buscar_saldo_a(93,l1.agen)
      LET l3.s094 = buscar_saldo_a(94,l1.agen)

      LET l3.s097 = buscar_saldo_a(97,l1.agen)
      LET l3.s098 = buscar_saldo_a(98,l1.agen)
      LET l3.s099 = buscar_saldo_a(99,l1.agen)

      LET l3.s101 = buscar_saldo_a(101,l1.agen)

      #----Diferencia de provisiones---#
      LET l_difp = NULL
      LET l_difp = l3.s033 - f009_buscar_varp_sede_cn559(p_fcie,l1.agen)

      #---Diferencia de ingresos financieros--#
      LET l_dinf = NULL
      LET l_dinf = l3.s038 - f023_buscar_ing_financ_cn559(l1.agen)

      #--Diferencia de gastos financierios--#
      LET l_degf = NULL
      LET l_degf = l3.s044 - f025_buscar_gast_financ_cn559(l1.agen)

      #---DATOS SUELDO---
      LET l_suel = NULL
      LET l_dife = NULL
      LET l_dif1 = NULL

      LET l3.s068 = buscar_saldo_a(68,l1.agen) #---
      EXECUTE p23_sel_tmp_sudli_uneg USING l1.agen,p_fcie,l1.agen INTO l_suel

      LET l_dife = l3.s068 - l_suel
      LET l_dif1 = l_dife / l3.nume

      LET l4.s012 = l3.s012 / l3.nume
      #LET l4.s029 = l3.s029 / l3.nume #@008
      LET l4.s034 = l3.s034 / l3.nume #@008

      LET l4.s053 = l3.s053 / l3.nume
      LET l4.s063 = l3.s063 / l3.nume

      LET l4.s069 = l3.s069 / l3.nume
      LET l4.s070 = l3.s070 / l3.nume

      #@005 ini
      #{
      LET l4.s072 = l3.s072 / l3.nume
      LET l4.s073 = l3.s073 / l3.nume
      LET l4.s074 = l3.s074 / l3.nume
      #}
      #@005 fin

      LET l4.s077 = l3.s077 / l3.nume
      LET l4.s078 = l3.s078 / l3.nume
      LET l4.s079 = l3.s079 / l3.nume

      LET l4.s083 = l3.s083 / l3.nume
      LET l4.s093 = l3.s093 / l3.nume
      LET l4.s094 = l3.s094 / l3.nume

      LET l4.s097 = l3.s097 / l3.nume
      LET l4.s098 = l3.s098 / l3.nume
      LET l4.s099 = l3.s099 / l3.nume

      LET l4.s101 = l3.s101 / l3.nume

      FOREACH q14_cana_tmp_eaccc USING l1.agen INTO l1.cana
         #---INGRESOS POR INTERESES AGENCIAS
         INITIALIZE l2.* TO NULL

         EXECUTE p20_sel_sals_cana USING p_fcie,l1.agen,l1.cana INTO l2.sals

         LET l1.codi = 2
         IF l1.cana != -1 THEN
            EXECUTE p76_sel_ingm USING l1.agen,l1.cana INTO l2.ingf
            LET l2.ingf = l2.ingf + l_difi
            #LET l2.ingf = l2.ingf + ((l2.sals/l3.sals) * l_dif2)
            LET l2.ingf = l2.ingf + ((l2.ingf/l_ting) * l_dif2) #@008
            LET l1.salt = l2.ingf
            EXECUTE p16_ins_tmp_datos_cana USING l1.*
        END IF

         #---INGRESOS POR INTERESES CENTRAL
         LET l1.codi = 12
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s012
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #@009 ini
         IF l2.ingf IS NULL THEN
            LET l2.ingf = 0
         END IF
         #@009 fin

         #---INGRESOS POR INTERES TOTAL--#
         LET l1.codi = 1
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s012 + l2.ingf
         ELSE
            LET l1.salt = l2.ingf
         END IF

         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #-GASTOS POR INTERESES AGENCIAS
         LET l1.codi = 14
         LET l4.s014 = (l2.sals / l3.sals) * l3.s014
         LET l1.salt = l4.s014
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         EXECUTE p26_sel_sven_cana USING p_fcie,l1.agen,l1.cana INTO l2.sven #--SALDO VENCIDO

         #-GASTOS POR INTERESES CENTRAL
         LET l1.codi = 29
         #@008 ini
         LET l4.s029 = (l2.sals/l3.sals) * l3.s029
         LET l1.salt = l4.s029
         {IF l2.sals > 0 THEN
            LET l1.salt = l4.s029
         ELSE
            LET l1.salt = 0
         END IF}
         #@008 fin
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--GASTOS POR INTERESES TOTAL
         LET l1.codi = 13
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s014 + l4.s029
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--MARGEN FINANCIERO BRUTO FINAL
         LET l1.codi = 31
         IF l2.sals > 0 THEN
            LET l4.s031 = (l4.s012 + l2.ingf) - (l4.s014 + l4.s029)
         ELSE
            LET l4.s031 = l2.ingf
         END IF
         LET l1.salt = l4.s031
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--Provisiones para Créditos directos Agencias
         LET l1.codi = 33
         LET l_difa = (l2.sven / l3.sven) * l_difp
         #@008 ini
         IF l_difa IS NULL THEN
            LET l_difa = 0
         END IF
         #@008 fin
         LET l4.s033 = f008_buscar_varp_cn559(p_fcie,l1.agen,l1.cana) + l_difa

         LET l1.salt = l4.s033
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--Provisiones para Créditos directos Central
         LET l1.codi = 34
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s034
         ELSE
            LET l1.salt = 0
         END IF
         LET l4.s034 = l1.salt
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--Provisiones para Créditos directos Total
         LET l1.codi = 32 #---Por definir---#
         #@008 ini
         {LET l_difa = (l2.sven / l3.sven) * l_difp
         LET l4.s032 = f008_buscar_varp_cn559(p_fcie,l1.agen,l1.cana) + l_difa
         }
         LET l4.s032 = l4.s033 + l4.s034
         LET l1.salt = l4.s032
         #@008 fin
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--MARGEN FINANCIERO NETO FINAL
         LET l1.codi = 36
         LET l4.s036 = l4.s031 - l4.s032
         LET l1.salt = l4.s036
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--38	INGRESOS POR SERVICIOS FINANCIEROS AGENCIAS
         LET l1.codi = 38
         #LET l4.s038 = (l2.sals / l3.sals) * l3.s038
         LET l_difa = (l2.sals / l3.sals) * l_dinf #@008
         LET l4.s038 = f024_buscar_ing_financ_cana_cn559(l1.agen,l1.cana)+ l_difa #@008
         LET l1.salt = l4.s038
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--42	INGRESOS POR SERVICIOS FINANCIEROS CENTRAL
         LET l1.codi = 42
         LET l4.s042 = (l2.sals / l3.sals) * l3.s042
         LET l1.salt = l4.s042
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--37	INGRESOS POR SERVICIOS FINANCIEROS TOTAL
         LET l1.codi = 37
         LET l4.s037 = l4.s038 + l4.s042
         LET l1.salt = l4.s037
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--44	GASTOS POR SERVICIOS FINANCIEROS AGENCIA
         LET l1.codi = 44
         #LET l4.s044 = (l2.sals / l3.sals) * l3.s044
         LET l_difa = (l2.sals / l3.sals) * l_degf #@008 ini
         IF l_difa IS NULL THEN
            LET l_difa = 0
         END IF
         #@008 fin
         LET l4.s044 = f026_buscar_gast_financ_cana_cn559(l1.agen,l1.cana)+ l_difa #@008
         LET l1.salt = l4.s044
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #--49	GASTOS POR SERVICIOS FINANCIEROS CENTRAL
         LET l1.codi = 49
         LET l4.s049 = (l2.sals / l3.sals) * l3.s049
         LET l1.salt = l4.s049
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #--43	GASTOS POR SERVICIOS FINANCIEROS TOTAL
         LET l1.codi = 43
         LET l4.s043 = l4.s044 + l4.s049
         LET l1.salt = l4.s043
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--51	MARGEN FINANCIERO NETO CENTRAL DE INGRESOS Y GASTOS POR SERVICIOS FINANCIEROSs
         LET l1.codi = 51
         LET l4.s051 = l4.s036 + l4.s037 - l4.s043
         LET l1.salt = l4.s051
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--53	RESULTADOS POR OPERACIONES FINANCIERAS (ROF) AGENCIAS
         LET l1.codi = 53
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s053
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #--63	RESULTADOS POR OPERACIONES FINANCIERAS (ROF) CENTRAL
         LET l1.codi = 63
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s063
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #--52	RESULTADOS POR OPERACIONES FINANCIERAS (ROF) TOTAL
         LET l1.codi = 52
         IF l2.sals > 0 THEN
            LET l4.s052 = l4.s053 + l4.s063
         ELSE
            LET l4.s052 = 0
         END IF
         LET l1.salt = l4.s052
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--64 OTROS INGRESOS Y GASTOS POR SERVICIOS FINANCIEROS
         LET l1.codi = 64
         LET l4.s064 = l4.s037 - l4.s043 + l4.s052
         LET l1.salt = l4.s064
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #------------------------------------------

         #--65	MARGEN OPERACIONAL
         LET l1.codi = 65
         LET l4.s065 = l4.s051 + l4.s052
         LET l1.salt = l4.s065
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--68	Gastos de Personal y Directorio Agencias
         LET l1.codi = 68
         EXECUTE p24_sel_tmp_sudli_rseg USING l1.agen,l1.cana INTO l4.s068
         IF l2.sals > 0 THEN
            LET l4.s068 = l4.s068 + l_dif1
         ELSE
            LET l4.s068 = 0
         END IF
         LET l1.salt = l4.s068
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--69	Gastos por Servicios Recibidos de Terceros Agencias
         LET l1.codi = 69
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s069
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--70	Impuestos y Contribuciones Agencias
         LET l1.codi = 70
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s070
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #--67	GASTOS DE  ADMINISTRACION AGENCIAS
         LET l1.codi = 67
         IF l2.sals > 0 THEN
            LET l4.s067 = l4.s068 + l4.s069 + l4.s070
         ELSE
            LET l4.s067 = 0
         END IF
         LET l1.salt = l4.s067
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #72 Gastos de Personal y Directorio Central
         LET l1.codi = 72
         IF l2.sals > 0 THEN
            #@008 LET l4.s072 = (l2.sals / l3.sals) * l3.s072 #@005
            LET l1.salt = l4.s072
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #73 Gastos por Servicios Recibidos de Terceros Central - Sin provisiÓn
         LET l1.codi = 73
         IF l2.sals > 0 THEN
            #@008 LET l4.s073 = (l2.sals / l3.sals) * l3.s073 #@005
            LET l1.salt = l4.s073
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #74 Impuestos y Contribuciones Central
         LET l1.codi = 74
         IF l2.sals > 0 THEN
            #@008 LET l4.s074 = (l2.sals / l3.sals) * l3.s074 #@005
            LET l1.salt = l4.s074
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #71	GASTOS DE ADMINISTRACION CENTRAL
         LET l1.codi = 71
         IF l2.sals > 0 THEN
            LET l4.s071 = l4.s072 + l4.s073 + l4.s074
         ELSE
            LET l4.s071 = 0
         END IF
         LET l1.salt = l4.s071
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #75	Gastos Administracion Cobranzas
         {#@008 ini
         LET l1.codi = 75
         #EXECUTE p26_sel_sven_cana USING p_fcie,l1.agen,l1.cana INTO l2.sven
         LET l4.s075 = (l2.sven / l3.sven) * l3.s075
         LET l1.salt = l4.s075
         EXECUTE p16_ins_tmp_datos_cana USING l1.*}
         LET l1.codi = 75
         LET l4.s075 = f006_buscar_saldo_analist_cn559(l1.agen,l1.cana,75)
         LET l1.salt = l4.s075
         #EXECUTE p16_ins_tmp_datos_cana USING l1.* #@009
         #@008 fin

         #--66	GASTOS DE ADMINISTRACION TOTAL
         LET l1.codi = 66
         LET l4.s066 = l4.s067 + l4.s071 + l4.s075
         LET l1.salt = l4.s066
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #77	DEPRECIACIÓN Y AMORTIZACIÓN AGENCIAS
         LET l1.codi = 77
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s077
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #78	DEPRECIACIÓN Y AMORTIZACIÓN CENTRAL
         LET l1.codi = 78
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s078
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #79	DEPRECIACIÓN Y AMORTIZACIÓN COBRANZAS
         LET l1.codi = 79
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s079
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         # 76	DEPRECIACIÓN Y AMORTIZACIÓN TOTAL
         LET l1.codi = 76
         IF l2.sals > 0 THEN
            LET l4.s076 = l4.s077 + l4.s078 + l4.s079
         ELSE
            LET l4.s076 = 0
         END IF
         LET l1.salt = l4.s076
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #81	MARGEN OPERACIONAL NETO FINAL
         LET l1.codi = 81
         LET l4.s081 = l4.s065 - l4.s066 - l4.s076
         LET l1.salt = l4.s081
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #83	VALUACIÓN DE ACTIVOS Y PROVISIONES AGENCIAS
         LET l1.codi = 83
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s083
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #93	VALUACIÓN DE ACTIVOS Y PROVISIONES CENTRAL
         LET l1.codi = 93
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s093
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #94 VALUACIÓN DE ACTIVOS Y PROVISIONES COBRANZAS
         LET l1.codi = 94
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s094
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #82	VALUACIÓN DE ACTIVOS Y PROVISIONES TOTAL
         LET l1.codi = 82
         IF l2.sals > 0 THEN
            LET l4.s082 = l4.s083 + l4.s093 + l4.s094
         ELSE
            LET l4.s082 = 0
         END IF
         LET l1.salt = l4.s082
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #95	RESULTADO DE OPERACION FINAL
         LET l1.codi = 95
         LET l4.s095 = l4.s081 - l4.s082
         LET l1.salt = l4.s095
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #97	Otros ingresos y gastos
         LET l1.codi = 97
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s097
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #98	Otros ingresos y gastos Central
         LET l1.codi = 98
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s098
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #99	Otros ingresos y gastos Cobranzas
         LET l1.codi = 99
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s099
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
         #96	OTROS INGRESOS Y GASTOS TOTAL
         LET l1.codi = 96
         IF l2.sals > 0 THEN
            LET l4.s096 = l4.s097 + l4.s098 + l4.s099
         ELSE
            LET l4.s096 = 0
         END IF
         LET l1.salt = l4.s096
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #100	RESULTADOS DEL EJERCICIO ANTES DEL IMPUESTO A LA RENTA
         LET l1.codi = 100
         LET l4.s100 = l4.s095 + l4.s096
         LET l1.salt = l4.s100
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         {
         #101	IMPUESTO A LA RENTA
         LET l1.codi = 101
         IF l2.sals > 0 THEN
            LET l1.salt = l4.s101
         ELSE
            LET l1.salt = 0
         END IF
         EXECUTE p16_ins_tmp_datos_cana USING l1.*

         #102	RESULTADO NETO DEL EJERCICIO
         LET l1.codi = 102
         IF l2.sals > 0 THEN
            LET l4.s102 = l4.s100 + l4.s101
         ELSE
            LET l4.s102 = l4.s100
         END IF
         LET l1.salt = l4.s102
         EXECUTE p16_ins_tmp_datos_cana USING l1.*
        }
      END FOREACH
   END FOREACH

   #@008 ini
   FOREACH q10_sel_gbofi USING p1.agei,p1.agef INTO l1.agen #@008
    LET l3.s101 = buscar_saldo_a(101,l1.agen)
    LET l3.s100 = f028_buscar_saldo_positivo_cn559(100,l1.agen)

    FOREACH q14_cana_tmp_eaccc USING l1.agen INTO l1.cana

        LET l4.s100 = f006_buscar_saldo_analist_cn559(l1.agen,l1.cana,100)
        LET l4.s101 = NULL

        #101 IMPUESTO A LA RENTA
        LET l1.codi = 101
        IF l4.s100 > 0 THEN
            LET l4.s101 = (l4.s100/l3.s100) * l3.s101
            LET l1.salt = l4.s101
        ELSE
            LET l1.salt = 0
        END IF

        EXECUTE p16_ins_tmp_datos_cana USING l1.*

        #102	RESULTADO NETO DEL EJERCICIO
        LET l1.codi = 102
        LET l4.s102 = l4.s100 + l1.salt
        LET l1.salt = l4.s102
        EXECUTE p16_ins_tmp_datos_cana USING l1.*
    END FOREACH
   END FOREACH
   #@008 fin

END FUNCTION
#@004 fin

#@004 ini
FUNCTION f006_buscar_saldo_analist_cn559(p_nofi,p_cana,p_codi)
DEFINE   p_nofi   SMALLINT,
         p_cana   INTEGER,
         p_codi   SMALLINT,
         l_sald   DECIMAL(14,2)

   LET l_sald = 0
   EXECUTE p14_sel_salt_cana USING p_codi,p_nofi,p_cana INTO l_sald
   #@008 ini
   IF l_sald IS NULL THEN
        LET l_sald = 0
   END IF
   #@008 fin

RETURN l_sald
END FUNCTION
#@004 fin

#@004 ini
FUNCTION f007_llenar_datos_prov_cn559(p_fcie,p_fant)
DEFINE   p_fcie  DATE,
         p_fant  DATE,
         l_texp  SMALLINT,
         #@008 ini
         l_tipo  SMALLINT,
         l_fant  DATE,
         l1 RECORD
            impi    FLOAT,
            agen    SMALLINT,
            cana    INTEGER,
            prim    FLOAT,
            neto    FLOAT,
            imp1    FLOAT,
            imp2    FLOAT,
            impo    DECIMAL(14,2)
         END RECORD,
         l_porc FLOAT,
         l_por1 FLOAT,
         l_por2 FLOAT,
         l_por3 FLOAT,
         #@008 fin
         #@009 ini
         l2 RECORD
            agen    SMALLINT,
            npre    INTEGER,
            cana    INTEGER,
            datr    INTEGER
         END RECORD,
         l_desc CHAR(40),
         l_impo DECIMAL(14,2),
         l_xdia SMALLINT
         #@009 fin

      EXECUTE p36_del_tmp_varp
      EXECUTE p39_ins_tmp_varp USING p_fcie

      #@008 ini
      LET l_fant = NULL
      LET l_fant = p_fcie - DAY(p_fcie) + 1
      EXECUTE p145_del_tmp_pvrep
      EXECUTE p146_ins_tmp_pvrep USING p_fcie,p_fant,p_fcie

      LET l_tipo = 1
      EXECUTE p147_ins_tmp_varp_01 USING p_fcie,p_fant,l_tipo,l_tipo
      EXECUTE p148_ins_temp_varp_02 USING p_fcie,p_fant,l_tipo,l_tipo
      LET l_tipo = 2
      EXECUTE p147_ins_tmp_varp_01 USING p_fcie,p_fant,l_tipo,l_tipo
      EXECUTE p148_ins_temp_varp_02 USING p_fcie,p_fant,l_tipo,l_tipo

      #---Importe pagado pctdtstat=7---#
      EXECUTE p149_ins_temp_varp_03 USING p_fcie,l_fant,p_fcie
      #@009 ini--Buscando analistas origen
      LET l_xdia = NULL
      EXECUTE p109_sel_pcprm_valo USING "555","27" INTO l_xdia

      IF p1.det2 = 'N' THEN
        INITIALIZE l2.* TO NULL
        FOREACH q176_sel_prov_cob INTO l2.agen,l2.npre

            EXECUTE p185_sel_eaccc_datr USING p1.fech,l2.npre INTO l2.datr
            IF l2.datr IS NULL OR l2.datr <=l_xdia THEN
                EXECUTE p175_sel_agen_orig USING l2.npre,l2.agen INTO l2.cana
                EXECUTE p177_upd_varp USING l2.cana,l2.npre
                IF (l2.cana = 6666 OR l2.cana = 9999) THEN
                    LET l2.cana = -1 #--Ajuste de sede
                    EXECUTE p177_upd_varp USING l2.cana,l2.npre
                END IF
            END IF
        END FOREACH

        FOREACH q_oficina USING p1.agei,p1.agef INTO l2.agen,l_desc
            LET l_impo = NULL
            LET l_impo = f008_buscar_varp_cn559(p_fcie,l2.agen,-1)
            IF l_impo !=0 THEN
                INSERT INTO tmp_rseg VALUES(l2.agen,-1)
            END IF
        END FOREACH
      END IF
      #@009 fin

      EXECUTE p150_del_tmp_ings
      #--Importe desgravamen--#
      LET l_tipo = 1
      INITIALIZE l1.* TO NULL
      EXECUTE p109_sel_pcprm_valo USING "555","16" INTO l_porc
      LET l_porc = l_porc/100
      FOREACH q151_sel_pcsgr USING p_fcie INTO l1.agen,l1.cana,l1.prim
        LET l1.neto = l1.prim/(1+l_porc)
        LET l1.impo = l1.prim - l1.neto
        EXECUTE p152_ins_tmp_ings USING l_tipo,l1.agen,l1.cana,l1.impo
      END FOREACH

      #--Importe microseguro--#
      LET l_tipo = 2
      LET l_porc = NULL
      LET l_por1 = NULL
      LET l_por2 = NULL
      LET l_por3 = NULL
      EXECUTE p109_sel_pcprm_valo USING "555","18" INTO l_por1
      EXECUTE p109_sel_pcprm_valo USING "555","19" INTO l_por2
      EXECUTE p109_sel_pcprm_valo USING "555","20" INTO l_por3
      LET l_por3 = l_por3 / 100
      INITIALIZE l1.* TO NULL
      FOREACH q153_sel_pcdmt USING p_fcie INTO l1.impi,l_porc,l1.agen,l1.cana,l1.prim
        LET l_porc = l_porc/100
        LET l1.prim = (l1.prim/l_por1)/l_por2
        LET l1.neto = l1.prim/(1+l_porc)

        LET l1.imp1 = l1.prim - l1.neto
        LET l1.imp2 = l1.neto * l_porc

        LET l1.impo = l1.imp1 + l1.imp2
        EXECUTE p152_ins_tmp_ings USING l_tipo,l1.agen,l1.cana,l1.impo
      END FOREACH

      #--Importe constancia de no adeudo--#
      EXECUTE p154_ins_tmp_ings_pccna USING l_fant,p_fcie,p_fcie

      #--Gastos financieros--#
      EXECUTE p159_del_tmp_gfin
      #1:Cobro corresponsalia 2:Desembolso corresponsalia
      EXECUTE p160_ins_tmp_gfin_1 USING l_fant,p_fcie,p_fcie
      EXECUTE p161_ins_tmp_gfin_2 USING l_fant,p_fcie,p_fcie

      EXECUTE p162_ins_tmp_gfin_3 USING l_fant,p_fcie,p_fcie

      #EXECUTE p39_ins_tmp_varp USING p_fant
      {
      IF (g_tipo IS NULL) THEN #@005
         EXECUTE p37_ind_tmp_varp_01
         EXECUTE p38_ind_tmp_varp_02
      END IF #@005

      EXECUTE p40_del_tmp_vari
      EXECUTE p41_ins_tmp_vari USING p_fcie,p_fant

      EXECUTE p42_upd_tmp_varp_1
      EXECUTE p43_upd_tmp_varp_prov USING p_fcie

      LET l_texp = 3
      EXECUTE p44_upd_tmp_varp_tipo USING l_texp,p_fcie
      LET l_texp = 2
      EXECUTE p44_upd_tmp_varp_tipo USING l_texp,p_fant

      EXECUTE p58_del_tmp_varp USING p_fcie,"1"
      EXECUTE p58_del_tmp_varp USING p_fant,"0"
    }

    #@008 fin

END FUNCTION
#@004 fin

#@004 ini
FUNCTION f008_buscar_varp_cn559(p_fcie,p_agen,p_cana)
DEFINE   p_fcie   DATE,
         p_agen   SMALLINT,
         p_cana   INTEGER,
         l_tip1   SMALLINT,
         l_tip2   SMALLINT,
         l_var1   DECIMAL(14,2),
         l_var2   DECIMAL(14,2),
         l_scas   DECIMAL(14,2), #--Saldo castigado
         l_varp   DECIMAL(14,2)

   LET l_varp = 0
   EXECUTE p46_sel_tmp_varp_varp USING p_agen,p_cana INTO l_varp

{#@008 ini
   LET l_var1 = 0
   LET l_var2 = 0
   LET l_scas = 0

   LET l_tip1 = 1
   LET l_tip2 = 3
   EXECUTE p46_sel_tmp_varp_varp USING p_agen,p_cana,l_tip1,l_tip2 INTO l_var1

   LET l_tip1 = 2
   LET l_tip2 = 2
   EXECUTE p46_sel_tmp_varp_varp USING p_agen,p_cana,l_tip1,l_tip2 INTO l_var2

   EXECUTE p45_sel_tmp_eaccc_cast USING p_fcie,p_fcie,p_agen,p_cana INTO l_scas

   LET l_varp = l_var1 - l_var2 + l_scas
}#@008 fin

RETURN l_varp
END FUNCTION
#@004 fin

#@004 ini
FUNCTION f009_eliminar_indices_cn559(p_deta)
DEFINE p_deta  CHAR(1)

   IF p1.deta = 'S' THEN
      EXECUTE p47_drop_ind_tmp_eaccc_01
      EXECUTE p48_drop_ind_tmp_eaccc_02

      EXECUTE p70_drop_ind_tmp_cndtr

      EXECUTE p49_drop_ind_tmp_saldos_01
      EXECUTE p50_drop_ind_tmp_saldos_cen_01
      EXECUTE p51_drop_ind_tmp_saldos_ag_01
      EXECUTE p52_drop_ind_tmp_saldos_cb_01
      {#@008 ini
      EXECUTE p53_drop_ind_tmp_varp_01
      EXECUTE p54_drop_ind_tmp_varp_02
      }#@008 fin
   ELSE
      EXECUTE p70_drop_ind_tmp_cndtr

      EXECUTE p49_drop_ind_tmp_saldos_01
      EXECUTE p50_drop_ind_tmp_saldos_cen_01
      EXECUTE p51_drop_ind_tmp_saldos_ag_01
      EXECUTE p52_drop_ind_tmp_saldos_cb_01

      #EXECUTE p158_drop_ind_pctcn #@008
   END IF

END FUNCTION
#@004 fin

#@004 ini
FUNCTION f009_buscar_varp_sede_cn559(p_fcie,p_agen)
DEFINE   p_fcie   DATE,
         p_agen   SMALLINT,
         l_tip1   SMALLINT,
         l_tip2   SMALLINT,
         l_var1   DECIMAL(14,2),
         l_var2   DECIMAL(14,2),
         l_scas   DECIMAL(14,2), #--Saldo castigado
         l_varp   DECIMAL(14,2)

         LET l_varp = 0
         EXECUTE p56_sel_tmp_varp_agen USING p_agen INTO l_varp
         IF l_varp IS NULL THEN
            LET l_varp = 0
         END IF
         #@008 ini
{
   LET l_var1 = 0
   LET l_var2 = 0
   LET l_scas = 0

   LET l_tip1 = 1
   LET l_tip2 = 3
   EXECUTE p56_sel_tmp_varp_agen USING p_agen,l_tip1,l_tip2 INTO l_var1

   LET l_tip1 = 2
   LET l_tip2 = 2
   EXECUTE p56_sel_tmp_varp_agen USING p_agen,l_tip1,l_tip2 INTO l_var2

   EXECUTE p57_sel_tmp_eaccc_cast USING p_fcie,p_fcie,p_agen INTO l_scas

   LET l_varp = l_var1 - l_var2 + l_scas
} #@008 fin

RETURN l_varp
END FUNCTION
#@004 fin

#@004 ini
FUNCTION f006_buscar_saldo_agencia_cn559(p_nofi,p_codi)
DEFINE   p_nofi   SMALLINT,
         p_codi   SMALLINT,
         l_sald   DECIMAL(14,2)

   LET l_sald = 0
   LET l_sald = buscar_saldo_a(37,p_nofi) - buscar_saldo_a(43,p_nofi) + buscar_saldo_a(52,p_nofi)

RETURN l_sald
END FUNCTION
#@004 fin

#@004 ini
FUNCTION f007_quitar_admin_cn559(p_fech,p_cade)
DEFINE   p_fech   DATE,
         p_cade   CHAR(6),
         l_rseg   INTEGER

      FOREACH q61_sel_rseg USING p_cade INTO l_rseg
         EXECUTE p62_del_rseg_eaccc USING p_fech,l_rseg
      END FOREACH

END FUNCTION
#@004 fin

#@005 ini
FUNCTION f008_llenar_eaccc_cn559()
DEFINE   l_fant   DATE,
         l_cade   CHAR(6),
         l_cad1   CHAR(6)

   EXECUTE p11_del_tmp_eaccc
   EXECUTE p27_del_tmp_codia

   LET l_cade = MONTH(p1.fech) using "&&",YEAR(p1.fech)
   EXECUTE p28_ins_tmp_codia_1 USING l_cade
   EXECUTE p12_ins_tmp_eaccc USING p1.fech,p1.agei,p1.agef #@009

   LET l_fant = p1.fech - DAY(p1.fech)
   LET l_cad1 = MONTH(l_fant) using "&&",YEAR(l_fant)
   EXECUTE p28_ins_tmp_codia_0 USING l_cad1
   EXECUTE p12_ins_tmp_eaccc USING l_fant,p1.agei,p1.agef #@009

   EXECUTE p15_indx01_tmp_eaccc
   EXECUTE p16_indx02_tmp_eaccc

   #@009 ini
   EXECUTE p117_del_tmp_rseg_ini
   EXECUTE p118_ins_tmp_rseg_ini USING p1.fech
   #EXECUTE p118_ins_tmp_rseg_ini USING l_fant #@013
   #@009 fin

   #@011 ini
   EXECUTE p29_del_tb_agenc
   INSERT INTO tb_agenc
   SELECT gbofinofi FROM gbofi;

   EXECUTE p71_del_tmp_cobr
   EXECUTE p72_ins_tmp_cobr USING p1.fech
   #@011 fin

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f009_distribuir_eg_cn559()
DEFINE   l2   RECORD
            agei  SMALLINT,
            agef  SMALLINT
         END RECORD

      EXECUTE p00_del_tmp_datos
      IF p1.vouc = 'N' THEN
         EXECUTE p63_del_tmp_saldos_cent
      END IF

      FOREACH q10_sel_gbofi USING p1.agei,p1.agef INTO g_agen #@008
         CALL f000_construir_egp()
      END FOREACH

      INITIALIZE l2.* TO NULL
      FOREACH q34_sel_pcprm INTO l2.agei,l2.agef
         EXECUTE p35_upd_agen_tmp_datos USING l2.agef,l2.agei
      END FOREACH

      #@007 ini
      CALL f019_ajustar_tmp_datos_cn559()
      CALL f021_actualizar_calculo_cn559()
      #@007 fin

	   CALL f003_distribuir_impuestos_ea017a()
END FUNCTION

#@005 ini
FUNCTION f010_crea_saldo_ea017_v3(p_tipo)
DEFINE   p_tipo   SMALLINT,
         l_fant   DATE

   EXECUTE p66_del_tmp_cndtr
   EXECUTE p01_del_tmp_saldos
   #EXECUTE p29_del_tb_agenc #@011

   EXECUTE p63_del_tmp_saldos_cent
   EXECUTE p64_del_tmp_saldos_ag
   EXECUTE p65_del_tmp_saldos_cobr

   IF p_tipo = 1 THEN
      EXECUTE p67_ins_tmp_cndtr_acum USING p1.fech,p1.agei,p1.agef #@008
   ELSE
      LET l_fant = p1.fech - DAY(p1.fech) + 1
      EXECUTE p68_ins_tmp_cndtr_mens USING l_fant,p1.fech,p1.agei,p1.agef #@008
   END IF

   EXECUTE p02_ins_tmp_saldos
   EXECUTE p04_ins_tmp_saldos_cent
   EXECUTE p06_ins_tmp_saldos_ag
   EXECUTE p08_ins_tmp_saldos_cobr

	UPDATE TMP_SALDOS
	SET IMPI = 0
	WHERE IMPI IS NULL

	UPDATE TMP_SALDOS_CENT
	SET IMPI = 0
	WHERE IMPI IS NULL

	UPDATE TMP_SALDOS_AG
	SET IMPI = 0
	WHERE IMPI IS NULL

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f011_llenar_temporal_x_sede()

   EXECUTE p83_del_tmp_cnegs USING p1.tipo
   EXECUTE p84_ins_tmp_cnegs
   EXECUTE p85_upd_tmp_cnegs USING p1.tipo

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f012_act_temporal_x_sede()
DEFINE   l1 RECORD
            age1  SMALLINT,
            cod1  SMALLINT
         END RECORD,
         l_sald   DECIMAL(14,2)

   INITIALIZE l1.* TO NULL
   FOREACH q86_sel_tmp_cnegs USING p1.tipo INTO l1.age1,l1.cod1

      LET l_sald = NULL
      EXECUTE p81_sel_tmp_datos_salt USING l1.cod1,l1.age1 INTO l_sald
      IF l_sald IS NULL THEN
         LET l_sald = 0
      END IF

      EXECUTE p86_upd_tmp_cnegs_02 USING l_sald,p1.tipo,l1.age1,l1.cod1

   END FOREACH

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f013_crear_ind_tmp_cn559()

   EXECUTE p69_ind_tmp_cndtr
   EXECUTE p30_indx_tmp_saldos
   EXECUTE p31_indx_tmp_saldos_cent
   EXECUTE p32_indx_tmp_saldos_ag
   EXECUTE p33_indx_tmp_saldos_cobr

   {#@008 ini
   EXECUTE p37_ind_tmp_varp_01
   EXECUTE p38_ind_tmp_varp_02
   } #@008 fin

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f014_llenar_temporal_x_rseg(p_subt)
DEFINE p_subt  SMALLINT

   EXECUTE p87_del_tmp_cnega USING p1.tipo,p_subt
   EXECUTE p88_ins_tmp_cnega
   EXECUTE p89_upd_tmp_cnega using p1.tipo,p_subt

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f015_act_temporal_x_rseg(p_subt)
DEFINE p_subt  SMALLINT

   EXECUTE p90_upd_tmp_cnega_02 USING p1.tipo,p_subt

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f016_detalle_egp_cn559(p_subt)
DEFINE   p_subt   SMALLINT

   CALL f010_crea_saldo_ea017_v3(p1.tipo)

   #@011 ini
   EXECUTE p17_del_tmp_cnega
   EXECUTE p18_del_tmp_cnegs
   #@011 fin

   LET p1.vouc = "S"
   CALL f009_distribuir_eg_cn559()
   CALL f005_llenar_datos_analist_cn559(p1.fech)

   CALL f011_llenar_temporal_x_sede()
   CALL f014_llenar_temporal_x_rseg(p_subt)

   LET p1.vouc = "N"
   CALL f009_distribuir_eg_cn559()
   CALL f005_llenar_datos_analist_cn559(p1.fech)

   CALL f012_act_temporal_x_sede()
   CALL f015_act_temporal_x_rseg(p_subt)

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f017_altas_egp_cn559(p_fcie)
DEFINE   p_fcie   DATE,
         l_ntra   INTEGER,
         l_ntr1   INTEGER,
         l_fpro   DATE,
         l_hora   CHAR(8)

   LET l_fpro = TODAY
   LET l_hora = TIME

   LET l_ntr1 = NULL
   EXECUTE p97_max_cnheg USING p_fcie INTO l_ntr1

   BEGIN WORK

      IF l_ntr1 IS NOT NULL THEN
         EXECUTE p98_del_cnegs USING l_ntr1
         EXECUTE p99_del_cnega USING l_ntr1
         EXECUTE p100_del_cnheg USING l_ntr1
      END IF

      LET l_ntra = 0
      EXECUTE p91_ins_cnheg USING p_fcie,g_user,l_fpro,l_hora
      LET l_ntra = SQLCA.SQLERRD[2]

      EXECUTE p92_upd_tmp_cnegs USING l_ntra
      EXECUTE p94_upd_tmp_cnega USING l_ntra

      EXECUTE p93_ins_cnegs ----Detalle de sede
      EXECUTE p95_ins_cnega ----Detalle de analista

   COMMIT WORK

END FUNCTION
#@005 fin

#@005 ini
FUNCTION f017_buscar_max_eaccc_cn559()
DEFINE   l_fcie  DATE

   LET l_fcie = NULL
   EXECUTE p96_max_eaccc INTO l_fcie

RETURN l_fcie
END FUNCTION
#@005 fin

#@006 inicio
FUNCTION f006_buscar_descripcion_pc2009(l_agen)
DEFINE	l_agen	INTEGER,
		l_desc	CHAR(50)

	LET l_desc=NULL

	SELECT GBOFIDESC INTO l_desc
	FROM GBOFI
	WHERE GBOFINOFI = l_agen

    RETURN l_desc
END FUNCTION
#@006 fin

#@004 inicio
FUNCTION crear_temp_sedes()
    DEFINE
        l_valo_upd CHAR(150),
        l_nofi_upd SMALLINT,
        #@008 ini
        l_flag      SMALLINT,
        l_agen      SMALLINT,
        l_nofi      SMALLINT,
        l_desc      CHAR(40),
        l_codi      INTEGER,
        l_sald_temp DECIMAL(14,2),
        l_sald_tem2 DECIMAL(14,2),
        l_sald      DECIMAL(14,2),
        l_salx      DECIMAL(14,2),
        l_nume      INTEGER,
        l_difp      DECIMAL(14,2),
        l_cana      INTEGER,
        l3_s100     DECIMAL(14,2),
        l3_s101     DECIMAL(14,2),
        l3_nume     SMALLINT,
        l4_s100     DECIMAL(14,2),
        l4_s101     DECIMAL(14,2),
        l1_salt     DECIMAL(14,2),
        l4_s102     DECIMAL(14,2)
        #@008 fin

    EXECUTE p103_del_tmp_sedes
    EXECUTE p101_ins_tmp_sedes

    #@008 ini
    LET l_nofi_upd = NULL
    LET l_valo_upd = NULL
    LET l_flag = 699
    FOREACH q165_sel_pcprm using l_flag INTO l_nofi_upd,l_valo_upd
        EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    END FOREACH

    LET l_codi = NULL
    FOREACH q166_sel_codi_tmp_datos INTO l_codi
        LET l_agen = NULL
        FOREACH q_agen USING p1.agei,p1.agef INTO l_agen #@008
            LET l_sald = 0
            LET l_nofi = NULL
            LET l_desc = NULL

            LET l_nume = NULL
            EXECUTE p167_sel_cont_cana USING l_agen INTO l_nume
            FOREACH q_sede USING l_agen INTO l_nofi, l_desc
                LET l_sald_temp = NULL
                IF l_codi = 64 THEN
                    LET l_sald_temp = f006_buscar_saldo_agencia_cn559(l_nofi,l_codi)
                ELSE
                    LET l_sald_temp = buscar_saldo_a(l_codi,l_nofi)
                END IF
                LET l_sald = l_sald + l_sald_temp
            END FOREACH

            LET l_salx = 0
            EXECUTE p60_sel_salt_agen USING l_codi,l_agen INTO l_salx
            IF l_salx IS NULL THEN
                LET l_salx = 0
            END IF

            LET l_difp = 0
            IF l_sald != 0 THEN
                IF l_sald !=l_salx THEN
                    LET l_difp = (l_sald - l_salx)/l_nume
                    IF l_difp IS NULL THEN
                        LET l_difp = 0
                    END IF
                    IF l_difp !=0 THEN
                        LET l_cana = NULL
                        FOREACH q169_sel_tmp_cana USING l_agen INTO l_cana
                            EXECUTE p16_ins_tmp_datos_cana USING l_codi,l_agen,l_cana,l_difp
                        END FOREACH
                    END IF
                END IF
            END IF
        END FOREACH
    END FOREACH

    LET l_agen = NULL
    EXECUTE p171_del_tmp_datos_cana_codi USING "101" #--Eliminar info cargada
    EXECUTE p171_del_tmp_datos_cana_codi USING "102" #--Eliminar info cargada

    FOREACH q_agen USING p1.agei,p1.agef INTO l_agen #@008
        LET l_sald = 0
        LET l3_s100 = 0
        FOREACH q_sede USING l_agen INTO l_nofi, l_desc
            LET l_sald_temp = NULL
            LET l_sald_tem2 = NULL
            LET l_sald_temp = buscar_saldo_a(101,l_nofi)
            LET l_sald_tem2 = f028_buscar_saldo_positivo_cn559(100,l_nofi)

            LET l_sald = l_sald + l_sald_temp
            LET l3_s100 = l3_s100 + l_sald_tem2
        END FOREACH

        LET l3_s101 = l_sald
        FOREACH q14_cana_tmp_eaccc USING l_agen INTO l_cana
            LET l4_s100 = f006_buscar_saldo_analist_cn559(l_agen,l_cana,100)
            LET l4_s101 = NULL
            #101 IMPUESTO A LA RENTA
            LET l_codi = 101
            IF l4_s100 > 0 THEN
                LET l4_s101 = (l4_s100/l3_s100) * l3_s101
                LET l1_salt = l4_s101
            ELSE
                LET l1_salt = 0
            END IF

            EXECUTE p16_ins_tmp_datos_cana USING l_codi,l_agen,l_cana,l1_salt
            #102	RESULTADO NETO DEL EJERCICIO
            LET l_codi = 102
            LET l4_s102 = l4_s100 + l1_salt
            LET l1_salt = l4_s102
            EXECUTE p16_ins_tmp_datos_cana USING l_codi,l_agen,l_cana,l1_salt
        END FOREACH
    END FOREACH


    {LET l_valo_upd = '1'        LET l_nofi_upd = 12
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    LET l_valo_upd = '63'       LET l_nofi_upd = 25
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    LET l_valo_upd = '65'       LET l_nofi_upd = 58
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    LET l_valo_upd = '60'       LET l_nofi_upd = 57
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    LET l_valo_upd = '62'       LET l_nofi_upd = 37
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    LET l_valo_upd = '70'       LET l_nofi_upd = 66
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    LET l_valo_upd = '72'       LET l_nofi_upd = 69
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    LET l_valo_upd = '71'       LET l_nofi_upd = 64
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    LET l_valo_upd = '61'       LET l_nofi_upd = 52
    EXECUTE p102_upd_tmp_sedes USING l_valo_upd, l_nofi_upd
    }

    #@008 fin
END FUNCTION
#@006 fin

#@007 ini
FUNCTION f018_buscar_gastos_ic_op_cn559(p_impt,p_fech,p_tcof)
DEFINE  p_impt  DECIMAL(14,2),
        p_fech  DATE,
        p_tcof  FLOAT,
        l_impc  DECIMAL(14,2), #--GASTO POR INTERES DE CARTERA
        l_impo  DECIMAL(14,2), #--GASTO POR INTERES OTROS PAGOS
        l_fini  DATE,
        l_nmes  SMALLINT,
        l_impt  DECIMAL(14,2),
        l_imp1  DECIMAL(14,2), #--Tesoreria
        l_imp2  DECIMAL(14,2), #--CuentasxCobrar
        l_impp  DECIMAL(14,2),
        l_tpaa  FLOAT, #--Tasa ponderada adeudo anual
        l_tpam  FLOAT, #--Tasa ponderada adeudo mensual
        l_adeu  DECIMAL(14,2), #--Total adeudo
        l_adet  FLOAT, #--Adeudo x tasa
        l_paop  FLOAT, #--Porcentaje AOP
        l_iaop  FLOAT, #--Adeudo otros pagos
        l_iaca  FLOAT  #--Adeudo de cartera

    LET l_impc = NULL
    LET l_impo = NULL
    LET l_fini = NULL
    LET l_nmes = NULL

    #--Requerimiento de fondos x trimeste--#
    EXECUTE p109_sel_pcprm_valo USING "555","7" INTO l_nmes
    EXECUTE p110_sel_fini_trim USING p_fech,l_nmes INTO l_fini
    LET l_fini = l_fini + 1
    LET l_impt = NULL
    EXECUTE p111_sel_samad_liq USING p_tcof,l_fini,p_fech INTO l_impt

    #--Pago de planilla el mes--#
    #LET l_fini = p_fech - DAY(p_fech) + 1
    LET l_imp1 = NULL
    LET l_imp2 = NULL
    LET l_impp = 0
    EXECUTE p112_sel_tstdc_impo USING p_tcof,l_fini,p_fech INTO l_imp1
    EXECUTE p113_sel_cpmcp_impt USING p_tcof,l_fini,p_fech INTO l_imp2
    LET l_impp = l_imp1 + l_imp2

    #--Adeudos--#
    LET l_adeu = NULL
    LET l_adet = NULL
    LET l_tpaa = NULL
    LET l_tpam = NULL
    LET l_paop = NULL
    EXECUTE p107_sel_sald_adeu INTO l_adeu
    EXECUTE p108_sel_tea_sald_adeu INTO l_adet

    LET l_tpaa = l_adet / l_adeu
    LET l_tpaa = l_tpaa / 100

    LET l_tpam = potencia((1+l_tpaa),(1/12)) - 1

    LET l_paop = l_impp / l_impt #--Total de planilla/Total adeudo trimestral

    LET l_iaop = NULL
    LET l_iaca = NULL
    LET l_iaop = l_adeu * l_paop
    LET l_iaca = l_adeu - l_iaop

    --#@020 ini condicional en formula
    IF l_impt <= l_impp THEN
        LET l_impc = l_impt * l_tpam #--GASTO POR INTERES DE CARTERA
    ELSE
        LET l_impc = l_iaca * l_tpam #--GASTO POR INTERES DE CARTERA
    END IF
    --#@020 fin condicional en formula
    # LET l_impc = l_iaca * l_tpam #--GASTO POR INTERES DE CARTERA --#@020 se comenta
    LET l_impo = p_impt - l_impc #--GASTO POR INTERES OTROS PAGOS

RETURN l_impc,l_impo
END FUNCTION
#@007 fin

#@007 ini
FUNCTION f019_ajustar_tmp_datos_cn559()
DEFINE  l_tcof  FLOAT,
        l_gasi  DECIMAL(14,2),
        l_imp1  DECIMAL(14,2),
        l_imp2  DECIMAL(14,2),
        l_sals  DECIMAL(14,2),
        l_sala  DECIMAL(14,2),
        l_impt  DECIMAL(14,2),
        l_agen  SMALLINT,
        l_cade  CHAR(6),
        l_cad1  CHAR(6),
        l_fant  DATE,
        l_codi  SMALLINT,
        l_drub  CHAR(100),
        l_nume  SMALLINT,
        l_nmed  DECIMAL(14,2), #--Meses de antiguedad
        l_area  SMALLINT, #--1:Operaciones y gestion
        l_item  SMALLINT, #Clasificacion de antiguedad
        l_subt  SMALLINT,
        l_porc  FLOAT,
        l_tot1  DECIMAL(14,2),
        l_tott  DECIMAL(14,2),
        l_drux  CHAR(100),
        l_cod1  SMALLINT,
        l_tota  DECIMAL(14,2)

        #LET l_fant = p1.fech - DAY(p1.fech) + 1 #@013
        LET l_fant = p1.fech - DAY(p1.fech) #@013
        LET l_cade = MONTH(p1.fech) USING "&&",YEAR(p1.fech)
        LET l_cad1 = MONTH(l_fant) USING "&&",YEAR(l_fant)

        LET l_sals = NULL
        EXECUTE p119_sel_sals_sin_vehi USING p1.fech INTO l_sals
        EXECUTE p130_del_tmp_subt

        IF p1.tipo = 1 THEN

        ELSE #---Mensual
            LET l_tcof = NULL
            EXECUTE p106_sel_tcof USING p1.fech INTO l_tcof
            EXECUTE p104_del_tmp_adeu
            EXECUTE p105_ins_tmp_adeu USING l_tcof,p1.fech,p1.fech,l_tcof

            #--Actualizando datos de gastos por interes sede--#
            LET l_gasi = NULL
            LET l_codi = 13
            EXECUTE p114_sel_tmp_datos_salt_codi USING l_codi INTO l_gasi
            #--Buscar distribución detalle de adeudos--#
            LET l_codi = 18
            LET l_tott = NULL
            EXECUTE p114_sel_tmp_datos_salt_codi USING l_codi INTO l_tott
            FOREACH q129_sel_detalle_codi USING l_codi INTO l_subt
                LET l_porc = NULL
                LET l_tot1 = NULL
                EXECUTE p114_sel_tmp_datos_salt_codi USING l_subt INTO l_tot1
                LET l_porc = l_tot1 / l_tott
                EXECUTE p133_ins_tmp_subt USING l_codi,l_subt,l_porc
            END FOREACH

            LET l_codi = 14
            LET l_drub = "GASTOS POR INTERESES AGENCIAS"
            #--Buscar distribución actual--#
            LET l_tott = NULL
            EXECUTE p114_sel_tmp_datos_salt_codi USING l_codi INTO l_tott
            FOREACH q129_sel_detalle_codi USING l_codi INTO l_subt
                LET l_porc = NULL
                LET l_tot1 = NULL
                EXECUTE p114_sel_tmp_datos_salt_codi USING l_subt INTO l_tot1
                LET l_porc = l_tot1 / l_tott
                EXECUTE p133_ins_tmp_subt USING l_codi,l_subt,l_porc
            END FOREACH
            EXECUTE p116_del_tmp_datos_salt_codi USING l_codi

            LET l_imp1 = NULL
            LET l_imp2 = NULL

            CALL f018_buscar_gastos_ic_op_cn559(l_gasi,p1.fech,l_tcof) RETURNING l_imp1,l_imp2
            #--Detalle de adeudo--#
            LET l_subt = NULL
            LET l_porc = NULL
            FOREACH q134_sel_subt_porc USING l_codi INTO l_subt,l_porc
                LET l_tot1 = NULL
                LET l_tot1 = l_imp1 * l_porc
                LET l_drux = NULL
                EXECUTE p136_sel_drub USING l_subt INTO l_drux
                EXECUTE p135_upd_tmp_subt USING l_tot1,l_drux,l_codi,l_subt
                EXECUTE p116_del_tmp_datos_salt_codi USING l_subt
            END FOREACH

            #--Sub Detalle de adeudo--#
            LET l_tott = NULL
            LET l_subt = NULL
            LET l_porc = NULL
            LET l_cod1 = 18
            EXECUTE p137_sel_impt_codi USING l_codi,l_cod1 INTO l_tott
            FOREACH q134_sel_subt_porc USING l_cod1 INTO l_subt,l_porc
                LET l_tot1 = NULL
                LET l_tot1 = l_tott * l_porc
                LET l_drux = NULL
                EXECUTE p136_sel_drub USING l_subt INTO l_drux
                EXECUTE p135_upd_tmp_subt USING l_tot1,l_drux,l_cod1,l_subt
                EXECUTE p116_del_tmp_datos_salt_codi USING l_subt
            END FOREACH

            LET l_agen = NULL
            FOREACH q10_sel_gbofi USING p1.agei,p1.agef INTO l_agen #@008
                LET l_sala = NULL
                EXECUTE p120_sel_sals_agen_sin_vehi USING p1.fech,l_agen INTO l_sala
                IF l_sala IS NULL THEN
                    LET l_sala = 0
                END IF
                LET l_impt = NULL
                LET l_porc = NULL
                LET l_porc = l_sala/l_sals
                LET l_impt = l_porc * l_imp1

                EXECUTE p80_ins_tmp_datos USING l_agen,l_codi,l_drub,l_impt,"0",l_impt

                LET l_subt = NULL
                LET l_tott = NULL
                FOREACH q135_sel_subt_impt INTO l_subt,l_drux,l_tott
                    LET l_porc = NULL
                    LET l_porc = l_sala / l_sals
                    LET l_impt = l_porc * l_tott
                    EXECUTE p80_ins_tmp_datos USING l_agen,l_subt,l_drux,l_impt,"0",l_impt
                END FOREACH

            END FOREACH
        END IF

        CALL f007_quitar_admin_cn559(p1.fech,l_cade)
        CALL f007_quitar_admin_cn559(l_fant,l_cad1)

        #@015 ini
        EXECUTE p139_del_tmp_pctao
        EXECUTE p140_ins_tmp_pctao USING p1.fech
        #@015 fin

        #@008 ini
        EXECUTE p139_del_tmp_rseo
        EXECUTE p140_ins_tmp_reso USING p1.fech #,p1.fech
        #@008 fin

        IF p1.det2 = 'N' THEN
           #EXECUTE p71_del_tmp_cobr
           #EXECUTE p72_ins_tmp_cobr USING p1.fech
           EXECUTE p62_del_rseg_eaccc USING p1.fech,"6666"
           EXECUTE p62_del_rseg_eaccc USING p1.fech,"9999"
           EXECUTE p62_del_rseg_eaccc USING l_fant,"6666"
           EXECUTE p62_del_rseg_eaccc USING l_fant,"9999"
        END IF

        #---Buscando analistas
        EXECUTE p14_del_tmp_rseg
        EXECUTE p15_ins_tmp_rseg using p1.fech
        #EXECUTE p15_ins_tmp_rseg using l_fant  #@013
        EXECUTE p15_ins_tmp_rseg using l_fant #@017

        #-Antiguedad de sede en meses--#
        LET l_agen = NULL
        LET l_nmed = NULL
        EXECUTE p123_del_tmp_area
        FOREACH q121_sel_anti_sede USING p1.fech INTO l_agen,l_nmed
            LET l_item = NULL
            LET l_nume = NULL
            EXECUTE p13_sel_cont_tmp_eaccc_sin_cob USING p1.fech,l_agen INTO l_nume #@010

            LET l_area = 1
            EXECUTE p122_buscar_item_sede USING l_area,l_nmed,l_nmed,l_nmed,l_nmed,l_nmed
            INTO l_item
            EXECUTE p124_ins_tmp_area USING l_area,l_agen,l_nume,l_item

            LET l_area = 4
            EXECUTE p122_buscar_item_sede USING l_area,l_nmed,l_nmed,l_nmed,l_nmed,l_nmed
            INTO l_item
            EXECUTE p124_ins_tmp_area USING l_area,l_agen,l_nume,l_item

        END FOREACH

        LET l_codi = 29
        LET l_drub = "GASTOS POR INTERESES CENTRAL"
        EXECUTE p116_del_tmp_datos_salt_codi USING l_codi
        CALL f020_llenar_factor_area_cn559(1,l_imp2,l_codi,l_drub)

        LET l_tota = NULL
        EXECUTE p114_sel_tmp_datos_salt_codi USING l_codi INTO l_tota
        LET l_tota = l_imp2 - l_tota
        IF l_tota != 0 THEN
            EXECUTE p80_ins_tmp_datos USING "1",l_codi,l_drub,l_tota,"0",l_tota
        END IF

        LET l_codi = 13
        LET l_drub = "GASTOS POR INTERESES TOTAL"
        EXECUTE p116_del_tmp_datos_salt_codi USING l_codi

        LET l_agen = NULL
        FOREACH q10_sel_gbofi USING p1.agei,p1.agef INTO l_agen #@008
            LET l_codi = 14
            LET l_imp1 = NULL
            LET l_tott = NULL
            EXECUTE p81_sel_tmp_datos_salt USING l_codi,l_agen INTO l_imp1
            LET l_cod1 = 29
            LET l_imp2 = NULL
            EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_imp2
            LET l_tott = l_imp1 + l_imp2
            IF l_tott!=0 THEN #@010
                EXECUTE p80_ins_tmp_datos USING l_agen,"13",l_drub,l_tott,"0",l_tott
            END IF #@010
        END FOREACH

END FUNCTION
#@007 fin

#@007 ini
FUNCTION f020_llenar_factor_area_cn559(p_tipo,p_mont,p_codi,p_drub)
DEFINE  p_tipo  SMALLINT,
        p_mont  DECIMAL(14,2),
        p_codi  SMALLINT,
        p_drub  CHAR(100),
        l_int1  SMALLINT,
        l_porc  DECIMAL(14,2),
        l_nume  SMALLINT,
        l_tota  SMALLINT,
        l_mont  DECIMAL(14,2),
        l_proa  FLOAT,
        l_fact  FLOAT,
        l_totm  FLOAT,
        l_impt  DECIMAL(14,2),
        l_agen  SMALLINT,
        l_numa  SMALLINT

        LET l_porc = NULL
        LET l_tota = 0
        EXECUTE p127_sel_nume_tota USING p_tipo INTO l_tota
        LET l_mont = p_mont

        FOREACH q125_sel_item_area USING p_tipo INTO l_int1,l_porc
            LET l_nume = NULL
            LET l_proa = NULL
            LET l_fact = NULL
            LET l_porc = l_porc/100
            EXECUTE p126_sel_nume_item USING p_tipo,l_int1 INTO l_nume
            LET l_proa = l_mont / l_tota
            LET l_fact = l_proa * (1 + l_porc)
            LET l_totm = l_nume * l_fact

            FOREACH q128_sel_agen_nume USING p_tipo,l_int1 INTO l_agen,l_numa
                LET l_impt = NULL
                LET l_impt = l_fact * l_numa
                EXECUTE p80_ins_tmp_datos USING l_agen,p_codi,p_drub,l_impt,"0",l_impt
            END FOREACH

            LET l_mont = l_mont - l_totm
            LET l_tota = l_tota - l_nume

        END FOREACH



END FUNCTION
#@007 fin

#@007 ini
FUNCTION f021_actualizar_calculo_cn559()
DEFINE  l_tot1  DECIMAL(14,2),
        l_tot2  DECIMAL(14,2),
        l_tot3  DECIMAL(14,2),
        l_tott  DECIMAL(14,2),
        l_agen  SMALLINT,
        l_cod1  SMALLINT,
        l_cod2  SMALLINT,
        l_cod3  SMALLINT,
        l_drub  CHAR(100)

    LET l_agen = NULL

    EXECUTE p116_del_tmp_datos_salt_codi USING "31" #--MARGEN FINANCIERO BRUTO FINAL
    EXECUTE p116_del_tmp_datos_salt_codi USING "36" #--MARGEN FINANCIERO NETO FINAL
    EXECUTE p116_del_tmp_datos_salt_codi USING "51" #--MARGEN FINANCIERO NETO CENTRAL DE INGRESOS Y GASTOS POR SERVICIOS FINANCIEROS
    EXECUTE p116_del_tmp_datos_salt_codi USING "65" #--MARGEN OPERACIONAL
    EXECUTE p116_del_tmp_datos_salt_codi USING "81" #--MARGEN OPERACIONAL NETO FINAL
    EXECUTE p116_del_tmp_datos_salt_codi USING "95" #--RESULTADO DE OPERACION FINAL
    EXECUTE p116_del_tmp_datos_salt_codi USING "100" #--RESULTADOS DEL EJERCICIO ANTES DEL IMPUESTO A LA RENTA
    FOREACH q138_sel_temp_datos_salt INTO l_agen
        LET l_tot1 = NULL
        LET l_tot2 = NULL
        LET l_tot3 = NULL
        LET l_tott = NULL

        LET l_cod1 = 1
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot1

        LET l_cod2 = 13
        EXECUTE p81_sel_tmp_datos_salt USING l_cod2,l_agen INTO l_tot2

        LET l_cod1 = 32
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot3 #-PROVISION

        LET l_cod3 = 31
        LET l_drub = "MARGEN FINANCIERO BRUTO FINAL"
        LET l_tott = l_tot1 - l_tot2
        EXECUTE p80_ins_tmp_datos USING l_agen,l_cod3,l_drub,l_tott,"0",l_tott

        LET l_cod3 = 36
        LET l_drub = "MARGEN FINANCIERO NETO FINAL"
        LET l_tott = l_tot1 - l_tot2 - l_tot3
        EXECUTE p80_ins_tmp_datos USING l_agen,l_cod3,l_drub,l_tott,"0",l_tott

        LET l_cod1 = 37 #--INGRESOS POR SERVICIOS FINANCIEROS TOTAL
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot1
        LET l_cod1 = 43 #--GASTOS POR SERVICIOS FINANCIEROS TOTAL
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot2

        LET l_cod3 = 51
        LET l_drub = "MARGEN FINANCIERO NETO CENTRAL DE INGRESOS Y GASTOS POR SERVICIOS FINANCIEROS"
        LET l_tott = l_tott + l_tot1 - l_tot2
        EXECUTE p80_ins_tmp_datos USING l_agen,l_cod3,l_drub,l_tott,"0",l_tott

        LET l_cod1 = 52 #--RESULTADOS POR OPERACIONES FINANCIERAS (ROF) TOTAL
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot1

        LET l_cod3 = 65
        LET l_drub = "MARGEN OPERACIONAL"
        LET l_tott = l_tott + l_tot1
        EXECUTE p80_ins_tmp_datos USING l_agen,l_cod3,l_drub,l_tott,"0",l_tott

        LET l_cod1 = 66 #-GASTOS DE ADMINISTRACIÓN TOTAL
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot1
        LET l_tott = l_tott - l_tot1

        LET l_cod1 = 76 #--DEPRECIACIÓN Y AMORTIZACIÓN TOTAL
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot1

        LET l_cod3 = 81
        LET l_drub = "MARGEN OPERACIONAL NETO FINAL"
        LET l_tott = l_tott - l_tot1
        EXECUTE p80_ins_tmp_datos USING l_agen,l_cod3,l_drub,l_tott,"0",l_tott

        LET l_cod1 = 82 #--VALUACIÓN DE ACTIVOS Y PROVISIONES TOTAL
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot1

        LET l_cod3 = 95
        LET l_drub = "RESULTADO DE OPERACION FINAL"
        LET l_tott = l_tott - l_tot1
        EXECUTE p80_ins_tmp_datos USING l_agen,l_cod3,l_drub,l_tott,"0",l_tott

        LET l_cod1 = 96 #--OTROS INGRESOS Y GASTOS TOTAL
        EXECUTE p81_sel_tmp_datos_salt USING l_cod1,l_agen INTO l_tot1

        LET l_cod3 = 100
        LET l_drub = "RESULTADOS DEL EJERCICIO ANTES DEL IMPUESTO A LA RENTA"
        LET l_tott = l_tott + l_tot1
        EXECUTE p80_ins_tmp_datos USING l_agen,l_cod3,l_drub,l_tott,"0",l_tott

    END FOREACH

END FUNCTION
#@007 fin

#@008 ini
FUNCTION f022_gastos_adm_cobr_cn559(p_tipo)
DEFINE  p_tipo  SMALLINT,
        l_nofi  SMALLINT,
        l_desc  CHAR(40),
        l_salt  DECIMAL(14,2),
        l_nume  SMALLINT,
        l_salx  DECIMAL(14,2),
        l_sala  DECIMAL(14,2),
        l_sals  DECIMAL(14,2),
        l_impt  DECIMAL(14,2),
        l_rseo  INTEGER

        LET l_nofi = NULL
        LET l_desc = NULL
        FOREACH q_oficina USING p1.agei,p1.agef INTO l_nofi, l_desc #@008
            LET l_salt = NULL
            LET l_nume = NULL

            EXECUTE p141_sals_ageo USING l_nofi INTO l_salt
            EXECUTE p143_sel_nume_rseg USING l_nofi INTO l_nume

            LET l_sala = 0
            LET l_sala = buscar_saldo_a(p_tipo,l_nofi)
            LET l_salx = 0
            LET l_rseo = NULL
            FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_rseo
                #@009 ini
                IF l_rseo = -1 THEN
                    CONTINUE FOREACH
                END IF
                #@009 fin
                LET l_sals = NULL
                EXECUTE p142_sals_rseo USING l_nofi,l_rseo INTO l_sals
                IF l_sals > 0 AND l_salt>0 THEN
                    LET l_impt = (l_sals/l_salt) * l_sala
                    LET l_salx = l_salx + l_impt
                    EXECUTE p16_ins_tmp_datos_cana USING p_tipo,l_nofi,l_rseo,l_impt
                END IF
            END FOREACH

            LET l_rseo = NULL
            LET l_salx = l_sala - l_salx
            IF l_salx > 0 THEN
                FOREACH q14_cana_tmp_eaccc USING l_nofi INTO l_rseo
                    #@009 ini
                    IF l_rseo = -1 THEN
                        CONTINUE FOREACH
                    END IF
                    #EXECUTE p142_sals_rseo USING l_nofi,l_rseo INTO l_sals
                    #@009 fin
                    LET l_impt = l_salx / l_nume
                    EXECUTE p16_ins_tmp_datos_cana USING p_tipo,l_nofi,l_rseo,l_impt
                END FOREACH
            END IF
        END FOREACH

END FUNCTION
#@008 fin

#@008 ini
FUNCTION f023_buscar_ing_financ_cn559(p_agen)
DEFINE  p_agen   SMALLINT,
        l_impt  DECIMAL(14,2)

        LET l_impt = NULL
        EXECUTE p155_sel_tmp_ings_agen USING p_agen INTO l_impt
        IF l_impt IS NULL THEN
            LET l_impt = 0
        END IF

RETURN l_impt
END FUNCTION
#@008 fin

#@008 ini
FUNCTION f024_buscar_ing_financ_cana_cn559(p_agen,p_cana)
DEFINE  p_agen  SMALLINT,
        p_cana  INTEGER,
        l_impt  DECIMAL(14,2)

        LET l_impt = NULL
        EXECUTE p156_sel_tmp_ings_cana USING p_agen,p_cana INTO l_impt
        IF l_impt IS NULL THEN
            LET l_impt = 0
        END IF

RETURN l_impt
END FUNCTION
#@008 fin

#@008 ini
FUNCTION f025_buscar_gast_financ_cn559(p_agen)
DEFINE  p_agen   SMALLINT,
        l_impt  DECIMAL(14,2)

        LET l_impt = NULL
        EXECUTE p163_sel_tmp_gfin_agen USING p_agen INTO l_impt
        IF l_impt IS NULL THEN
            LET l_impt = 0
        END IF

RETURN l_impt
END FUNCTION
#@008 fin

#@008 ini
FUNCTION f026_buscar_gast_financ_cana_cn559(p_agen,p_cana)
DEFINE  p_agen  SMALLINT,
        p_cana  INTEGER,
        l_impt  DECIMAL(14,2)

        LET l_impt = NULL
        EXECUTE p164_sel_tmp_gfin_cana USING p_agen,p_cana INTO l_impt
        IF l_impt IS NULL THEN
            LET l_impt = 0
        END IF

RETURN l_impt
END FUNCTION
#@008 fin

#@008 ini
FUNCTION f027_buscar_num_analis_resu_neto_cn559(l_codi,l_agen)
DEFINE l_codi   SMALLINT,
       l_agen   SMALLINT,
       l_nume   SMALLINT

   LET l_nume = NULL
   EXECUTE p170_sel_salt_cana USING l_codi,l_agen INTO l_nume

   IF l_nume IS NULL THEN
      LET l_nume = 0
   END IF

RETURN l_nume
END FUNCTION
#@008 fin

#@008 ini
FUNCTION f028_buscar_saldo_positivo_cn559(l_codi, l_agen)
DEFINE l_codi   SMALLINT,
       l_agen   SMALLINT,
       l_sald   DECIMAL(14,2)

   LET l_sald = NULL

   EXECUTE p171_sel_tmp_datos_salt_posi USING l_codi,l_agen INTO l_sald
   IF l_sald IS NULL THEN
      LET l_sald = 0
   END IF

RETURN l_sald
END FUNCTION
#@008 fin

#@008 ini
FUNCTION f029_mostrar_dif_cod2_cn559()
DEFINE  l_spoo  CHAR(20),
        l_cmd   CHAR(20000),
        l1  RECORD
            agen    SMALLINT,
            cana    INTEGER,
            imp1    DECIMAL(14,2), #@012
            imp2    DECIMAL(14,2), #@012
            dvg1    DECIMAL(14,2),
            dvg2    DECIMAL(14,2),
            imp3    DECIMAL(14,2), #@012
            mo19    DECIMAL(14,2), #@013
            #ingm    DECIMAL(14,2),
            #dife    DECIMAL(14,2),
            ingf    DECIMAL(14,2),
            ajus    DECIMAL(14,2),
            salt    DECIMAL(14,2)
        END RECORD,
        l_cont  SMALLINT

    LET l_cont = 0
    EXECUTE p174_sel_cont_usrn_codi_2 USING g_user INTO l_cont #@009
    IF l_cont = 0 THEN
        RETURN
    END IF

    LET l_spoo = 'egp_codi2.r'
    LET g_sHtml = "<html><head><title>EGP POR AGENCIA</title></head>"
    LET g_sHtml = g_sHtml CLIPPED, "<body>"
    LET g_sHtml = g_sHtml CLIPPED, "<h3><font face='Arial' color = '#000066'>EDPYME ALTERNATIVA SA</font></h3>"
    LET g_sHtml = g_sHtml CLIPPED, "<h2>DETALLE DE INGRESOS DE INTERES POR SEDE AL ", p1.fech USING "DD/MM/YYYY","</h2>"
    LET g_sHtml = g_sHtml CLIPPED, "<table border = 1>"
    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' > ", l_spoo
    RUN l_cmd

    INITIALIZE l1.* TO NULL
    LET g_sHtml = "<tr>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>COD.SEDE</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>NOMB SEDE</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>COD.ANALISTA</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>NOMBRE ANALISTA</center></b></td>" #@012
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>IMPORTE AUTOMATICO (MODULO 18 Y 21)</center></b></td>" #@012
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>IMPORTE MANUAL (MODULO 18 Y 59)</center></b></td>" #@012
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>DEVENGADO MES</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>DEVENGADO MES ANTERIOR</center></b></td>"
    #LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>IMPORTE COBRO</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>POSTEO DE PAGOS</center></b></td>" #@013
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>IMPORTE(MODULO 19)</center></b></td>" #@012
    #LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>DIFERIDO</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>INGRESO FINAL</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td><b><center>AJUSTE IMPT</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td bgcolor=#BBFF33><b><center>INGRESO EGP</center></b></td>"
    LET g_sHtml = g_sHtml CLIPPED, "<td bgcolor=#FF5833><b><center>DIF</center></b></td>"

    LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ",  l_spoo
    RUN l_cmd

   FOREACH q173_sel_detalle_codi_2 INTO l1.*
   #@013 Inicio
   IF l1.imp1 = 0 and l1.imp2 = 0 and l1.dvg1 = 0 and l1.dvg2 = 0 and l1.imp3 = 0 and l1.cana <> 9999 and l1.cana <> 6666 THEN
   ELSE
   #@013 Fin
      #@014 Inicio
      IF p1.det2 = 'N' THEN
         IF l1.cana = 9999 OR l1.cana = 6666 OR l1.cana = 1408 THEN
         ELSE
            LET g_sHtml = "<tr>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.agen,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",f006_buscar_descripcion_pc2009(l1.agen) CLIPPED,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.cana,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",f004_buscar_analista_cn559(l1.cana) CLIPPED,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.imp1,"</td>" #@012
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.imp2,"</td>" #@012
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.dvg1,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.dvg2,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.imp3,"</td>" #@012
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.mo19,"</td>" #@013
            #LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.ingm,"</td>"
            #LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.dife,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.ingf,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.ajus,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.salt,"</td>"
            LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.salt - (l1.ingf + l1.ajus) ,"</td>"
            LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ",  l_spoo
            RUN l_cmd
         END IF
      ELSE
      #@014 Fin
         LET g_sHtml = "<tr>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.agen,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",f006_buscar_descripcion_pc2009(l1.agen) CLIPPED,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.cana,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",f004_buscar_analista_cn559(l1.cana) CLIPPED,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.imp1,"</td>" #@012
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.imp2,"</td>" #@012
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.dvg1,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.dvg2,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.imp3,"</td>" #@012
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.mo19,"</td>" #@013
         #LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.ingm,"</td>"
         #LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.dife,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.ingf,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.ajus,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.salt,"</td>"
         LET g_sHtml = g_sHtml CLIPPED, "<td>",l1.salt - (l1.ingf + l1.ajus) ,"</td>"
         LET l_cmd  = "echo '", g_sHtml CLIPPED, "' >> ",  l_spoo
         RUN l_cmd
      END IF #@014 Fin

    END IF #@013
   END FOREACH

   CALL f0100_imprimir_gb001(l_spoo)

END FUNCTION
#@008 fin

#@009 ini
FUNCTION f030_calcular_tasa_ponderada_cn559()
DEFINE  l1  RECORD
            agen    SMALLINT,
            cana    INTEGER,
            tea0    FLOAT
        END RECORD,
        l_mont  FLOAT,
        l_sals  DECIMAL(14,2),
        l_tipo  SMALLINT,
        l_tip2  SMALLINT,
        l_subt  SMALLINT,
        l_cont  SMALLINT,
        l_salx  FLOAT #@011

        EXECUTE p178_del_tmp_tasp
        INITIALIZE l1.* TO NULL
        LET l_tipo = 1
        LET l_tip2 = 2

        IF p1.det2 = 'S' THEN #---Incluir cobranzas
            LET l_subt = 2
        ELSE
            LET l_subt = 1
        END IF

        FOREACH q180_sel_temp_eacc_agen USING p1.fech INTO l1.agen
            FOREACH q181_sel_temp_eacc_cana USING p1.fech,l1.agen INTO l1.cana
                LET l_mont = 0
                LET l_sals = 0
                EXECUTE p182_sel_tea0_sals USING p1.fech,l1.agen,l1.cana INTO l_mont
                EXECUTE p20_sel_sals_cana USING p1.fech,l1.agen,l1.cana INTO l_sals
                LET l1.tea0 = l_mont/l_sals
                EXECUTE p179_ins_tmp_tasp using l_tipo,l1.agen,l1.cana,l1.tea0

                LET l_mont = 0
                LET l_salx = NULL #@011
                #@011 ini
                IF p1.vouc = 'N' THEN
                    EXECUTE p14_sel_ings_12_sc USING l_subt,l1.agen,l1.cana INTO l_mont,l_cont
                ELSE
                    EXECUTE p14_sel_ings_12 USING l_subt,l1.agen,l1.cana INTO l_mont,l_cont
                END IF
                #@011 fin
                LET l_mont = (l_mont / l_cont) * 12

                EXECUTE p16_sel_sals_anual_cana USING l1.agen,l1.cana INTO l_salx #@011
                IF l_salx IS NULL THEN #@011
                    LET l_salx = 0 #@011
                ELSE
                    LET l_salx = l_salx/l_cont #@011
                    LET l1.tea0 = (l_mont/l_salx) * 100 #@011
                    EXECUTE p179_ins_tmp_tasp using l_tip2,l1.agen,l1.cana,l1.tea0
                END IF
            END FOREACH

            LET l_mont = 0
            LET l_sals = 0
            EXECUTE p183_sel_tea0_agen_sals USING p1.fech,l1.agen INTO l_mont
            EXECUTE p19_sel_sals_agen USING p1.fech,l1.agen INTO l_sals
            LET l1.tea0 = l_mont/l_sals
            LET l1.cana = 0
            EXECUTE p179_ins_tmp_tasp using l_tipo,l1.agen,l1.cana,l1.tea0

            LET l_mont = 0
            LET l_cont = 0
            LET l_salx = NULL #@011
            #@011 ini
            IF p1.vouc = 'N' THEN
                EXECUTE p14_sel_ings_12_sede_sc USING l1.agen INTO l_mont,l_cont
            ELSE
                EXECUTE p14_sel_ings_12_sede USING l1.agen INTO l_mont,l_cont
            END IF
            #@011 fin

            LET l_mont = (l_mont / l_cont) * 12

            EXECUTE p15_sel_sals_anual USING l1.agen INTO l_salx
            IF l_salx IS NULL THEN #@011
                LET l_salx = 0 #@011
            ELSE
                LET l_salx = l_salx/l_cont #@011
                LET l1.tea0 = (l_mont/l_salx) * 100 #@011
                EXECUTE p179_ins_tmp_tasp using l_tip2,l1.agen,l1.cana,l1.tea0
            END IF
        END FOREACH
END FUNCTION
#@009 fin

#@009 ini
FUNCTION f031_buscar_tpp_cn559(p_agen,p_cana,p_tipo)
DEFINE  p_agen  SMALLINT,
        p_cana  INTEGER,
        p_tipo  SMALLINT,
        l_tpp0  DECIMAL(14,2)

        LET l_tpp0 = NULL
        EXECUTE p184_sel_tpp USING p_tipo,p_agen,p_cana INTO l_tpp0

RETURN l_tpp0
END FUNCTION
#@009 fin

#@009 ini
FUNCTION f032_buscar_fechas_pc913(p_tipo,p_fech)
DEFINE  p_tipo  SMALLINT,
        p_fech  DATE,
        l_fec1  DATE,
        i       SMALLINT,
        l_nmes  SMALLINT,
        l_ani2  INTEGER,
        l_dia2  SMALLINT,
        l_fini  DATE, #--Fecha ini egp analista
        l_ffin  DATE  #--Fecha fin egp analista

        LET l_fini = NULL
        LET l_ffin = NULL

        IF p_tipo = 2 OR p_tipo = 1 THEN
            EXECUTE p109_sel_pcprm_valo USING "555","28" INTO l_fini
            EXECUTE p109_sel_pcprm_valo USING "555","29" INTO l_ffin
        END IF

        EXECUTE p13_ins_tmp_fecha USING p_tipo,p_fech
        LET l_fec1 = p_fech
        FOR i=1 TO 11
          LET l_nmes=MONTH(l_fec1)
          LET l_ani2=YEAR(l_fec1)

          IF l_nmes=1 THEN
            LET l_nmes=12
            LET l_ani2=l_ani2 - 1
          ELSE
            LET l_nmes=l_nmes - 1
          END IF

          CASE
            WHEN l_nmes=1 OR l_nmes=3 OR l_nmes=5 OR l_nmes=7 OR l_nmes=8 OR l_nmes=10 OR l_nmes=12
              LET l_dia2=31

            WHEN l_nmes=2
              IF f033_buscar_bisiesto_cn559(l_ani2) THEN
                LET l_dia2=29
              ELSE
                LET l_dia2=28
              END IF

            WHEN l_nmes=4 OR l_nmes=6 OR l_nmes=9 OR l_nmes=11
              LET l_dia2=30

          END CASE

          LET l_fec1=MDY(l_nmes,l_dia2,l_ani2)
          IF p_tipo = 2 or p_tipo = 1 THEN
            IF l_fec1 < l_fini AND (p_fech >= l_fini AND p_fech<=l_ffin) THEN
                CONTINUE FOR
            END IF
          END IF
          EXECUTE p13_ins_tmp_fecha USING p_tipo,l_fec1
      END FOR

END FUNCTION
#@009 fin

#@009 ini
FUNCTION f033_buscar_bisiesto_cn559(p_anio)
DEFINE	p_anio	INTEGER,
		l_flag	SMALLINT

	LET l_flag=FALSE
	IF ((p_anio MOD 4 == 0 AND p_anio MOD 100 != 0) OR p_anio MOD 400 == 0) THEN
		LET l_flag=TRUE
	END IF

RETURN l_flag
END FUNCTION
#@009 fin
